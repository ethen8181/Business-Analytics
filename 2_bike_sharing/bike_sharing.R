# kaggle bike sharing 
# https://www.kaggle.com/c/bike-sharing-demand

# objective : predict the total count of bikes rented during each hour covered by the test set

# dependent variables :
# registered : number of registered user
# casual : number of non-registered user 
# count : number of total rentals

library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(party)
library(scales)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(data.table)
setwd("/Users/ethen/Business-Analytics/2_bike_sharing/data")

data <- fread("train.csv")


# ----------------------------------------------------------------------------
# 						Exploratory Data Analysis 
# ----------------------------------------------------------------------------

# two main goals of exploratoy data analysis
# 1. knowing the distribution of our data more. Check for missing values, 
# 	 and outliers, so that we can know whether they'll be needing transformations
# 	 or included (excluded) for the modeling 
# 2. discover the relationships between target (dependent) variables or predictors
# 	 (independent), which can be used for feature selection

# --------------------------------------------------------
# a quick check on if there's any na value for each column
sapply( data, function(y) sum( is.na(y) ) )

# plot a histogram for numeric independent variables to visualize and understand the distribution 
independent <- c( "casual", "registered", "count" )
columns <- which( !names(data) %in% independent & sapply( data, is.numeric ) )

histogram <- lapply( names(columns), function(column)
{
	ggplot( data[ , column, with = FALSE ], aes_string( column ) ) + 
	geom_histogram()
})
do.call( grid.arrange, histogram )

# from the visualization 
# we can see that four variables including temp, atemp, humidity and windspeed are continuous variables
# and values are normally distributed
# the weather variable shows that most of the days are clear (1)
# the holiday variable consists mainly of 0 tells you most days are not considered as holidays
# and most value of 1 for workday tells you most days are neither weekends or holidays
# Season has four categories ranging from 1 to 4 that are equally distributed


# check the correlation of the continuous variables with the independent variables
cor( select( data, casual, registered, temp:windspeed ) ) %>%
findCorrelation( cutoff = .85, name = TRUE )


# --------------------------------------------------------
# hypothesis test

# 1. hourly trend
# convert datetime column to date type
data[ , datetime := ymd_hms(datetime) ]

# extract the datetime; and casual, registered column 
subdata1 <- select( data, datetime, casual, registered ) %>%
		    tbl_dt() %>%
		    mutate( datetime = hour(datetime) ) %>%
		    gather( "user", "count", -1 )

# box plot for registered and unregisted users
ggplot( subdata1, aes( datetime, count, group = datetime, fill = user ) ) + 
geom_boxplot() + 
facet_wrap( ~ user ) +
scale_fill_discrete( limit = c( "registered", "casual" ) ) + 
guides( fill = FALSE ) + 
ggtitle( "2011-2012 User's Hourly Bike Demand" )

# to be specific 
# The "dots" at the end of the boxplot represent outliers. Which is determined by 
# checking if a data point is:
# less than Q1 - 1.5 * IQR or greater than Q3 + 1.5 * IQR, where IQR stands for interquartile range = Q3 - Q1
# And the line goes to the first data point before the the cut-off. 
# Notice the outliers, log transformation

# the registered users' behavior can be categorized into three groups


# 2. daily trend 
subdata2 <- select( data, datetime, casual, registered ) %>%
		    tbl_dt() %>%
		    mutate( datetime = lubridate::wday( datetime, label = TRUE ) ) %>%
		    gather( "user", "count", -1 ) %>%
		    group_by( datetime, user ) %>%
			summarise( count = sum(count) )

# registered and unregisted users
ggplot( subdata2, aes( datetime, count, group = user, color = user ) ) + 
geom_line( size = 1 ) + geom_point( size = 3 ) + 
scale_y_continuous( label = comma ) +
scale_colour_discrete( limit = c( "registered", "casual" ) ) + 
ggtitle( "2011-2012 User's Daily Bike Demand" )


# total bike demand 
demand <- select( data, datetime, casual, registered ) %>%
		  tbl_dt() %>%
		  mutate( datetime = year(datetime) ) %>%
	      gather( "user", "count", -1 ) %>%
		  group_by( datetime ) %>%
		  summarise( count = sum(count) )

# 3. trend of bike demand over year for casual and registered users
subdata3 <- select( data, datetime, casual, registered ) %>%
			tbl_dt() %>%
		    mutate( datetime = year(datetime) ) %>%
			gather( "user", "count", -1 ) %>% 
			group_by( datetime, user ) %>%
			summarise( count = sum(count) )

ggplot( subdata3, aes( datetime, count, fill = user ) ) + 
geom_bar( stat = "identity", position = "dodge", width = .6 ) + 
geom_text( aes( label = count ), position = position_dodge(.6), vjust = -.6, size = 3 ) +
scale_x_continuous( breaks = unique(subdata3$datetime) ) + 
scale_y_continuous( label = comma ) +
scale_fill_discrete( limit = c( "registered", "casual" ) ) +
ggtitle( "2011-2012 Bike Demand of Users" ) 


# increase of bike demand 


# from the exploratory analysis section we can tell that
# the behaviors of the registered and non-registered users are totally different
# thus for the following two section, feauture engineering and model training
# we'll treat registered users and one for non-registered users differently
# and add up the final result


# ----------------------------------------------------------------------------
# 						Feature Engineering
# ----------------------------------------------------------------------------
# data[ , datetime := ymd_hms(datetime) ]
# add a hour, weekday, year, month column 
data[ , c( "hour", "weekday", "year", "month" ) := list( hour(datetime), 
												   		 lubridate::wday(datetime),
												   		 year(datetime),
												   		 month(datetime) ) ]

# split into 80 / 20 percent and evaluate prediction accuracy on test set
set.seed(4321) 
train_index <- createDataPartition( data$count, p = .8, list = FALSE )
data_train  <- data[ train_index, ]
data_test   <- data[ -train_index, ]
rm(data)


# --------------------------------------------------------------
# distribution 
ggplot( data_train, aes( log( casual + 1 ) ) ) + 
geom_histogram()

# list of the final independent variables 
variables <- c( "weather", "temp", "humidity", "windspeed", 
				"type", "year", "month", "hour", "weekday" )

# putting it all together 
ExtractFeatures <- function( data, data.type )
{

	# 1. convert holiday and workingday into one variable
	#    indicating whether its a holiday, weekend and workday
	data$type <- with( data, 

		ifelse( holiday == 1, "holiday",
		ifelse( holiday == 0 & workingday == 0, "weekend", "workday" ) ) 
	)

	# 2. keep the needed variables, and convert discrete variables into factors
	# if it is the original training data, log transformation of dependent variables 
	if( data.type == "train" )
	{
		columns <- which( colnames(data) %in% c( "casual", "registered", variables ) )
		data <- select( data, columns )

		# log transformation
		data <- transform( data, casual     = log( casual + 1 ),
					   		 	 registered = log( registered + 1 ) )

	}else # "test"
	{
		columns <- which( colnames(data) %in% variables )
		data <- select( data, columns )	
	}

	# 3. convert to factor 
	factor_col <- which( colnames(data) %in% 
						 c( "weather", "hour", "year", "month", "weekday", "type" ) )	

	data <- modifyList( data, lapply( select( data, factor_col ), as.factor ) )
			 
	return(data)
}

# store the count for the test set 
count <- data_test$count
data_train <- ExtractFeatures( data = data_train, data.type = "train" )
data_test  <- ExtractFeatures( data = data_test , data.type = "train" )

# 10-fold cross validation 
control <- trainControl( method = "cv", number = 6 )

# evaluation measure 
RMSLE <- function( y, pred )
{
	sqrt( sum( ( log( y + 1) - log( pred + 1 ) )^2 ) / length(y) )
}

# formula 
formula_casual 	  <- as.formula( paste( "casual ~ ", paste( variables, collapse = " + " ) ) )
formula_registerd <- as.formula( paste( "registered ~ ", paste( variables, collapse = " + " ) ) )


model_ctree1 <- train( formula_casual, data 	 = data_train, 
									   trControl = control, 
									   method 	 = "ctree" )

model_ctree2 <- train( formula_registerd, data   	= data_train, 
									      trControl = control, 
									      method    = "ctree" )

result_ctree1 <- predict( model_ctree1, newdata = data_test )
result_ctree2 <- predict( model_ctree2, newdata = data_test )

prediction_ctree <- ( exp(result_ctree1) - 1 ) + ( exp(result_ctree2) - 1 )
RMSLE( count, prediction_ctree )

# gbm
library(gbm)
gbm_grid <- expand.grid( n.trees 	  	   = 1500,
						 shrinkage    	   = .01,
						 interaction.depth = 4,
						 n.minobsinnode    = 10 )

model_gbm1 <- train( formula_casual, data      = data_train, 
									 trControl = control, 
									 method    = "gbm",
									 tuneGrid  = gbm_grid, 
									 verbose   = FALSE )

model_gbm2 <- train( formula_registerd, data      = data_train, 
									 	trControl = control, 
										method    = "gbm",
										tuneGrid  = gbm_grid, 
										verbose   = FALSE )

result_gbm1 <- predict( model_gbm1, newdata = data_test )
result_gbm2 <- predict( model_gbm2, newdata = data_test )

prediction_gbm <- ( exp(result_gbm1) - 1 ) + ( exp(result_gbm2) - 1 )
RMSLE( count, prediction_gbm )

RMSLE( count, ( prediction_ctree + prediction_gbm ) / 2 )

# -------------------------------------------------------------------------
# predicting on the test set 

test1 <- fread("test.csv")
test1[ , datetime := ymd_hms(datetime) ]
test1[ , c( "hour", "weekday", "year", "month" ) := list( hour(datetime), 
												   		  lubridate::wday(datetime),
												   		  year(datetime),
												   		  month(datetime) ) ]
test2 <- ExtractFeatures( data = test1, data.type = "test" )

result1 <- predict( model_rf1, newdata = test2 )
result2 <- predict( model_rf2, newdata = test2 )

predict_count <- ( exp(result1) - 1 ) + ( exp(result2) - 1 )


result <- data.frame( datetime = test1$datetime, count = predict_count )
write.csv( result, "result.csv", row.names = FALSE )

# ----------------------------------------------------------------------------
# 						Model Training 
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# test code
library(doParallel)
library(randomForest)

# specify the number of cores to use to train the model 
registerDoParallel( cores = 2 )

# improvement, but random forest takes too long
model_rf1 <- train( formula_casual, data 	  = data_train, 
									trControl = control, 
									method 	  = "rf", ntree = 100 )

model_rf2 <- train( formula_registerd, data      = data_train, 
									   trControl = control, 
									   method    = "rf", ntree = 100 )

result_rf1 <- predict( model_rf1, newdata = data_test )
result_rf2 <- predict( model_rf2, newdata = data_test )

prediction_rf <- ( exp(result_rf1) - 1 ) + ( exp(result_rf2) - 1 )
RMSLE( count, prediction_rf )



# https://github.com/DfAC/StrategicBusinessAnalytics/blob/master/ProjectSlide.Rmd

# https://rpubs.com/chengjiun/52658
# https://github.com/chengjiun/kaggle_bikesharing/blob/master/code/bikeshare-demand.Rmd
# http://www.analyticsvidhya.com/blog/2015/06/solution-kaggle-competition-bike-sharing-demand/

