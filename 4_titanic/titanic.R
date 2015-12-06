# kaggle titanic  
# https://www.kaggle.com/c/titanic/data

library(mice)
library(caret)
library(dplyr)
library(rpart)
library(party)
library(stringr)
library(ggplot2)
library(data.table)
library(randomForest)
setwd("/Users/ethen/Business-Analytics/4_titanic")

data <- fread( "train.csv", stringsAsFactors = FALSE )
# @Parch : Number of Siblings/Spouses Aboard
# @Sibsp : Number of Parents/Children Aboard

# percentage of people that have survived 
prop.table( table( data$Survived ) )

# unboard passenger's gender and survived distribution
prop.table( with( data, table( Sex, Survived ) ), 1 )

# confirmed that 74 percent of the female survived 

# ------------------------------------------------------------------
# 						Exploratory Analysis 
# ------------------------------------------------------------------

# 1. missing value (numeric)
col_numeric <- which( sapply( data, is.numeric ) )

Missing <- function(x) sum( is.na(x) ) / length(x) * 100

missing_df <- apply( data %>% select(col_numeric), 2, Missing )
missing_df <- data.frame( variable   = names(missing_df),
						  percentage = missing_df ) %>%
			  arrange( desc(percentage) )

# visualize the percentage of missing values 
ggplot( missing_df, aes( reorder( variable, -percentage ), percentage ) ) + 
geom_bar( stat = "identity", fill = "skyblue3" ) + 
labs( x = "variables", title = "Variables with Missing Values" )

# count the percentage of columns that are missing for each observations
# remove the observations that have too large of a proportion 
table( apply( data %>% select(col_numeric), 1, Missing ) )

# Embark also has missing values ! 

# 2. Extract the title info from the Name information and replace the old one  

# split the name on both comma and period symbol, remove the white space
# at front
data$Name <- lapply( data$Name, function(x)
{
	string <- strsplit( x, split = "[,.]" )[[1]][2] %>% str_trim()
}) %>% unlist()

# group rare factor variables together with Other  
table(data$Name)

data$Name[ !data$Name %in% c( "Master", "Miss", "Mr", "Mrs" ) ] <- "Other"



# ------------------------------------------------------------------------
#							Feature Engineering 
# ------------------------------------------------------------------------
data <- fread( "train.csv", stringsAsFactors = FALSE, na.strings = "" )
data <- data %>% select( -PassengerId )

ExtractFeatures <- function(data)
{
	# 1 Add the number ob siblings and parent number and add 1 (himself)
	# to indicate the whole family size ; and remove the original two columns 
	data[ , FamilySize := SibSp + Parch + 1 ]
	data[ , c( "SibSp", "Parch" ) := NULL ]

	# split the name on both comma and period symbol, remove the white space
	# at front
	data$Name <- lapply( data$Name, function(x)
	{
		string <- strsplit( x, split = "[,.]" )[[1]][2] %>% str_trim()
	}) %>% unlist()

	data$Name[ !data$Name %in% c( "Master", "Miss", "Mr", "Mrs" ) ] <- "Other"

	# 2. 
	# ticket numbers are unique values for everyone, thus is excluded
	# and Cabin numbers are all missing too many values  
	variable <- c( "Ticket", "Cabin" )
	data <- data %>% 
			select( -one_of(variable) ) %>%
			mutate( Sex 	 = as.factor(Sex),
					Name     = as.factor(Name),
					Pclass   = as.factor(Pclass),
					Embarked = as.factor(Embarked),
					Survived = as.factor(Survived) )

	# 3. impute the missing age data with all the numeric variables 
	col_numeric <- which( sapply( data, is.numeric ) )

	data_temp <- mice( data %>% select(col_numeric), 
					   m = 1, maxit = 50, method = "pmm", seed = 500, print = FALSE )

	# replace the missing values with the first imputed dataset 
	# ( you can specify more than one with the m parameter in the mice function )
	data_complete <- mice::complete( data_temp, 1 )

	# combine the categorical data back 
	data <- cbind( data_complete, data %>% select( -col_numeric ) )

	return( data.table( data[ complete.cases(data), ] ) )
}

data <- ExtractFeatures(data)



# ------------------------------------------------------------------------
#							Model Training with Caret 
# ------------------------------------------------------------------------

# split into 80 / 20 percent and evaluate prediction accuracy on test set 
set.seed(4321)
train_index <- createDataPartition( data$Survived, p = .8, list = FALSE )
data_train  <- data[ train_index, ]
data_test   <- data[ -train_index, ]

# define training control to be 10-fold cross validation
# classProbs = TRUE 
control <- trainControl( method = "cv", number = 6 )

# initialize the method 
methods <- c( "rpart", "glm", "ctree" )
result_list   <- vector( mode = "list", length = length(methods) )
accuracy_list <- vector( mode = "list", length = length(methods) )
names(result_list)   <- methods
names(accuracy_list) <- methods

TrainModel <- function( method )
{
	for( method in methods )
	{
		model <- train( Survived ~. , 
						data      = data_train, 
					    trControl = control, 
					    method    = method )

		# convert result to numeric (from factor)
		result <- predict( model, newdata = data_test )	
		result_list[[method]] <- data.frame( as.numeric( levels(result) )[result] )
		setnames( result_list[[method]], method )

		# store the method and its accuracy 
		accuracy_list[[method]] <- data.table( 
			
			method   = method, 
			accuracy = confusionMatrix( data_test$Survived, result )$overall[["Accuracy"]] 
		)
	}

	result   <- do.call( cbind, result_list )
	accuracy <- rbindlist(accuracy_list)

	return( list( result = result, accuracy = accuracy  ) )
}

train_model <- TrainModel(methods)


# ensemble 
# start from here 

train_model$result[1,] %>% table()



dt <- data.table(item=c(1,1,1,1,2,2,2,2), category=c(2,3,2,2,2,3,1,1))

dt[, .SD[, .N, by = category][order(-N)][1], by = item]

# https://github.com/mattdelhey/kaggle-titanic
# http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r
# http://www.r-bloggers.com/titanic-challenge-on-kaggle-with-decision-trees-party-and-svms-kernlab/


# -------------------------
# hard-code way


# rpart : decision tree 
model_tree <- train( Survived ~. , 
					 data      = data_train, 
				     trControl = control, 
				     method    = "rpart" )

result_tree <- predict( model_tree, newdata = data_test )
confusionMatrix( data_test$Survived, result_tree )$overall[["Accuracy"]]


# glm
model_glm <- train( Survived ~. , 
					data      = data_train, 
				    trControl = control, 
				    method    = "glm" )

result_glm <- predict( model_glm, newdata = data_test )
confusionMatrix( data_test$Survived, result_glm )$overall[["Accuracy"]]


# randomForest 
model_rf <- train( Survived ~. , 
				   data      = data_train, 
				   trControl = control, 
				   method    = "rf", ntree = 100, importance = TRUE )

result_rf <- predict( model_rf, newdata = data_test )
confusionMatrix( data_test$Survived, result_rf )$overall[["Accuracy"]]



# party : conditional forest 
model_cf <- train( Survived ~. , 
				   data      = data_train, 
				   trControl = control, 
				   method    = "ctree" )
# interpretation 
# plot(model_cf$finalModel)
# index <- which( data_test$Name == "Mr" & data_test$Pclass == 3 )
# cbind( data_test[ index, ], predict = result_cf[index] )

result_cf <- predict( model_cf, newdata = data_test )
confusionMatrix( data_test$Survived, result_cf )$overall[["Accuracy"]]




test <- fread( "test.csv", stringsAsFactors = FALSE )



# ------------------------------------------------------------------------
#							Model Training 
# ------------------------------------------------------------------------

library(rpart)
library(rattle)
library(rpart.plot)

model_tree <- rpart( Survived ~ ., data = data_train, method = "class" )
# specify control = rpart.control to tune parameters 

# visualize the decision tree, which
# shows the prediction, and the probability that it belongs to the class
# the last percentage shows the percentage the observations that ended up
# in this slot   
fancyRpartPlot(model_tree)

result_tree <- predict( model_tree, newdata = data_test, type = "class" )
confusionMatrix( data_test$Survived, result_tree )



# update the model, and delete the insignificant ones manually 
model_glm <- glm( Survived ~., data = data_train )
summary(model_glm)
model_glm <- update( model_glm, .~. -Embarked )
model_glm <- update( model_glm, .~. -Fare )

result_glm <- predict( model_glm, newdata = data_test )
confusionMatrix( data_test$Survived, as.numeric( result_glm > .55 ) )

# explains about 40 percent 
with( summary(model_glm), 1 - deviance / null.deviance )




