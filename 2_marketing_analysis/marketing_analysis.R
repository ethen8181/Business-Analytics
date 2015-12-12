# marketing analysis

library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(data.table)
setwd("/Users/ethen/Business-Analytics/2_marketing_analysis")

# customer purchase record for a retailer
data <- fread( "purchase.txt", sep = "\t", header = FALSE )

# ----------------------------------------------------------------------------
# 										Exploring around, not included  
# ----------------------------------------------------------------------------

# 1. name the column variables :
# there are three columns for this dataset
# namely, customer id, purchase amount and date of purchase ( year-month-day ) 
setnames( data, c( "customer_id", "purchase_amount", "purchase_date" ) )

# 2. convert date_of_purchase column to date type
data[ , purchase_date := ymd(purchase_date) ]

# 3. extract the year of purchase 
data[ , purchase_year := year(purchase_date) ]

# obtain the 
# 1. number of purchases per year
# 2. average purchase amount per year
# 3. total purchase amounts per year
# data.table allows you to compute all that in one concatenated command 

explore <- data[ , .( counter 	 = .N, 
					  avg_amount = mean(purchase_amount),
					  sum_amount = sum(purchase_amount) ), by = purchase_year ] %>%
		   arrange( purchase_year )

ggplot( explore, aes( purchase_year, sum_amount ) ) + 
geom_bar( stat = "identity", fill = "lightblue", color = "black" ) + 
scale_y_continuous( label = comma ) + 
ggtitle( "Total Purchase Amount 2005-2015" )
# as you can see from the bar plot there's a nice and positive trend in terms 
# of how much money is spent for this retailer 

# using sql-like command
# GROUP BY 1 :group by the first column regardless of what it's called
library(sqldf)
sqldf( "SELECT 	 purchase_year, 
			     COUNT(purchase_year) AS 'counter',
			     AVG(purchase_amount) AS 'avg_amount',
			     SUM(purchase_amount) AS 'sum_amount'
		FROM     data
		GROUP BY 1" )

# ----------------------------------------------------------------------------
# 										Segmentation 
# ----------------------------------------------------------------------------

# 1. name the column variables :
# there are three columns for this dataset
# namely, customer id, purchase amount and date of purchase ( year-month-day ) 
setnames( data, c( "customer_id", "purchase_amount", "purchase_date" ) )

# 2. convert date_of_purchase column to date type
# and extract the year 
data[ , purchase_date := ymd(purchase_date) ]
data[ , purchase_year := year(purchase_date) ]


# 3. calculate the recency value, we're going to
# compute the time difference between the purchase_date and 
# the very last day (plus 1 day) recorded in the dataset. After
# obtaining the day difference we'll convert it back to numeric
last <- max(data$purchase_date) + days(1)
data[ , recency := difftime( last, purchase_date ) %>% as.numeric() ]


summary(data)
# from the dataset, we can see that it consists of customer purchase
# from January 2005 to the last day of 2015. and the mean of the year_of_purchase
# tells that most purchases have been made during 2011.


# managerial segmentation 
# which customers should receive more attention 
# e.g. e-mails, phone calls 
# these decisions are based on who the customers are,
# that is how much they spent and how likely are they going to buy from us in the future


# recency is measured in days
# @recency : the purchase date closest to today
# @first_purchase : the first time the customer bought something 
rfm <- data[ , .( recency 	  	 = min(recency),
				  first_purchase = max(recency),
				  frequency  	 = .N, 
				  avg_amount 	 = mean(purchase_amount) ), by = customer_id ]


# managerial segmentation parameters 
inactive <- 3 * 365
cold <- 2 * 365
warm <- 365 
amount <- 100


Segmentation <- function( data, inactive, cold, warm, amount )
{
	# segmentation criteria:
	# four groups, grouped by recency 
	data[ , segment := NA ]
	data$segment[ with( data, recency > inactive ) ] <- "inactive"
	data$segment[ with( data, recency <= inactive & recency > cold ) ] <- "cold"
	data$segment[ with( data, recency <= cold & recency > warm ) ] <- "warm"
	data$segment[ with( data, recency <= warm ) ] <- "active"

 	# split the warm and cold segment further into new and old customers 
	# those that made their first purhase within a year, 
	# with respecitve to the cold and warm baseline.
	# and split it into low and high base on a specified average amount 
	data$segment[ with( data, segment == "warm" & 
							  first_purchase <= cold & 
							  avg_amount < amount ) ] <- "new warm low"

	data$segment[ with( data, segment == "warm" & 
							  first_purchase <= cold & 
							  avg_amount >= amount ) ] <- "new warm high"

	data$segment[ with( data, segment == "warm" & 
							  first_purchase > cold & 
							  avg_amount < amount ) ] <- "old warm low"

	data$segment[ with( data, segment == "warm" & 
							  first_purchase > cold & 
							  avg_amount >= amount ) ] <- "old warm high"

	data$segment[ with( data, segment == "active" & 
							  first_purchase <= warm & 
							  avg_amount < amount ) ] <- "new active low"

	data$segment[ with( data, segment == "active" & 
							  first_purchase <= warm & 
							  avg_amount >= amount ) ] <- "new active high"

	data$segment[ with( data, segment == "active" & 
							  first_purchase > warm & 
							  avg_amount < amount ) ] <- "old active low"

	data$segment[ with( data, segment == "active" & 
							  first_purchase > warm & 
							  avg_amount >= amount ) ] <- "old active high"

	# order by customer id 
	return( data[ order(customer_id), ] )
}

rfm1 <- Segmentation( data = rfm, inactive = inactive, 
					 cold = cold, warm = warm, amount = amount )
# segment size 
table(rfm1$segment)

# describe the segment profile's averages
test <- rfm1[ , lapply( .SD, mean ), by = segment, .SDcols = -c("customer_id") ][ order(segment) ]

test$segment <- factor( test$segment, 
						   levels = c( "inactive", "cold", 
							   		   "new warm low", "new warm high",
							   		   "old warm low", "old warm high",
							   		   "new active low", "new active high",
							   		   "old active low", "old active high" ) )

# non-existent customers,
# segment retrospectively

rfm2 <- data[ recency > 365, 
			  .( recency 	  	= min(recency) - 365,
				 first_purchase = max(recency) - 365,
				 frequency  	= .N, 
				 avg_amount 	= mean(purchase_amount) ), by = customer_id ]

rfm2 <- Segmentation( data = rfm2, inactive = inactive, 
					  cold = cold, warm = warm, amount = amount )

# revenue generation per segmentation 
revenue1 <- data[ purchase_year == 2015, .( revenue = sum(purchase_amount) ), by = customer_id ]

# for 2015 
# merge so that customer who have not generated revenue will also show up 
actual1 <- merge( rfm1, revenue1, by = "customer_id", all.x = TRUE )
actual1$revenue[ is.na(actual1$revenue) ] <- 0
actual1[ , .( avg_revenue = mean(revenue) ), by = segment ]

# compare the previous year and see how they did this year 
actual2 <- merge( rfm2, revenue1, by = "customer_id", all.x = TRUE )
actual2$revenue[ is.na(actual2$revenue) ] <- 0

# revenue that you would expect from each segment in the next year 
expected <- actual2[ , .( avg_revenue = mean(revenue) ), by = segment ] %>%
			arrange( desc(avg_revenue) )

ggplot( expected, aes( reorder( segment, -avg_revenue ), avg_revenue ) ) + 
geom_bar( stat = "identity" )


# 1. table(rfm1$segment)

# The number of "new active high" customers has increased between 2014 and 2015, rate of increase 
# 2. table(rfm1$segment)["new active high"] / table(rfm2$segment)["new active high"]

# Looking at segment description, what is the average purchase amount of a customer who belongs to the "new active high" segment



# ----------------------------------------------------------------------------
# 										Scoring Target  
# ----------------------------------------------------------------------------

# calculating the p-value by hand: coefficient / standard deviation of coefficient 
# http://stats.stackexchange.com/questions/9715/how-to-set-up-and-estimate-a-multinomial-logit-model-in-r
# https://www.researchgate.net/post/Significance_of_Regression_Coefficient
# predict probability and amount 

# predictors: information of the customers a year ago 
# target variable: probability that the customer will buy something
# and if they do, how much they will spend on it 


# to predict the probability, we use the entire dataset as our calibration model
# as for the amount, we use the customers that have had a purchase record 
















# Computing the RFM value for each customer 
rfm1 <- data[ , .( recency 	  = min(recency),
				   frequency  = .N, 
				   avg_amount = mean(purchase_amount) ), by = customer_id ]

# use a histogram 
grid.arrange( ggplot( rfm1, aes( avg_amount ) ) + geom_histogram(),
			  ggplot( rfm1, aes( frequency )  ) + geom_histogram(), ncol = 2 )

# log transformation 
ggplot( rfm1, aes( log(avg_amount) ) ) + 
geom_histogram()

# data transformation
# store the original one  
rfm2 <- copy(rfm1)

# 1. remove the customer id and store it elsewhere for now as they're meaningless in terms of segmentation
customer_id <- rfm2$customer_id
rfm2[ , customer_id := NULL ]

# 2. take the logarithm value to make it normally distributed.
rfm2[ , `:=`( avg_amount = log(avg_amount),
		   	  frequency  = log(frequency) ) ]

# 3. scale each column
rfm2[ , names(rfm2) := lapply( .SD, scale ) ]



# Customers with poor prospects in terms of loyalty and profitability, due to the few purchases they have made so far


# test 
source("/Users/ethen/machine-learning/clustering/clustering.R")

criteria <- CHCriterion( data = rfm2, kmax = 10, 
                         clustermethod = "kmeanspp", iter.max = 300 )

criteria_long <- gather( criteria, "index", "value", -1 )

ggplot( criteria_long, aes( k, value, color = index ) ) + 
geom_line() + geom_point( aes( shape = index ), size = 3 ) +
facet_wrap( ~ index, scale = "free_y" ) + 
guides( color = FALSE, shape = FALSE )

















