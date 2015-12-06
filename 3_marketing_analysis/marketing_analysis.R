# marketing analysis


library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(data.table)
setwd("/Users/ethen/Business-Analytics/3_marketing_analysis")

# customer purchase record for a retailer
data <- fread( "purchase.txt", sep = "\t", header = FALSE  )

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
data[ , purchase_date := ymd(purchase_date) ]

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


# Computing the RFM value for each customer 
rfm1 <- data[ , .( recency 	 = min(recency),
				   frequency  = .N, 
				   avg_amount = mean(purchase_amount) ), by = customer_id ]

# use a histogram 
grid.arrange( ggplot( rfm1, aes( avg_amount ) ) + geom_histogram(),
			  ggplot( rfm1, aes( frequency )  ) + geom_histogram(), ncol = 2 )

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







