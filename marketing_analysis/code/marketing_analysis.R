# marketing analysis

library(grid)
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


# recency is measured in days
# @recency : the purchase date closest to today
# @first_purchase : the first time the customer bought something 
rfm1 <- data[ , .( recency 	  	  = min(recency),
				   first_purchase = max(recency),
				   frequency  	  = .N, 
				   avg_amount 	  = mean(purchase_amount) ), by = customer_id ]

# use a histogram 
grid.arrange( ggplot( rfm1, aes( avg_amount ) ) + geom_histogram(),
			  ggplot( rfm1, aes( frequency )  ) + geom_histogram(), ncol = 2,
			  top = textGrob( "Variable Distribution" ) )


# managerial segmentation 
# which customers should receive more attention 
# e.g. e-mails, phone calls 
# these decisions are based on who the customers are,
# that is how much they spent and how likely are they going to buy from us in the future


# cutoff value of amount 
quantile( rfm1$avg_amount, probs = seq( 0, 1, 0.1 ) )

# managerial segmentation parameters 
inactive <- 3 * 365
cold 	 <- 2 * 365
warm 	 <- 365 
amount   <- 100

source("/Users/ethen/Business-Analytics/2_marketing_analysis/code/marketing_functions.R")
rfm1 <- Segmentation( data = rfm1, inactive = inactive, 
					  cold = cold, warm = warm, amount = amount )

# segment proportional distribution and 
# describe the segment profile's averages 
table(rfm1$segment) / sum( table(rfm1$segment) )
rfm1[ , lapply( .SD, mean ), by = segment, .SDcols = -c("customer_id") ]


# non-existent customers, retrospective segment
# go back in time for a year 
rfm2 <- data[ recency > 365, 
			  .( recency 	  	= min(recency) - 365,
				 first_purchase = max(recency) - 365,
				 frequency  	= .N, 
				 avg_amount 	= mean(purchase_amount) ), by = customer_id ]

rfm2 <- Segmentation( data = rfm2, inactive = inactive, 
					  cold = cold, warm = warm, amount = amount )

round( table(rfm2$segment) / sum( table(rfm2$segment) ), 2 )


# revenue per customer 
revenue_per_customer <- data[ purchase_year == 2015, 
				  			  .( revenue = sum(purchase_amount) ), by = customer_id ]

# compare the previous year and see how they did this year 
revenue_predicted <- merge( rfm2, revenue_per_customer, by = "customer_id", all.x = TRUE )
revenue_predicted[ is.na(revenue), revenue := 0 ]

# average revenue that you would expect from each segment's segment in the next year and 
# expected total revenue generated by each segment
revenue_summary <- revenue_predicted[ , .( avg_revenue 	 = mean(revenue),
								  		   total_revenue = sum(revenue) ), by = segment ] %>%
				   arrange( desc(total_revenue), desc( avg_revenue) )
revenue_summary


# -------------------------------------------------
# rfm python
# https://github.com/bowen0701/rfmtk/blob/master/rfmtk.py


