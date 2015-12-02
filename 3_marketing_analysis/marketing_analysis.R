# marketing analysis


library(dplyr)
library(scales)
library(ggplot2)
library(lubridate)
library(data.table)
setwd("/Users/ethen/Business-Analytics/2_marketing_analysis")

# customer purchase record for a retailer
data <- fread( "purchase.txt", sep = "\t", header = FALSE  )

# ----------------------------------------------------------------------------
# 										Data Cleaning 
# ----------------------------------------------------------------------------

# 1. name the column variables :
# there are three columns for this dataset
# namely, customer id, purchase amount and date of purchase ( year-month-day ) 
setnames( data, c( "customer_id", "purchase_amount", "date_of_purchase" ) )

# 2. convert date_of_purchase column to date type
data[ , date_of_purchase := ymd(date_of_purchase) ]

# 3. extract the year of purchase 
data[ , year_of_purchase := year(date_of_purchase) ]

summary(data)
# from the dataset, we can see that this dataset consists of customer purchase
# from January 2005 to the last day of 2015. and the mean of the year_of_purchase
# tells that most purchases have been made during 2011.


# obtain the 
# 1. number of purchases per year
# 2. average purchase amount per year
# 3. total purchase amounts per year
# data.table allows you to compute all that in one concatenated command 

explore <- data[ , .( counter 	 = .N, 
					  avg_amount = mean(purchase_amount),
					  sum_amount = sum(purchase_amount) ), by = year_of_purchase ] %>%
		   arrange( year_of_purchase )

ggplot( explore, aes( year_of_purchase, sum_amount ) ) + 
geom_bar( stat = "identity", fill = "lightblue", color = "black" ) + 
scale_y_continuous( label = comma )
ggtitle( "Total Purchase Amount 2005-2015" )
# as you can see from the bar plot there's a nice and positive trend in terms 
# of how much money is spent for this retailer 

# using sql-like command
# GROUP BY 1 :group by the first column regardless of what it's called
library(sqldf)
sqldf( "SELECT 	 year_of_purchase, 
			     COUNT(year_of_purchase) AS 'counter',
			     AVG(purchase_amount) AS 'avg_amount',
			     SUM(purchase_amount) AS 'sum_amount'
		FROM     data
		GROUP BY 1" )





