# marketing analysis


# -------------------------------------------------------------------------------------------
# [Segmentation]: 
# Takes the rfm data and creates manual segmentation 
# based on the recency criteria first that splits customer's into inactive, cold, 
# warm and active based on the three recency cutoff value. Then further splits 
# the warm and active segment into four sub-groups each based on the amount cutoff value and 
# whether they have made their first purchase within the same timespan with respect to the "baseline". 
# classifying each of the two original segments further into new high, new low, old high and old low.

# @data 	: rfm segmentation data, must have five columns exactly named as  
# 			  customer_id, recency, first_purchase, frequency and avg_amount, 
# 			  the order of the column doesn't matter. 
# @inactive : customers whose recency are higher than this value will be classified as 
# 			  "inactive" customers. Note that the unit of recency is "days".
# @cold		: same functionality as above, except it's the cutoff value for "cold" customers.
# @warm 	: same functionality as above, cutoff value for "warm" and "active" customers.
# @amount 	: cutoff value for splitting the warm and active customers further into
# 			  high and low average purchased amount.
# returns 	: original rfm data and adds a new segment column to it. It also sorts the order
# 			  of the rows by customer id.  

Segmentation <- function( data, inactive, cold, warm, amount )
{
	# segmentation criteria:
	# four main groups, grouped by recency 
	data[ recency > inactive, segment := "inactive" ]
	data[ recency <= inactive & recency > cold, segment := "cold" ]
	data[ recency <= cold & recency > warm, segment := "warm" ]
	data[ recency <= warm, segment := "active" ]

 	# split the active and warm segment further into new and old customers 
	# those that made their first purhase within a year, 
	# with respecitve to the cold and warm baseline.
	# and split it into low and high base on a specified average amount

	# Note that the old and new is based on the cold criteria for the "warm" segment!
	data[ segment == "warm" & first_purchase > cold & avg_amount < amount,
		  segment := "old warm low" ]

	data[ segment == "warm" & first_purchase > cold & avg_amount >= amount,
		  segment := "old warm high" ]

	data[ segment == "warm" & first_purchase <= cold & avg_amount < amount, 
		  segment := "new warm low" ]

	data[ segment == "warm" & first_purchase <= cold & avg_amount >= amount,
		  segment := "new warm high" ]

	# active
	data[ segment == "active" & first_purchase > warm & avg_amount < amount,
		  segment := "old active low" ]

	data[ segment == "active" & first_purchase > warm & avg_amount >= amount,
		  segment := "old active high" ]

	data[ segment == "active" & first_purchase <= warm & avg_amount < amount,
		  segment := "new active low" ]

	data[ segment == "active" & first_purchase <= warm & avg_amount >= amount,
		  segment := "new active high" ]

	# convert to ordered factor 
	data[ , segment := factor( segment, 
							   levels = c( "inactive", "cold",
							   			   "old warm low", "old warm high",
							   		   	   "new warm low", "new warm high",
							   		   	   "old active low", "old active high",
							   		   	   "new active low", "new active high" ) ) ]
	# order by customer id 
	return( data[ order(customer_id), ] )
}


