# finding similar groups

# environment setting
library(caret)
library(scales)
library(ggradar)
library(ggplot2)
library(data.table)
rm( list = ls() )
setwd("/Users/ethen/Business-Analytics/finding_groups")


# ----------------------------------------------------------
#              example 1 : supply chain
# ----------------------------------------------------------
data_sku <- fread("data/DATA_2.01_SKU.csv")
head(data_sku)

# figure 1
ggplot( data_sku, aes( CV, ADS ) ) + geom_point() +  
geom_hline( yintercept = 4, color = "blue" ) + 
geom_vline( xintercept = .2, color = "blue" ) + 
labs( x = "Coefficient of Variation", y = "Average Daily Sales", 
	  title = "SKU Example" )

# hierarchical clustering, with k (clustering number) = 3 
d <- dist( scale(data_sku), method = "euclidean" )
cluster <- hclust( d, method = "ward.D" )
data_sku[ ,  groups := as.factor( cutree( cluster, k = 3 ) ) ]

# figure 2
ggplot( data_sku, aes( CV, ADS, color = groups ) ) + geom_point() + 
labs( x = "Coefficient of Variation", y = "Average Daily Sales", 
	  title = "SKU Example" )


# ----------------------------------------------------------
#              example 2 : HR
# ----------------------------------------------------------
data_hr <- fread("data/DATA_2.02_HR.csv")
head(data_hr)

# correlation, method 1
# convert the correlation of oneself to 0
# look that which index is highly correlated, or i.e. often appears together
# M <- abs( cor(data_hr) )
# diag(M) <- 0 
# which( M > 0.8, arr.ind = TRUE )

# correlation, method 2
scaled <- scale(data_hr)
data_hr_scaled <- data.table(scaled)
findCorrelation( cor(data_hr_scaled), cutoff = 0.8, names = TRUE )


# normality plot 
ggplot( melt( data_hr_scaled[ , c( "LPE", "NP" ), with = FALSE ] ), 
		aes( value, fill = variable ) ) + 
geom_histogram( alpha = .4, position = "identity" )


# clustering 
d <- dist( data_hr_scaled[ , -1, with = FALSE ], method = "euclidean" ) 
cluster <- hclust( d, method = "ward.D" )
# plot(cluster) # dendogram
groups <- as.factor( cutree( cluster, k = 4 ) )
data_hr_scaled[ , groups := groups ]

# aggregation 
# hr_agg <- aggregate( .~ groups, data = data_hr_scaled, FUN = median )
hr_agg <- data_hr_scaled[ , lapply( .SD, median ), by = groups ]

# order by cluster's proportion size
t <- table(data_hr_scaled$groups)
hr_agg[ , proportion := t / sum(t) ][ order(-proportion) ]

# order the aggregated table by proportion, dplyr way
# library(dplyr)
# hr_agg <- mutate( hr_agg, proportion = tl / sum(tl) ) %>%
#           arrange( desc(proportion) )


# rescale is equivalent to applying Normalize below
# Normalize <- function(x) ( x - min(x) ) / ( max(x) - min(x) ) 


# radar plot
# future work, currently does not support facetting
# the first column has to be the group
hr_agg2 <- hr_agg[ , -c( "groups", "proportion" ), with = FALSE ]
hr_agg2 <- hr_agg2[ , lapply( .SD, rescale ) ]
hr_agg2[ , groups := as.factor( paste0( "group", 1:nrow(hr_agg2) ) ) ]
name <- colnames(hr_agg2)
setcolorder( hr_agg2, c( "groups", name[ name != "groups" ] ) )

ggradar(hr_agg2)


# ----------------------------------------------------------------------------------------------------------
# testing code, not implemented
# exclude the proportion and groups
hr_agg_2 <- hr_agg[ , c( -1, -8 ) ]
groups <- hr_agg$groups

hr_agg_melt <- apply( hr_agg_2, 2, Normalize ) %>%
			   data.frame() %>%
			   mutate( groups = groups ) %>%
               melt( id.vars = "groups" )


ggplot( hr_agg_melt, aes( x = variable  ) ) + 
geom_bar( aes( y = value, fill = groups ), stat = "identity" ) +
coord_polar() + facet_wrap( ~ groups ) + ggtitle( "HR" ) + 
theme( legend.position = "none",  
	   axis.text.y = element_blank(), 
	   axis.text.x = element_text( color = "black" ), 
	   axis.ticks  = element_blank(),
	   axis.title  = element_blank() )

