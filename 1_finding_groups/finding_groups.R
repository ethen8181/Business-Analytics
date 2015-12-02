#### finding similar groups

# environment setting
library(caret)
library(dplyr)
library(ggplot2)
library(reshape2)

setwd("/Users/ethen/Business-Analytics/1_finding_groups/data")
rm( list = ls() )

# ----------------------------------------------------------
#              example 1 : supply chain
# ----------------------------------------------------------

data_sku <- read.csv("DATA_2.01_SKU.csv")
head(data_sku)

# figure 1
ggplot( data_sku, aes( CV, ADS ) ) + 
geom_point() + 
labs( title = "SKU Example", y = "Average Daily Sales", x = "Coefficient of Variation" ) + 
geom_hline( yintercept = 4, color = "blue" ) + 
geom_vline( xintercept = .2, color = "blue" )

# hierarchical clustering 
d <- dist( scale(data_sku), method = "euclidean" )
cluster <- hclust( d, method = "ward.D" )
data_sku$groups  <- as.factor( cutree( cluster, k = 3 ) )

# figure 2
ggplot( data_sku, aes( CV, ADS, color = groups ) ) + 
geom_point() + 
labs( title = "SKU Example", y = "Average Daily Sales", x = "Coefficient of Variation" )

# ----------------------------------------------------------
#              example 2 : HR
# ----------------------------------------------------------
data_hr <- read.csv("DATA_2.02_HR.csv")
str(data_hr)

# correlation, method 1
# convert the correlation of oneself to 0
# look that which index is highly correlated, or i.e. often appears together
M <- abs( cor(data_hr) )
diag(M) <- 0 
which( M > 0.8, arr.ind = TRUE )

# correlation, method 2
hr_scaled <- data.frame( scale(data_hr) )
findCorrelation( cor(hr_scaled), cutoff = 0.8, names = TRUE )


# normality plot 
ggplot( melt( hr_scaled[ , 2:3 ] ), aes( value, fill = variable ) ) + 
geom_histogram( alpha = .4, position = "identity" )


# clustering 
d <- dist( hr_scaled[ , -1 ], method = "euclidean" ) 
cluster <- hclust( d, method = "ward.D" )
plot(cluster)
data_hr$groups <- cutree( cluster, k = 4 )

# aggregation 
hr_agg <- aggregate( .~ groups, data = data_hr, FUN = median )

# proportion size
tl <- table(data_hr$groups)
hr_agg$proportion <- tl / sum(tl)

# order the aggregated table by proportion 
hr_agg <- mutate( hr_agg, proportion = tl / sum(tl) ) %>%
          arrange( desc(proportion) )


# -------------------------------------------------------------------------------------------------------
# radar chart ??
# http://stackoverflow.com/questions/29452431/how-to-plot-a-radar-chart-in-ggplot2-or-r
# http://artax.karlin.mff.cuni.cz/r-help/library/fmsb/html/radarchart.html
# http://stackoverflow.com/questions/24248161/plot-a-radar-chart-for-each-row-in-a-data-frame-with-r
# rChart library
# http://stackoverflow.com/questions/28486186/modify-existing-function-for-a-radar-plot-in-r

Normalize <- function(x) ( x - min(x) ) / ( max(x) - min(x) ) 

# exclude the proportion and groups
hr_agg_2 <- hr_agg[ , c( -1, -8 ) ]
groups <- hr_agg$groups

hr_agg_melt <- apply( hr_agg_2, 2, Normalize ) %>%
			   data.frame() %>%
			   mutate( groups = groups ) %>%
               melt( id.vars = "groups" )


ggplot( hr_agg_melt, aes( x = variable  ) ) + 
geom_bar( aes( y = value, fill = groups ), stat = "identity" ) +
coord_polar() + 
facet_wrap( ~ groups ) +
ggtitle( "HR" ) + 
theme( legend.position = "none",  
	   axis.text.y = element_blank(), 
	   axis.text.x = element_text( color = "black" ), 
	   axis.ticks  = element_blank(),
	   axis.title  = element_blank() )


# ----------------------------------------------------------------------------------------------------------
# testing code, not implemented

# correlation plot
library(GGally)
sample <- dput( data_hr[ 1:10, -c(5,6) ] )
ggcorr( sample, nbreaks = 5 )

