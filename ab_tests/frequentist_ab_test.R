library(scales)
library(ggplot2)
library(data.table)

PlotPower <- function( size, min_diff ){
	
	size_a <- size_b <- size * 0.5 # size are assumed to be equal
	p_a <- 0.08 # your current baseline solution
	p_b <- p_a + min_diff
	count_a  <- size_a * p_a
	count_b  <- size_b * p_b
	p_pooled <- ( count_a  + count_b ) / ( size_a + size_b )
	Z <- ( p_b - p_a ) / sqrt( p_pooled * ( 1 - p_pooled ) * ( 1 / size_a + 1 / size_b ) )

	# Z corresponds to the mean of the normal distribution
	mean1 <- 0
	mean2 <- Z

	x <- seq( -4, 6, 0.1 ) # use for generating the x axis of the normal distribution
	data <- data.frame( x = x, y1 = dnorm( x, mean1, 1 ), y2 = dnorm( x, mean2, 1 ) )

	plot <- ggplot( data, aes( x = x ) ) +
			geom_line( aes( y = y1, colour = 'H0 is true' ), size = 1.2 ) +
			geom_line( aes( y = y2, colour = 'H1 is true' ), size = 1.2 ) +
			geom_area( aes( y = y1, x = ifelse( x > 1.65, x, NA ) ), fill = 'black' ) +
			geom_area( aes( y = y2, x = ifelse( x > 1.65, x, NA ) ), fill = 'blue', alpha = 0.3 ) +
			labs( x = '', y = '', title = sprintf( 'p1 = %s, p2 = %s, size = %d', p_a, p_b, size ) ) + 
			theme( legend.title = element_blank() ) +
			scale_colour_manual( breaks = c( "H0 is true", "H1 is true" ), 
								 values = c( "blue", "red" ) )
	return(plot)
}

PlotPower( size = 5000, min_diff = 0.02 )

# smaller N
PlotPower( size = 2500, min_diff = 0.02 )

# smaller probability difference
PlotPower( size = 5000, min_diff = 0.01 )


# -----------------------------------------------------------------------------
# Power test
baseline  <- 0.1  # baseline conversion rate 
delta 	  <- 0.02 # minimum detectable boundary (practical significance boundary)
power 	  <- 0.8  # specificity, or true negative rate
sig_level <- 0.05 # false positive rate
result <- power.prop.test( p1 = baseline, p2 = baseline - delta, 
				 		   power = power, sig.level = sig_level,
				 		   alternative = "two.sided" )
round(result$n)


# -----------------------------------------------------------------------------
baseline  <- 0.1
power 	  <- 0.8 
sig_level <- 0.05

# calculate the the required sample size for reaching the 
# range detectable difference (dd)
dd <- seq( from = 0.01, to = 0.03, by = 0.0001 )
result <- matrix( nrow = length(dd), ncol = 2 )
result[ , 1 ] <- dd
for( i in 1:length(dd) ){
	result[ i, 2 ] <- power.prop.test( p1 = baseline, p2 = baseline - dd[i], 
									   power = power, sig.level = sig_level,
									   alternative = "two.sided" )$n
}

result <- data.table(result)
setnames( result, c( 'dd', 'n' ) )

ggplot(data = result, aes( x = dd, y = n ) ) +
geom_line() + ylab("required sample size") + xlab("Detectable difference") + 
scale_x_continuous( labels = comma )


# ------------------------------------------------------------------
#				Example 
# ------------------------------------------------------------------

baseline  <- 0.1  # baseline conversion rate 
delta 	  <- 0.02 # minimum detectable boundary (practical significance boundary)
power 	  <- 0.8  # specificity, or true negative rate
sig_level <- 0.05 # false positive rate
result <- power.prop.test( p1 = baseline, p2 = baseline - delta, 
                           power = power, sig.level = sig_level,
                           alternative = "two.sided" )
round(result$n)


# ------------------------------------------------------------------
# Analyze the result 

# parameters
count_control <- 974
sizes_control <- 10072
count_experiment <- 1242
sizes_experiment <- 9886

result <- prop.test( c( count_control, count_experiment ), 
					 c( sizes_control, sizes_experiment ) )
result

# do the computation ourselves to see that the confidence interval matches
# compute the probability of each group and the standard error
p1 <- count_control / sizes_control
p2 <- count_experiment / sizes_experiment
se <- sqrt( p1 * ( 1 - p1 ) / sizes_control + p2 * ( 1 - p2 ) / sizes_experiment )

# 95 percent confidence interval's z score
conf_level <- 0.95
zscore <- qnorm( conf_level + ( 1 - conf_level ) / 2 )
conf_int <- abs(p2 - p1) + c( -1, 1 ) * zscore * se
conf_int

# ------------------------------------------------------------------
# different scenarios

# fixed artifical plot
# using delta = 0.02 as the minimum detectable boundary 
scenario <- as.character(1:6)
lower <- c( conf_int[1], -0.008, 0.011, -0.025, -0.005, 0.015 )
mean  <- c( abs(p2 - p1), 0.005, 0.014, 0.005, 0.025, 0.025 )
upper <- c( conf_int[2], 0.018, 0.017, 0.035, 0.055, 0.035 )
examples <- data.frame( scenario, lower, mean, upper )
examples$scenario <- factor( examples$scenario, levels = as.character(6:1) )
 
ggplot( examples, aes( mean, scenario, color = scenario ) ) + 
geom_point() + 
geom_errorbarh( aes( xmin = lower, xmax = upper ), height = 0.1 ) + 
geom_vline( xintercept = 0, color = "black" ) + 
geom_vline( xintercept = delta, color = "blue", linetype = "dotted" ) + 
geom_vline( xintercept = -delta, color = "blue", linetype = "dotted" ) +
scale_color_discrete( breaks = as.character(1:6) ) +  
labs( title = "Different Scenarios of Confidence Interval",
	  x = "confidence interval" ) 


# sanity checks
group1 <- 64454
group2 <- 61818
SanityCheck <- function( group1, group2 )
{
	# 95 percent confidence interval = qnorm(0.95 + (1-0.95) / 2 )
	n <- group1 + group2
	confidence <- n * 0.5 + c( -1, 1 ) * qnorm(0.975) * sqrt( n * 0.5 * 0.5 ) 
	return(confidence)
}
( sanity <- SanityCheck( group1, group2 ) )


# ----------------------------------------------------------------------------
# rule of thumb for online testing, future reference
# http://www.exp-platform.com/Documents/2014%20experimentersRulesOfThumb.pdf

