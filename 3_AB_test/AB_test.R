
# campaign response rate testing
# Problem definition:

# we're sending out number of sales offers at one or more chosen price-points 
# to different populations. 
# we want to measure the response rate at each different price level
# so that we can make a good choice of which price to continue with the future
# sales campaigns


# ------------------------------------------------------------------
#				Design : determining the sample size 
# ------------------------------------------------------------------

baseline  <- 0.1  # baseline conversion rate 
delta 	  <- 0.02 # minimum detectable boundary ( practical significance boundary )
power 	  <- 0.8  # sensitivity 
sig_level <- 0.05 # specificity 

result <- power.prop.test( p1 = baseline, p2 = baseline + delta, 
				 		   power = power, sig.level = sig_level,
				 		   alternative = "two.sided" )
round(result$n)


# ------------------------------------------------------------------
#				Analyze the result  
# ------------------------------------------------------------------

# parameters 
count_control 	 <- 974
sizes_control 	 <- 10072
count_experiment <- 1242
sizes_experiment <- 9886

ABTest <- function( count_control = count_control, 
					sizes_control = sizes_control,
					count_experiment = count_experiment, 
					sizes_experiment = sizes_experiment )
{
	# probability of each group 
	p_control <- count_control / sizes_control
	p_experiment <- count_experiment / sizes_experiment

	# @p : pooled probability
	# @std_error pooled standard deviation (error ) 
	p <- ( count_control + count_experiment ) / ( sizes_experiment + sizes_control )
	std_error <- sqrt( p * ( 1 - p ) * ( 1 / sizes_control + 1 / sizes_experiment ) )

	# 95 percent confidence interval's z score = 1.96, equivalent to 
	# qnorm( 0.975 )
	difference <- p_experiment - p_control
	confidence <- difference + c( -1, 1 ) * 1.96 * std_error

	return( data.frame( lower = confidence[1], 
						mean  = difference,
						upper = confidence[2] ) )
}

confidence <- ABTest( count_control = count_control, 
					  sizes_control = sizes_control,
					  count_experiment = count_experiment, 
					  sizes_experiment = sizes_experiment )

# ------------------------------------------------------------------
# different scenarios

# fixed artifical plot
# using delta = 0.02 as the minimum detectable boundary 
library(ggplot2)

scenario <- as.character(2:6)
lower <- c( -0.008, 0.011, -0.025, -0.005, 0.015 )
mean  <- c( 0.005, 0.014, 0.005, 0.025, 0.025 )
upper <- c( 0.018, 0.017, 0.035, 0.055, 0.035 )

examples <- data.frame( scenario, lower, mean, upper )
examples <- rbind( cbind( scenario = "1", confidence ), examples )
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


# ------------------------------------------------------------------
#				Variance   
# ------------------------------------------------------------------

# mean 
# might have to use a power.t.test to calculate the sample size needed 
visit <- c( 87029, 113407, 84843, 104994, 99327, 92052, 60684 )
mean(visit) + c( -1, 1 ) * 1.96 * sd(visit) / sqrt( length(visit) )


group1 <- 64454
group2 <- 61818
SanityCheck <- function( group1, group2 )
{
	n <- group1 + group2
	confidence <- n * 0.5 + c( -1, 1 ) * 1.96 * sqrt( n * 0.5 * 0.5 ) 
	return(confidence)
}
( sanity <- SanityCheck( group1, group2 ) )


# ----------------------------------------------------------------------------
# rule of thumb
# http://www.exp-platform.com/Documents/2014%20experimentersRulesOfThumb.pdf

# determining sample size required
# https://signalvnoise.com/posts/3004-ab-testing-tech-note-determining-sample-size

