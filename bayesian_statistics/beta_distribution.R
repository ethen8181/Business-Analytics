library(dplyr)
library(ggplot2)

# simulated data
# "\u03B1" unicode for the greek letter alpha
sim <- data.frame( a = c( 81, 82, 81 + 100 ),
				   b = c( 219, 219, 219 + 200 ) ) %>%
	   group_by( a, b ) %>%
	   do( data_frame( x = seq( 0, 1, by = .001 ), y = dbeta( x, .$a, .$b ) ) ) %>%
	   mutate( Parameters = paste0( "\u03B1 = ", a, ", \u03B2 = ", b ) ) %>%
	   ungroup() %>%
	   mutate( Parameters = factor( Parameters, levels = unique(Parameters) ) )

# plot of the distribution
PlotBeta <- function(sim)
{
	ggplot( sim, aes( x, y, color = Parameters ) ) + geom_line() +
	xlim( 0, .5 ) + ylab("Density of beta") + theme_bw()
}
PlotBeta( sim = filter( sim, a == 81 ) )


# update 1 hit of 1 bat
PlotBeta( sim = filter( sim, a == 81 | a == 82 ) )


# update 100 hit of 300 bat
PlotBeta( sim = sim )
