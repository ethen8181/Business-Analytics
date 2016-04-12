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


# http://varianceexplained.org/r/empirical_bayes_baseball/
library(MASS)
library(knitr)
library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)


# load the batting and pitching data from the Lahman package
# the Master is used to get further details e.g. corresponding 
# player name for the player id column in the Batting and Pitching data
data(Master)
data(Batting)
data(Pitching)

# prevent all warning and messages in code chunks,
# and float numbers to print only three digits after the decimal point
opts_chunk$set( warning = FALSE, message = FALSE )
options( digits = 3 )

# anti_join : return all rows from x, where there're no matching values in y
# H = hits
# AB = at bats
career <- Batting %>% 
		  filter( AB > 0 ) %>% 
		  anti_join( Pitching, by = "playerID" ) %>%
		  group_by(playerID) %>%
		  summarize( H = sum(H), AB = sum(AB) ) %>%
		  mutate( average = H / AB )

# map the player name to player id
career <- Master %>%
		  tbl_df() %>%
		  select( playerID, nameFirst, nameLast ) %>%
		  unite( name, nameFirst, nameLast, sep = " " ) %>%
		  inner_join( career, by = "playerID" )

career_filtered <- filter( career, AB > 500 ) 

ggplot( career_filtered, aes(average) ) +
geom_histogram( binwidth = .005 )

m <- MASS::fitdistr( career_filtered$average, densfun = dbeta,
					 start = list( shape1 = 70, shape2 = 200 ) )

alpha0 <- m$estimate[1]
beta0  <- m$estimate[2]

ggplot(career_filtered) +
geom_histogram( aes( x = average, y = ..density.. ), binwidth = .005 ) +
xlab("Batting average") + 
stat_function( fun = function(x) dbeta( x, alpha0, beta0 ), 
			   color = "red", size = 1 )


career <- career %>% 
		  mutate( estimate = ( H + alpha0 ) / ( AB + alpha0 + beta0 ) )

options( digits = 3 )
arrange( career, desc(estimate) )
arrange( career, estimate)

ggplot( career, aes( average, estimate, color = AB ) ) + geom_point() + 
geom_hline( yintercept = alpha0 / ( alpha0 + beta0 ), color = "red" ) + 
labs( x = "Batting average", y = "Empirical Bayes batting average" ) +
geom_abline( intercept = 0, slope = 1, color = "red" ) + 
scale_color_gradient( trans = "log", breaks = 10^(1:4) ) # convert to log scale


# http://varianceexplained.org/r/credible_intervals_baseball/
library(broom)

career <- career %>%
		  mutate( alpha1 = H + alpha0, beta1 = AB - H + beta0 )

yankee_1998 <- c( "brosisc01", "jeterde01", "knoblch01", "martiti02", 
				  "posadjo01", "strawda01", "willibe02" )

career_yankee_1998 <- career %>%
					  filter( playerID %in% yankee_1998 )

career_yankee_1998 %>%
inflate( x = seq( .18, .33, .0002 ) )



iris %>%
inflate( x = seq( 1, 10, 3 ) ) %>%
data.frame()
