# bayesian statistics
library(tidyr)
library(dplyr)
library(knitr)
library(broom)
library(ggplot2)

# set float numbers to print only three digits after the decimal point
options( digits = 3 )

# -----------------------------------------------------------------------
# beta distribution
# http://varianceexplained.org/statistics/beta_distribution_and_baseball/
# -----------------------------------------------------------------------

# simulated data
# "\u03B1" unicode for the greek letter alpha
sim <- data.frame( a = c( 81, 82, 81 + 100 ),
				   b = c( 219, 219, 219 + 200 ) ) %>%
	   group_by( a, b ) %>%
	   do( data_frame( x = seq( 0, 1, by = .001 ), y = dbeta( x, .$a, .$b ) ) ) %>%
	   mutate( parameters = paste0( "\u03B1 = ", a, ", \u03B2 = ", b ) ) %>%
	   ungroup() %>%
	   mutate( parameters = factor( parameters, levels = unique(parameters) ) )

# plot of the distribution
PlotBeta <- function(sim)
{
	ggplot( sim, aes( x, y, color = parameters ) ) + geom_line() +
	xlim( 0, .5 ) + ylab("Density of beta") + theme_bw()
}
PlotBeta( sim = filter( sim, a == 81 ) )


# update 1 hit of 1 bat
PlotBeta( sim = filter( sim, a == 81 | a == 82 ) )


# update 100 hit of 300 bat
PlotBeta( sim = sim )


# -----------------------------------------------------------------------
# empirical bayes
# http://varianceexplained.org/r/empirical_bayes_baseball/
# -----------------------------------------------------------------------

# load the Batting and Pitching data from the Lahman package
# the Master is used to get further details e.g. corresponding 
# player name for the player id column in the Batting and Pitching data
library(Lahman)
data(Master)
data(Batting)
data(Pitching)

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
career <- tbl_df(Master) %>%
		  select( playerID, nameFirst, nameLast ) %>%
		  unite( name, nameFirst, nameLast, sep = " " ) %>%
		  inner_join( career, by = "playerID" )

# best and worse batters
arrange( career, desc(average) ) %>%
head(5)
arrange( career, average ) %>%
head(5)

# filter out noise and 
# plot the average batting distribution
career_filtered <- filter( career, AB > 500 ) 
ggplot( career_filtered, aes(average) ) +
geom_histogram( binwidth = .005 )

# fit beta distribution, you can specify any
# starting parameter for shape1 and shape2 (alpha and beta),
# the outputted warning does not matter
m <- MASS::fitdistr( career_filtered$average, densfun = dbeta,
					 start = list( shape1 = 70, shape2 = 200 ) )

alpha0 <- m$estimate[1]
beta0  <- m$estimate[2]

# observe the fitted beta distribution
ggplot(career_filtered) +
geom_histogram( aes( x = average, y = ..density.. ), binwidth = .005 ) +
xlab("Batting average") + 
stat_function( fun = function(x) dbeta( x, alpha0, beta0 ), 
			   color = "red", size = 1 )


# update the prior batting average to all players 
# using the unfiltered dataset
career <- mutate( career, estimate = ( H + alpha0 ) / ( AB + alpha0 + beta0 ) )

# best and worse batters new
arrange( career, desc(estimate) ) %>%
head(5)
arrange( career, estimate ) %>%
head(5)

# plot shrinkage
ggplot( career, aes( average, estimate, color = AB ) ) + geom_point() + 
geom_hline( yintercept = alpha0 / ( alpha0 + beta0 ), color = "red" ) + 
labs( x = "Batting average", y = "Empirical Bayes batting average" ) +
geom_abline( intercept = 0, slope = 1, color = "red" ) + 
scale_color_gradient( trans = "log", breaks = 10^(1:4) ) # convert to log scale


# -----------------------------------------------------------------------
# credible intervals
# http://varianceexplained.org/r/credible_intervals_baseball/
# -----------------------------------------------------------------------

# traditinal binomal confidence interval
binom.test( x = 1, n = 3 )$conf.int


# calculate the prior beta distribution's parameter
# alpha0 and beta0 is calculated in the last section
career <- mutate( career, alpha1 = H + alpha0, beta1 = AB - H + beta0 )

# filter sample
yankee_1998 <- c( "brosisc01", "jeterde01", "knoblch01", "martiti02", 
				  "posadjo01", "strawda01", "willibe02" )
career_yankee_1998 <- filter( career, playerID %in% yankee_1998 )

# create the x axis for the beta distribution's probability density function
yankee_beta <- career_yankee_1998 %>%
			   inflate( x = seq( .18, .33, .0005 ) ) %>%
			   ungroup() %>%
			   mutate( density = dbeta( x, alpha1, beta1 ) )

# visualize posterior beta
ggplot( yankee_beta, aes( x, density, color = name ) ) + geom_line() +
stat_function( fun = function(x) dbeta( x, alpha0, beta0 ),
			   lty = 2, color = "black" )


# visualize the credible interval for one player
jeter <- filter( yankee_beta, name == "Derek Jeter" )

# calculate the cumulative probability and
# extract the ones that lies between the 95 percent credible interval
jeter_pred <- jeter %>%
			  mutate( cumulative = pbeta( x, alpha1, beta1 ) ) %>%
			  filter( cumulative > .025, cumulative < .975 )

# obtain the x coordinate of the 95 percent credible interval's 
# endpoint to visualize the error bar
jeter_low  <- qbeta( .025, jeter$alpha1[1], jeter$beta1[1] )
jeter_high <- qbeta( .975, jeter$alpha1[1], jeter$beta1[1] )

# credible interval plot
ggplot( jeter, aes( x, density ) ) + geom_line() +
geom_ribbon( data = jeter_pred, aes( ymin = 0, ymax = density ),
			 alpha = .25, fill = "red" ) +
stat_function( fun = function(x) dbeta( x, alpha0, beta0 ),
			   lty = 2, color = "black" ) +
geom_errorbarh( aes( xmin = jeter_low, xmax = jeter_high, y = 0 ), 
				height = 3.5, color = "red" ) +
xlim( .18, .34 )

# all the player's credible interval
yankee_1998_career <- yankee_1998_career %>%
					  mutate( low  = qbeta( .025, alpha1, beta1 ),
					  		  high = qbeta( .975, alpha1, beta1 ) )

select( yankee_1998_career, -alpha1, -beta1, -eb_estimate ) %>%
knitr::kable()

# alternative credible interval plot
yankee_1998_career %>%
mutate( name = reorder( name, average ) ) %>%
ggplot( aes( average, name ) ) +
geom_point() +
geom_errorbarh( aes( xmin = low, xmax = high ) ) +
geom_vline( xintercept = alpha0 / (alpha0 + beta0 ), color = "red", lty = 2 ) +
xlab("Estimated batting average (95% interval)") +
ylab("Player")


# credible interval v.s. confidence interval 
career <- career %>%
		  mutate( low  = qbeta( .025, alpha1, beta1 ),
			 	  high = qbeta( .975, alpha1, beta1 ) )

# draw random 20 players
set.seed(2015)
some <- career %>%
		sample_n(20) %>%
		mutate( name = paste0( name, " (", H, "/", AB, ")" ) )

# credible interval
bayesian <- some %>%
			select( playerID, name, AB, estimate = estimate,
				    low = low, high = high) %>%
			mutate( method = "Credible" )

# confidence interval
frequentist <- some %>%
			   group_by( playerID, name, AB ) %>%
			   do( tidy( binom.test( .$H, .$AB ) ) ) %>%
			   select( playerID, name, estimate, low = conf.low, high = conf.high ) %>%
			   mutate( method = "Confidence" )

combined <- bind_rows( bayesian, frequentist )
combined %>%
mutate( name = reorder( name, -AB ) ) %>%
ggplot( aes( estimate, name, color = method, group = method ) ) +
geom_point() +
geom_errorbarh( aes( xmin = low, xmax = high ) ) +
geom_vline( xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2 ) +
xlab("Estimated batting average") +
ylab("Player") +
labs(color = "")


# -----------------------------------------------------------------------
# Understanding the Bayesian Approach to False Discovery Rates
# http://varianceexplained.org/r/bayesian_fdr_baseball/
# -----------------------------------------------------------------------

hank_aaron <- filter( career, name == "Hank Aaron" )

# posterior beta
hank_aaron %>%
do( data_frame( x = seq( .27, .33, .0002 ),
				density = dbeta( x, .$alpha1, .$beta1 ) ) ) %>%
ggplot( aes( x, density ) ) + geom_line() +
geom_ribbon( aes( ymin = 0, ymax = density * ( x < .3 ) ),
				  alpha = .1, fill = "red" ) +
geom_vline( color = "red", lty = 2, xintercept = .3 )

# posterior error probability (PEP)
pbeta( .3, hank_aaron$alpha1[1], hank_aaron$beta1[1] )

# PEP for every player
career <- career %>%
		  mutate( PEP = pbeta( .3, alpha1, beta1 ) )

# histogram of PEP
ggplot( career, aes(PEP) ) +
geom_histogram( binwidth = .02 ) +
xlab("Posterior Error Probability (PEP)") +
xlim( 0, 1 )

# PEP and estimate batting average
ggplot( career, aes( estimate, PEP, color = AB ) ) + geom_point( size = 1 ) +
xlab("(Shrunken) batting average estimate") +
ylab("Posterior Error Probability (PEP)") +
geom_vline( color = "red", lty = 2, xintercept = .3 ) +
scale_colour_gradient( trans = "log", breaks = 10^(1:5) )

# ranking PEP
by_PEP <- career %>%
		  arrange(PEP) %>%
		  mutate( rank = row_number(PEP) ) %>%
		  select( rank, name, H, AB, estimate, PEP )

by_PEP %>%
head(10) %>%
knitr::kable()

by_PEP %>%
slice(90:100) %>%
knitr::kable()

# top 100 players false positive rate
top_players <- career %>%
			   arrange(PEP) %>%
			   head(100)

# PEP's cumulative mean for meeting the false discovery rate
career <- career %>%
		  arrange(PEP) %>%
		  mutate( qvalue = cummean(PEP) )

hall_of_fame <- career %>%
				filter( qvalue < .05 )

# numbers of players included at various q-value cutoff
career %>%
filter( qvalue < .25 ) %>%
ggplot( aes( qvalue, rank(PEP) ) ) + geom_line() +
labs( x = "q-value cutoff", y = "Number of players included" )

# link to original code
# https://github.com/dgrtwo/dgrtwo.github.com/tree/master/_R


