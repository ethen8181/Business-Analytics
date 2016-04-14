# bayesian statistics
library(ggplot2)
library(data.table)

# set float numbers to print only three digits after the decimal point
options( digits = 3 )

# -----------------------------------------------------------------------
# beta distribution
# http://varianceexplained.org/statistics/beta_distribution_and_baseball/
# -----------------------------------------------------------------------

# simulated data,
# generate a sequence of numbers for each combination of a and b
# to plot the probability density function.
# "\u03B1" unicode for the greek letter alpha
sim <- data.table( a = c( 81, 82, 81 + 100 ),
				   b = c( 219, 219, 219 + 200 ) )
sim <- sim[ , .( x = seq( 0, 1, by = 0.002 ) ), by = .( a, b ) ]

sim[ , `:=`( y = dbeta( x, a, b ),
			 parameters = paste0( "\u03B1 = ", a, ", \u03B2 = ", b ) ) ]
sim[ , parameters := factor( parameters, levels = unique(parameters) ) ]


# plot of the distribution
PlotBeta <- function(sim)
{
	ggplot( sim, aes( x, y, color = parameters ) ) + geom_line() +
	xlim( 0, .5 ) + ylab("Density of beta") + theme_bw()
}
PlotBeta( sim = sim[ a == 81, ] )

# update 1 hit of 1 bat
PlotBeta( sim = sim[ a %in% c( 81, 82 ), ] )

# update 100 hit of 300 bat
PlotBeta( sim = sim )


# -----------------------------------------------------------------------
# empirical bayes
# http://varianceexplained.org/r/empirical_bayes_baseball/
# -----------------------------------------------------------------------

# load the batting and pitching data from the Lahman package
# the Master is used to get further details e.g. corresponding 
# player name for the player id column in the Batting and Pitching data
library(Lahman)
data(Master)
data(Batting)
data(Pitching)

master   <- data.table( Master  , key = "playerID" )
pitching <- data.table( Pitching, key = "playerID" )
batting  <- data.table( Batting , key = "playerID" )

# ! stands for not join,
# return all rows from x, where there're no matching values in y
# H = hits
# AB = at bats
career <- batting[ AB > 0, ][!pitching]
career <- career[ , .( H = sum(H), AB = sum(AB) ), by = playerID ]
career[ , average := H / AB ]

# map the player name to player id
master <- master[ , .( playerID, nameFirst, nameLast ) ]
career <- master[ , name := paste( nameFirst, nameLast ) ][career]
career[ , `:=`( nameFirst = NULL, nameLast = NULL ) ]

# best and worse batters
career[ head( order(-average) ), ]
career[ head( order(average) ), ]

# filter out noise and 
# plot the average batting distribution
career_filtered <- career[ AB > 500, ]
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
career[ , estimate := ( alpha0 + H ) / ( alpha0 + beta0 + AB ) ]

# best and worse batters new
career[ head( order(-estimate) ), ]
career[ head( order(estimate) ), ]

# plot shrinkage
ggplot( career, aes( average, estimate, color = AB ) ) + geom_point() + 
geom_hline( yintercept = alpha0 / ( alpha0 + beta0 ), color = "red", lty = 2 ) + 
labs( x = "Batting average", y = "Empirical Bayes batting average" ) +
geom_abline( intercept = 0, slope = 1, color = "red" ) + 
scale_color_gradient( trans = "log", breaks = 10^(1:4) )


# -----------------------------------------------------------------------
# understanding credible intervals
# http://varianceexplained.org/r/credible_intervals_baseball/
# -----------------------------------------------------------------------

# traditinal binomal confidence interval
binom.test( x = 1, n = 3 )$conf.int


# calculate the prior beta distribution's parameter
# alpha0 and beta0 is calculated in the last section
career[ , `:=`( alpha1 = alpha0 + H, beta1 = beta0 + AB - H ) ]

# filter sample
yankee_1998 <- c( "brosisc01", "jeterde01", "knoblch01", "martiti02", 
				  "posadjo01", "strawda01", "willibe02" )
career_yankee_1998 <- career[ playerID %in% yankee_1998, ]

# create the x axis for the beta distribution's probability density function
expand <- career_yankee_1998[ , .( x = seq( .18, .33, .0005 ) ), by = playerID ]
yankee_beta <- career_yankee_1998[expand]
yankee_beta[ , density := dbeta( x, alpha1, beta1 ) ]

# visualize posterior beta
ggplot( yankee_beta, aes( x, density, color = name ) ) + geom_line() +
stat_function( fun = function(x) dbeta( x, alpha0, beta0 ),
			   lty = 2, color = "black" )


# visualize the credible interval for one player
jeter <- yankee_beta[ name == "Derek Jeter", ]

# calculate the cumulative probability and
# extract the ones that lies between the 95 percent credible interval
p <- 0.95
ci_low  <- ( 1 - p ) / 2
ci_high <- p + ci_low

jeter_pred <- jeter[ , cumulative := pbeta( x, alpha1, beta1 ) 
				  ][ cumulative > ci_low & cumulative < ci_high ]

# obtain the x coordinate of the 95 percent credible interval's 
# endpoint to visualize the error bar
jeter_low  <- qbeta( ci_low , jeter$alpha1[1], jeter$beta1[1] )
jeter_high <- qbeta( ci_high, jeter$alpha1[1], jeter$beta1[1] )

# credible interval plot
ggplot( jeter, aes( x, density ) ) + geom_line( color = "blue" ) +
xlim( .18, .34 ) + 
geom_ribbon( data = jeter_pred, aes( ymin = 0, ymax = density ),
			 alpha = .25, fill = "red" ) +
stat_function( fun = function(x) dbeta( x, alpha0, beta0 ),
			   lty = 2, color = "black" ) +
geom_errorbarh( aes( xmin = jeter_low, xmax = jeter_high, y = 0 ), 
				height = 3.5, color = "red" )

# all the player's credible interval
career_yankee_1998[ , `:=`( low  = qbeta( ci_low , alpha1, beta1 ),
							high = qbeta( ci_high, alpha1, beta1 ) ) ]

col_names <- colnames(career_yankee_1998)
show <- col_names[ !col_names %in% c( "alpha1", "beta1", "estimate" ) ]
knitr::kable( career_yankee_1998[ , show, with = FALSE ] )

# alternative credible interval plot
ggplot( career_yankee_1998, aes( average, reorder( name, average ) ) ) +
geom_point() +
geom_errorbarh( aes( xmin = low, xmax = high ), height = 0.8 ) +
geom_vline( xintercept = alpha0 / (alpha0 + beta0 ), color = "red", lty = 2 ) +
xlab("Estimated batting average (95% interval)") +
ylab("Player")


# credible interval v.s. confidence interval
library(broom) # for tidying binom.test's output
library(ggthemes)

career[ , `:=`( low  = qbeta( ci_low , alpha1, beta1 ),
				high = qbeta( ci_high, alpha1, beta1 ) ) ]

# draw random 20 players
set.seed(2015)
some <- career[ sample.int( nrow(career), 20 ), ]
some[ , name := paste0( name, " (", H, "/", AB, ")" ) ]

# credible interval
bayesian <- some[ , .( name, AB, estimate, low, high ) ]
bayesian[ , method := "Credible" ]

# confidence interval
frequentist <- some[ , broom::tidy( binom.test( H, AB ) ), 
					   by = .( playerID, name, AB ) ]
frequentist <- frequentist[ , .( name, AB, estimate, 
								 low = conf.low, high = conf.high ) ]
frequentist[ , method := "Confidence"]

combined <- rbind( bayesian, frequentist )
ggplot( combined, aes( estimate, reorder( name, -AB ), color = method ) ) +
geom_point() +
geom_errorbarh( aes( xmin = low, xmax = high ) ) +
geom_vline( xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2 ) +
labs( x = "Estimated batting average", y = "Player", color = "" ) +
scale_color_tableau()


# -----------------------------------------------------------------------
# Understanding the Bayesian Approach to False Discovery Rates
# http://varianceexplained.org/r/bayesian_fdr_baseball/
# -----------------------------------------------------------------------

( hank_aaron <- career[ name == "Hank Aaron", ] )

# hall of fame threshold
threshold <- .3

# posterior beta
# merge the original data.table with the generated sequence ones,
hank_aaron <- hank_aaron[ hank_aaron[ , .( x = seq( .27, .33, .0004 ) ), by = playerID ] ]
hank_aaron[ , density := dbeta( x, alpha1, beta1 ) ]

ggplot( hank_aaron, aes( x, density ) ) + geom_line() +
geom_ribbon( aes( ymin = 0, ymax = density * ( x < threshold ) ),
				  alpha = .1, fill = "red" ) +
geom_vline( color = "red", lty = 2, xintercept = threshold )

# posterior error probability (PEP)
pbeta( threshold, hank_aaron$alpha1[1], hank_aaron$beta1[1] )

# PEP for every player
career[ , PEP := pbeta( threshold, alpha1, beta1 ) ]

# histogram of PEP
ggplot( career, aes(PEP) ) + geom_histogram( binwidth = .02 ) +
xlab("Posterior Error Probability (PEP)") + xlim( 0, 1 )

# PEP and estimate batting average
ggplot( career, aes( estimate, PEP, color = AB ) ) + geom_point( size = 1 ) +
xlab("(Shrunken) batting average estimate") +
ylab("Posterior Error Probability (PEP)") +
geom_vline( color = "red", lty = 2, xintercept = threshold ) +
scale_colour_gradient( trans = "log", breaks = 10^(1:5) )

# ranking PEP
career <- career[ order(PEP), ]
by_PEP <- career[ , rank := 1:nrow(career) 
			   ][ , .( rank, name, H, AB, estimate, PEP ) ]

head( by_PEP, 10 )
by_PEP[ 90:100, ]

# top 100 players false positive rate
top_players <- career[ 1:100, ]
sum(top_players$PEP)
mean(top_players$PEP)

# PEP's cumulative mean for meeting the false discovery rate
career[ , qvalue := cumsum(PEP) / 1:nrow(career) ]

# number of hall of famers for 5% false discovery rate
hall_of_fame <- career[ qvalue < .05, ]

# numbers of players included at various q-value cutoff
ggplot( career[ qvalue < .25, ], aes( qvalue, rank ) ) + geom_line() +
labs( x = "q-value cutoff", y = "Number of players included" )

# TODO :
# 1. read on confidence interval, v.s. credential interval
# 2. bayesian heirarchical modeling


