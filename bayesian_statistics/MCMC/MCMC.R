# monte carlo markov chain
library(ggplot2)
library(ggthemes)

f <- function(x){
	( sin(.2 * x^3) + sin(x^2) / 5 + 2.5 + sin(10 * x) / 2 ) * dnorm( x, mean = 1 )
}

# # boundary for the x axis
x_max <- 6
x_min <- -6

# for simply plotting the function
ggplot( data.frame( x = c( x_max, x_min  ) ), aes(x) ) + 
stat_function( fun = f ) + 

x <- seq( x_min, x_max, length.out = 500 )
y <- f(x)
data <- data.frame( x = x, y = y )

ggplot( data, aes( x, y ) ) + geom_line() + 
geom_ribbon( aes( ymin = 0, ymax = y ), alpha = 0.2 )


# -------------------------------------------------
#   monte carlo rejection sampling
# -------------------------------------------------

# throw N darts (uniformly distributed x-y pairs) at the plot above
N <- 10000
ceiling <- 1.5
xs <- runif( N, min = x_min, max = x_max )
ys <- runif( N, min = 0, max = ceiling )

# accept any samples that are below the function curve
accepted_samples <- xs[ ys <= f(xs) ]

ggplot( data.frame( x = accepted_samples ) ) + 
geom_histogram( aes( x = x, y = ..density.. ) ) + 
geom_line( data = data, aes( x, y ) )

# study the samples however you want
# mean(accepted_samples)
# quantile(accepted_samples, probs = seq(0, 1, length = 4 ) )

# accepted and unaccepted points
df <- data.frame( xs = xs, ys = ys, accept = ys <= f(xs) )

# plot the accepted and unaccepted point
ggplot( df, aes( xs, ys ) ) + 
geom_point( aes( color = accept ), size = 0.5 ) + 
geom_line( data = data, aes( x, y ) )

# -------------------------------------------------
#   markov chain monte carlo 
# -------------------------------------------------

lik <- function( x, y ){
	dnorm(x - 3) * dnorm(y - x + 2)
}

grid_values <- seq( x_min, x_max, length = 500 )
grid <- expand.grid( x = grid_values, y = grid_values )
z <- lik( grid$x, grid$y )

# prior probability plot
plot <- ggplot( data = grid, aes( x = x, y = y ) ) + 
		geom_raster( aes( fill = z ) ) + 
		scale_fill_gradient2() + coord_equal() +
		theme_tufte()
plot


MCMC <- function( iter, burnin = 0, thin = 1 )
{
	# the list of the first argument for dimnames is to specify the row names,
	# sampling starts at a random point, here 0, 0 
	samples <- matrix( NA, nrow = iter, ncol = 2, 
					  dimnames = list( NULL, c( "x", "y" ) ) )
	samples[ 1, ] <- c( 0, 0 )

	for(i in 2:iter){

		sample <- samples[ i - 1, ]
		
		for(j in 1:thin){
			
			# propose a new sample point			
			proposal <- sample + rnorm( n = 2, mean = 0, sd = 1 )

			# compare its likelihood with the current position
			lik_old <- lik( sample["x"], sample["y"] )
			lik_new <- lik( proposal["x"], proposal["y"] )
			ratios  <- lik_new / lik_old

			# flip a coin and accept the 
			# new proposal with probability min( ratio, 1 ),
			# if you don't accept the proposal,
			# then just keep what you had in the last step,
			# meaning nothing changed.
			# note that 1 is evaluated as TRUE
			if( rbinom( 1, size = 1, prob = min( ratios, 1 ) ) )
				sample <- proposal
		}
		samples[ i, ] <- sample
	}

	return( data.frame( samples[ (burnin + 1):iter, ] )	)
}

TracePlot <- function( plot, samples ){
	plot + 
	geom_path( data = samples, aes( x, y ), color = "orange" ) + 
	geom_point( data = samples, aes( x, y ), size = 1 )
}

# plot that shows the first few sampling points are not good ones
samples <- MCMC(iter = 50)
TracePlot( plot, samples )

# plot with burnin, discard the bad sampling points in the beginning
samples <- MCMC( iter = 250, burnin = 125 )
TracePlot( plot, samples )

# use thin to avoid correlation and
# explore more of the sample space
samples <- MCMC( iter = 250, burnin = 125, thin = 2 )
TracePlot( plot, samples )


