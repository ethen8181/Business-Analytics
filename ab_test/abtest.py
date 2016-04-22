import pymc as pm
import numpy as np
import matplotlib.pyplot as plt
from pymc.Matplot import plot as mcplot


def acf( x, lag = 100 ):
    """
    autocorrelation function that calculates the 
    autocorrelation for series x from 1 to the user-specified lag
    
    Parameter
    ---------
    x : 1d-array, size ( iteration - burn-in iteration, )
        pymc's random sample
        
    lag : int, default 100
        maximum lagging number
        
    Return
    ------
    corr : list, size (lag)
        autocorrelation for each lag
    
    Reference
    ---------
    http://stackoverflow.com/questions/643699/how-can-i-use-numpy-correlate-to-do-autocorrelation
    """
    # np.corrcoef here returns a 2 * 2 matrix, either [ 0, 1 ] or [ 1, 0 ]
    # will be the actual autocorrelation between the two series,
    # the diagonal is just the autocorrelation between each series itself
    # which is 1
    corr = [ np.corrcoef( x[k:], x[:-k] )[ 0, 1 ] for k in range( 1, lag ) ]
    return corr


def effective_sample( iteration, burnin, corr ):
    """
    calculate the effective sample size of the mcmc,
    note that the calculation is based on the fact that
    the autorrelation plot appears to have converged
    
    Parameters
    ----------
    iteration : int
        number of iteration of the mcmc
    
    burnin : int
        burn-in iteration of the mcmc
    
    corr : list
        list that stores the autocorrelation for different lags
    
    Returns
    -------
    the effective sample size : int
    """
    for index, c in enumerate(corr):
        if c < 0.05:
            i = index
            break
    
    return ( iteration - burnin ) / ( 1 + 2 * np.sum( corr[:i + 1] ) )


@pm.stochastic( dtype = np.float64 )
def beta_priors( value = [ 1.0, 1.0 ] ):  
    a, b = value
    # outside of the support of the distribution
    if a <= 0 or b <= 0:
        return -np.inf
    else:
        return np.log( np.power( ( a + b ), -2.5 ) )


def bayesian_ab_test( beta_priors, trials, successes
					  iteration, burnin, thin ):
	"""
	bayesian ab test using beta hierarchical models

	Parameters
	----------
	beta_priors : pm.stochastic decorated function
		hierarchical model's beta prior

	trials : 1d-array, size ( number of test that were ran, )
		the observed number of trials (e.g. visitors)

	successes : 1d-array, size ( number of test that were ran, )
		the observed number of success or actions (e.g. clicks and orders)

	iteration : int
		mcmc sample iteration

	burnin : int
		mcmc burn-in iteration

	thin : int
		mcmc thinning iteration

	Returns
	-------
	mcmc : pymc model
		the pymc model that has been fitted,
		our parameter of interest is named as 'true_rates'

	Examples
	--------
	iteration = 70000
	burnin = 10000

	trials = np.array([ 1055, 1057, 1065, 1039, 1046 ])  
	successes = np.array([ 28, 45, 69, 58, 60 ])

	mcmc = bayesian_ab_test( beta_priors, trials, successes, iteration, burnin )
	mcplot( mcmc.trace("true_rates"), common_scale = False )
	"""
	
	# intialize parameters
	a = beta_priors[0]
	b = beta_priors[1]
	true_rates = pm.Beta( 'true_rates', a, b, size = trials.shape[0] )
	observed_values = pm.Binomial( 'observed_values', trials, true_rates, 
	                               value = successes, observed = True )
	# fit the model
	model = pm.Model([ a, b, true_rates, observed_values ])  
	mcmc  = pm.MCMC(model)
	pm.MAP(model).fit()
	mcmc.sample( iteration, burnin, thin )
	return mcmc

