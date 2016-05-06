# %matplotlib inline
import numpy as np
from scipy.stats import beta
import matplotlib.pylab as plt
from collections import namedtuple


class BetaBandit(object):
	"""
	Thompson Sampling
	
	Parameters
	----------
	K : int
		total number of arms
	
	prior_params : list of float length 2 tuple, default None, (optional)
		each element of the list is a tuple, where each tuple
		contains the alpha and beta parameter that represents the prior
		beta distribution for each arm. If not supplied
		it will assume that all arms's prior starts with an uniform distribution
    
	Attributes
	----------
	trials, success : int 1d-array, shape( K, )
		stores the trials and success for each arm,
		e.g. trial = [ 1, 1 ] and success = [ 0, 1 ] means 
		that both arm has been pulled once and arm 1 has generated
		the reward (clicked)
	"""

	def __init__( self, K, prior_params = None ):
		if prior_params:
			priors = namedtuple( "priors", [ "alpha", "beta" ] )
			prior  = [ priors(*p) for p in prior_params ]
			self.alphas = np.asarray([ p.alpha for p in prior ])
			self.betas  = np.asarray([ p.beta  for p in prior ])
		else:
			self.alphas = np.ones(K)
			self.betas  = np.ones(K)

		self.trials  = np.zeros( K, dtype = int )
		self.success = np.zeros( K, dtype = int )
		
	
	def get_recommendation(self):
		"""
		for all arms, construct their beta distribution and
		draw a random sample from it, then return the arm
		with the maximum random sample
		"""
		theta = np.random.beta( self.alphas + self.success, 
								self.betas + self.trials - self.success )
		return np.argmax(theta)
	
	
	def add_result( self, arm, converted ):
		"""
		override the trials and success array, the success array
		will only be updated if it has generated a reward
		"""
		self.trials[arm] += 1
		if converted:
			self.success[arm] += 1


def experiment( T, K = None, ctr = None, prior_params = None ):
	"""
	run the experiment for Thompson Sampling.
	You need to supply the ctr, the fixed ctr for each arm
	or K, the total number of arms to run the experiment,
	if K is supplied then it will be randomly generated
	
	Parameters
	----------
	T : int
		number of simulation in an experiment
	
	K : int, , default = None, (optional)
		total number of arms
		
	ctr : float sequence, len = K, default = None, (optional)
		the empirical click through rate for each arm
		
	prior_params : list of float length 2 tuple, default None, (optional)
		each element of the list is a tuple, where each tuple
		contains the alpha and beta parameter that represents the prior
		beta distribution for each arm. If not supplied
		it will assume that all arms's prior starts with an uniform distribution
	
	Returns
	-------
	ctr : float sequence, len = K
		the supplied or the randomly generated ctr
	
	trials, success : 2d-array, shape( T, K )
		trials and success recorded for each turn of the experiment
		
	alphas, betas : float 1d-array, shape( K, )
		the alpha and beta parameters for each arm
	"""
	if ctr:
		K = len(ctr)
	else:
		ctr = np.random.rand(K)

	trials  = np.zeros( ( T, K ), dtype = int )
	success = np.zeros( ( T, K ), dtype = int )

	bb = BayesianBandit( K, prior_params )
	for t in range(T):
		arm = bb.get_recommendation()
		converted = np.random.rand() < ctr[arm]
		bb.update_result( arm, converted )
		trials[t]  = bb.trials
		success[t] = bb.success

	return ctr, trials, success, bb.alphas, bb.betas


def experiment_plot( ctr, trials, success ):
	"""
	Pass in the ctr, trials and success returned
	by the `experiment` function and plot
	the Cumulative Number of Turns For Each Arm and
	the CTR's Convergence Plot side by side
	"""
	T, K = trials.shape
	n = np.arange(T) + 1
	fig = plt.figure( figsize = ( 14, 7 ) )

	plt.subplot(121)	
	for i in range(K):
		plt.loglog( n, trials[ :, i ], label = "arm {}".format(i + 1) )

	plt.legend( loc = "upper left" )
	plt.xlabel("Number of turns")
	plt.ylabel("Number of turns/arm")
	plt.title("Cumulative Number of Turns For Each Arm")

	plt.subplot(122)
	for i in range(K):
		plt.semilogx( n, np.zeros(T) + ctr[i], label = "arm {}'s CTR".format( i + 1 ) )

	plt.semilogx( n, ( success[ :, 0 ] + success[ :, 1 ] ) / n, label = "CTR at turn t" )

	plt.axis([ 0, T, 0, 1 ] )
	plt.legend( loc = "upper left" )
	plt.xlabel("Number of turns")
	plt.ylabel("CTR")
	plt.title("CTR's Convergence Plot")

	return fig


def plot_beta_dist( ctr, trials, success, alphas, betas, turns ):
	"""
	Pass in the ctr, trials and success, alphas, betas returned
	by the `experiment` function and the number of turns 
	and plot the beta distribution for all the arms in that turn
	"""
	subplot_num = len(turns) / 2
	x = np.linspace( 0.001, .999, 200 )
	fig = plt.figure( figsize = ( 14, 7 ) ) 

	for idx, turn in enumerate(turns):

		plt.subplot( subplot_num, 2, idx + 1 )

		for i in range( len(ctr) ):
			y = beta( alphas[i] + success[ turn, i ], 
					  betas[i] + trials[ turn, i ] - success[ turn, i ] ).pdf(x)
			line = plt.plot( x, y, lw = 2, label = "arm {}".format( i + 1 ) )
			color = line[0].get_color()
			plt.fill_between( x, 0, y, alpha = 0.2, color = color )
			plt.axvline( x = ctr[i], color = color, linestyle = "--", lw = 2 )
			plt.title("Posteriors After {} turns".format(turn) )
			plt.legend( loc = "upper right" )

	return fig


if __name__ == "__main__":
	# number of simulation in an experiment
	T = 10000

	# the empirical click through rate for each arm
	ctr = ( 0.25, 0.35 )

	ctr, trials, success, alphas, betas = experiment( T = T, ctr = ctr )
	fig = experiment_plot( ctr, trials, success )
	fig.show()

	turns = [ 1, 100, 1000, 9999 ]
	posterior_fig = plot_beta_dist( ctr, trials, success, alphas, betas, turns )
	posterior_fig.show()


