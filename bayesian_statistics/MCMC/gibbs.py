
"""

Gibbs Sampling
The MCMC method we've introduced above is also known as the *Metropolis-Hastings algorithm*. It is very general and broadly applicable, but one drawback is that your proposed distribution has to be properly tuned to the posterior distribution. Recall that we proposed a new random sample point using the a normal distribtuion with the mean of 1 and standard deviation of 1 `rnorm( ..., mean = 0, sd = 1 )`. If this random sample point generator's distribution is too wide or too narrow, a large proportion of our proposed sample point will still be rejected. Or more intuitively, the effectiveness of our random walk will farely low.
Given this situation, there's an alternative sampling algorithm called **Gibbs Samplinig**. Unlike the **Metropolis Sampling**, with **Gibbs Samplinig**  all proposed jumps are accepted.

Takeaways
If you can decompose your probability distribution into conditional probabilities, then **Gibbs Samplinig** is preferable over **Metropolis Sampling**.


Gibbs sampling with for uninitiated (paper), with latent dirichlet allocation
http://www.umiacs.umd.edu/~resnik/pubs/gibbs.pdf
https://theclevermachine.wordpress.com/2012/11/19/a-gentle-introduction-to-markov-chain-monte-carlo-mcmc/
http://nbviewer.jupyter.org/github/cs109/content/blob/master/labs/lab7/GibbsSampler.ipynb

"""
