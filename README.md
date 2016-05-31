# Business-Analytics

This is one of the continuously updated repositories that documents personal data science journey. Currently, contents are organized into two separate repositories based on the following table's description.

| Repository | Documentation Focus |
| ---------- | ----------- |
| [machine-learning](https://github.com/ethen8181/machine-learning) | Machine learning and programming in R / python. |
| [Business-Analytics](https://github.com/ethen8181/Business-Analytics) | All other data analytic related stuffs, e.g. concepts, statistics, articles, visualizations. |

Within each section, documentations are listed in reverse chronological order of the latest complete date and each of them are independent of one another unless specified.


## General Analytics


**cohort_analysis : 2016.4.1**

- Visualize user retention cohort analysis with *seaborn's* heatmap.
- View [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/cohort_analysis/cohort_analysis.ipynb)]


**marketing_analysis :**  ( on hold : add customer lifetime value )

Conducting customer segmentations using RFM ( Recency, Frequency, Monetary Value ) method.

- View [[R markdown](http://ethen8181.github.io/Business-Analytics/marketing_analysis/marketing_analysis.html)]


**finding_groups : 2015.11.10** 

Examples of how finding similar patterns using hierarchical clustering algorithms can be applied to the supply chain’s and human resource’s business field.

- Radar chart with ggradar.
- View [[R markdown](http://ethen8181.github.io/Business-Analytics/finding_groups/finding_groups.html)]


## Statistics

**bandits : 2016.6.2**

Multi-armed bandits algorithms, a possible alternative to ab test in short tests or extremely long tests. For those that are not familiar with bayesian statistics, it's recommended to go through the first two documents in the **bayesian_statistics** folder.

- Epsilon Greedy, Softmax, Upper Confidence Bound and Thompson Sampling from scratch. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/bandits/bandits.ipynb)]


**ab_tests : 2016.6.1**

Includes Bayesian and Frequentist A/B testing. For those that are not familiar with bayesian statistics, it's recommended to go through all the documents in the **bayesian_statistics** folder.

- Bayesian AB testing, beta heirarchical model with pymc. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/ab_tests/bayesian_ab_test.ipynb)]
- Frequentist AB testing. [[R markdown](http://ethen8181.github.io/Business-Analytics/ab_tests/frequentist_ab_test.html)]
- Template and caveats for the A/B testing process (applicable for both types of testing). [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/ab_tests/ab_test_template.ipynb)]


**bayesian_statistics : 2016.4.21**

For starters with bayesian statistics, read the documents in listed order.

- Bayes theorem basics. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/bayesian_statistics/bayes_basics.ipynb)]
- Beta distribution, empirical bayes estimation, credible interval and false discovery rate. [[R markdown](http://ethen8181.github.io/Business-Analytics/bayesian_statistics/bayes/bayes.html)]
- Markov Chain Monte Carlo (MCMC) - Metropolis Hastings Algorithm. [[R markdown](http://ethen8181.github.io/Business-Analytics/bayesian_statistics/MCMC/MCMC.html)]


## Articles & Visualizations

- 2015.5.18 | Template for problem solving and presentations. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/articles/logic_workflow.ipynb)]
- 2016.5.12 | Production ready scatter plot. [[R markdown](http://ethen8181.github.io/Business-Analytics/articles/nyt_scatter/nyt_scatter.html)]
- 2016.5.12 | Production ready faceted bar plot. [[R markdown](http://ethen8181.github.io/Business-Analytics/articles/avoid_pie_charts/avoid_pie_charts.html)]
- Continuously updated articles link stored for future reference. [[R markdown](http://ethen8181.github.io/Business-Analytics/articles/articles.html)]

