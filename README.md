# Business-Analytics

[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://github.com/ethen8181/Business-Analytics/blob/master/LICENSE)

This is one of the continuously updated repositories that documents personal journey on learning data science related topics. Currently, contents are organized into two separate repositories based on the following table's description.

| Repository | Documentation Focus |
| ---------- | ------------------- |
| [machine-learning](https://github.com/ethen8181/machine-learning) | Machine learning, algorithm and programming (mainly in Python) |
| [Business-Analytics](https://github.com/ethen8181/Business-Analytics) | All other data analytic related stuffs, e.g. concepts, statistics, visualizations (R, Python) |

Within each section, documentations are listed in reverse chronological order of the start date (the date when the first notebook in that folder was created, if the notebook happened to be updated, then the actual date will be at the top of each notebook). Each of them are independent of one another unless specified.


## Documentation Listings

#### cohort_analysis : 2016.04.01

- Visualize user retention by cohort with seaborn's heatmap.
- View [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/cohort_analysis/cohort_analysis.ipynb)]

#### finding_groups : 2015.11.10

Examples of how finding similar patterns using hierarchical clustering algorithms can be applied to the supply chain’s and human resource’s business field.

- Radar chart with ggradar.
- View [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/finding_groups/finding_groups.html)]


## Statistics

#### frequentist_statistics : 2016.07.27

- Notes for frequentist statistics inference (t-test, anova, proportion test, chi-square, power). [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/frequentist_statistics/frequentist_statistics.html)]
- Bonferroni correction for multiple hypothesis testing. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/frequentist_statistics/multiple_testing.ipynb)]
- Spearman rank correlation. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/frequentist_statistics/correlation.ipynb)]

#### bandits : 2016.06.02

Multi-armed Bandits Algorithms, a possible alternative to A/B testing for short-term tests or extremely long tests. For those that are not familiar with bayesian statistics, it's recommended to go through the first two documents in the [bayesian_statistics](#bayesian_statistics--20160421) folder.

- Epsilon Greedy, Softmax, Upper Confidence Bound and Thompson Sampling from scratch. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/bandits/bandits.ipynb)]

#### ab_tests : 2016.06.01

Includes Bayesian and Frequentist A/B testing. For those that are not familiar with bayesian statistics, it's recommended to go through all the documents in the **bayesian_statistics** folder.

- Bayesian A/B testing, beta heirarchical model with pymc. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/ab_tests/bayesian_ab_test.ipynb)]
- Frequentist A/B testing. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/ab_tests/frequentist_ab_test.html)]
- Template and caveats for the A/B testing process (applicable for both types of testing). [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/ab_tests/ab_test_template.ipynb)]

#### bayesian_statistics : 2016.04.21

For starters with bayesian statistics, read the documents in listed order.

- Bayes theorem basics. [[nbviewer](http://nbviewer.jupyter.org/github/ethen8181/Business-Analytics/blob/master/bayesian_statistics/bayes_basics.ipynb)]
- Beta distribution, empirical bayes estimation, credible interval and false discovery rate. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/bayesian_statistics/bayes/bayes.html)]
- Markov Chain Monte Carlo (MCMC) - Metropolis Hastings Algorithm. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/bayesian_statistics/MCMC/MCMC.html)]


## General

#### Articles

- Continuously updated non-technical articles. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/articles/articles.html)]
- 2017.01.17 | Data Science advice (mentality, problem solving and presentation template). [[Rmarkdown]](http://ethen8181.github.io/Business-Analytics/articles/data_science_advice.html)
- 2016.07.02 | Some ways of addressing data hygiene. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/articles/data_hygiene.html)]

#### Visualizations

- 2016.07.09 | Production ready calendar heatmap. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/articles/calendar_heatmaps/calendar_heatmaps.html)]
- 2016.05.12 | Production ready scatter plot. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/articles/nyt_scatter/nyt_scatter.html)]
- 2016.05.12 | Production ready faceted bar plot. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/articles/avoid_pie_charts/avoid_pie_charts.html)]

#### R

- 2017.01.13 | Unit testing and setting up a basic R package. [[Rmarkdown]](http://ethen8181.github.io/Business-Analytics/R/tests_packages/test.html)
- 2017.01.13 | Efficient (parallel) looping in R. [[Rmarkdown]](http://ethen8181.github.io/Business-Analytics/R/efficient_looping/efficient_looping.html)
- 2017.01.13 | Rmarkdown quickstart. [[Rmarkdown]](http://ethen8181.github.io/Business-Analytics/R/Rmarkdown/Rmarkdown.html)
- 2017.01.13 | data.table joining and other tricks. [[Rmarkdown](http://ethen8181.github.io/Business-Analytics/R/data_table/data_table.html)]

