# efficient looping

# -----------------------------------------------------------------------------------
# Parallel Programming
library(doParallel)

# we specify the number of cores/workers we want to use
n_cores <- detectCores() - 1
n_cores

# generate a toy function that
# simply generate the summary of a bunch of random numbers
summarize <- function(i) {
    summary( rnorm(100000) )
}

# the time difference between using n_cores and not using it
inputs <- 1:20
system.time({
    results <- mclapply(inputs, summarize, mc.cores = n_cores)
})

system.time({
    results <- lapply(inputs, summarize)
})


# overhead;
# when parallelizing trivial tasks
inputs <- 1:10000
system.time({
    results <- mclapply(inputs, sqrt, mc.cores = n_cores)
})

system.time({
    results <- lapply(inputs, sqrt)
})


# foreach;

# take a subset of the iris dataset, and train a bunch of
# logisitic regression on the same dataset
iris_subset <- iris[iris$Species != 'setosa',]
iris_subset$Species <- factor( iris_subset$Species, 
                               levels = c('versicolor', 'virginica') )
# number of models to train
trials <- 1000

# we first register a backend specifying the number
# of cores that we wish to use
registerDoParallel(cores = detectCores() - 1)

# parllelized foreach
system.time({
    result <- foreach(1:trials) %dopar% {
        model <- glm(Species ~., data = iris_subset, family = 'binomial')
    }
})

# foreach also allows us to run things in a serial manner,
# to do so, we simply change the dopar to do
system.time({
    result <- foreach(1:trials) %do% {
        model <- glm(Species ~., data = iris_subset, family = 'binomial')
    }
})

# by default foreach returns a list
head(result, 2)

# we can specify how we want it to
# combine the result through the .combine 
# argument, say we want it to return a vector
result <- foreach(1:trials, .combine = c) %dopar% {
    model <- glm(Species ~., data = iris_subset, family = 'binomial')
    deviance <- model$deviance
    return(deviance)
}


# modifying global state;

# create a vector of 0s
# and modifying the element by
# multiplying every index by 2
x1 <- rep(0, times = 5)
for(i in 1:5) {
    x1[i] <- i * 2
}

# we try to do the same
# thing in a foreach loop
x2 <- rep(0, times = 5)
foreach(i = 1:5) %dopar% {
    x2[i] <- i * 2
}

# the two different results
list(serial = x1, parallel = x2)

# the way to fix this, is simply return what
# we actually want
x3 <- foreach(i = 1:5, .combine = c) %dopar% {
    i * 2
}
x3


# nested for loops;

# simple simulation function
sim <- function(a, b) {
    return(10 * a + b)
}

# loop over all combinations of the values that
# are stored in the two vectors, avec, bvec to 
# create the simulated data
avec <- 1:2
bvec <- 1:4

# for loop way
x <- matrix(0, length(avec), length(bvec))
for(j in 1:length(bvec)) {
    for(i in 1:length(avec)) {
        x[i, j] <- sim(avec[i], bvec[j])
    }
}
x

# foreach way, notice we don't
# put braces around the inner foreach loop
x <- foreach(b = bvec, .combine = 'cbind') %:%
    foreach(a = avec, .combine = 'c') %dopar% {
        sim(a, b)
    }
x

# remember to explicity stop the cluster
stopImplicitCluster()


# -----------------------------------------------------------------------------------
# Iterators

# note that the iterators library is
# also loaded when we load the doParallel library
library(iterators)
library(itertools)

# create a iterator of length 2,
# Here iteration is performed manually by calling nextElem()
# on the iterator, each call yields the next element in sequence
iter_count <- icount(2)
nextElem(iter_count)
nextElem(iter_count)

# when the sequence has been exhausted, we'll get a StopIteration error
# nextElem(iter_count)

# we can also create iterators from an existing sequence
name <- c('Bob', 'Mary')
iter_name <- iter(name)
nextElem(iter_name)
nextElem(iter_name)

# wrap the iterator with the ihasNext function,
# while the sequence has not been exhausted, print
# the next element
iter_count <- ihasNext( icount(5) )
while( hasNext(iter_count) ) {
    print( nextElem(iter_count) )
}

# iterators with apply
# using the multiplying each index by 2 as an example
x1 <- vapply( icount(5), function(i) {
    return(i * 2)
}, numeric(1) )

# iterators with foreach
x2 <- foreach(i = icount(5), .combine = c) %dopar% {
    return(i * 2)
}
list(apply_way = x1, foreach_way = x2)

