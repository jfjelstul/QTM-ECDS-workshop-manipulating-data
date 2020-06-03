###########################################################################
# Josh Fjelstul, Ph.D.
# QTM-ECDS Workshops, Coding in R Series
# Intermediate R
###########################################################################

# libraries
library(plyr) # always load plyr before dplyr
library(dplyr)
library(stringr)
library(ggplot2)
library(magrittr)
library(foreach)
library(parallel)
library(doParallel)
library(doSNOW)

###########################################################################
###########################################################################
# piping (migrittr)
###########################################################################
###########################################################################

# what is chaining?
# why do it? code is easier to read

# sequence
# x < f(x)
# x <- g(x)

# nested
# x <- g(f(x))

# pipes
# x <- x %>% f() %>% g()

# can pass through arguments
# f(x, y) becomes x %>% f(y)

# can do this many times
# x <- h(g(f(x)))
# x <- x %>% f() %>% g() %>% h()

# make some fake data
x <- rnorm(10, 0, 1)

# we could do each step separately, saving the output of each step in an object
z <- sin(x)
z <- cos(z)
z <- round(z, 3)
z

# or we could nest the functions
round(cos(sin(x)), 3)

# this can be hard to read

# piping lets us specify the operations sequentially 
# this is usually more intuitive and much easier to read

# you can use parentheses
x %>% sin() %>% cos() %>% round(3)

# or omit them
x %>% sin %>% cos %>% round(3)

###########################################################################
# custom single-variable functions
###########################################################################

# suppose we have a custom function that does an operation

# we can use a named argument
f <- function(x) {
  round(cos(sin(x)), 3)
}

# run the function
f(x)

# or we can use an unnamed argument if there is only one
f <- function(.) {
  round(cos(sin(.)), 3)
}

# run the function
f(x)

# or we can write a unary function using pipes
# the object f is a pipeline
f <- . %>% sin %>% cos %>% round(3) 

# run the function
x %>% f

# we can do the same thing with parantheses to improve readability
f <- . %>% sin() %>% cos() %>% round(3) 

# run the function
x %>% f()

# usually pipes are better than doing operations in a sequence and saving the intermediate steps 
# or nesting operations

###########################################################################
# the compound assignment operator
###########################################################################

# usually we assign the output of a function to an object using the assignment operator (<-)
z <- x %>% sin() %>% cos() %>% round(3)

# if we're over-writing an object we can use the compound assignment operator instead

# duplicate our simulated data
z <- x

# apply a pipline
z %<>% sin() %>% cos() %>% round(3)

# this way we don't have to specify the name of our object twice

###########################################################################
# the exposition operator
###########################################################################

# some functions don't have data arguments

# for example, cor() needs x and y variables

# so if we want to pass individual variables from a data frame to 
# a function using pipes, we need to use the exposition operator %$%

# it exposes the names in the data frame on the LHS to the function of the RHS

# generate fake data
x <- rnorm(100, 0, 1)
y <- 1 + 3 * x + runif(100, 0, 5)

# normal way
cor(x, y)

# using a pipeline that makes a data frame first
data.frame(x, y) %$%
  cor(x, y)

###########################################################################
# performance considerations
###########################################################################

# what are reasons not to use pipes?

# if one of the functions is really slow, you may want an intermediate object
# so you don't have to re-run all the code every time

# if you're using functions that output multiple objects in a list
# if this is the case, you need to make sure subsequent functions can handle the list as input

# use Sys.time() to see how long an operation takes

# generate data
x <- runif(10, 0, 1)

# sequence
t0 <- Sys.time()
z <- sin(x)
z <- cos(z)
z <- round(z, 3)
z
t1 <- Sys.time()
t_seq <- t1 - t0

# nesting
t0 <- Sys.time()
round(cos(sin(x)), 3)
t1 <- Sys.time()
t_nest <- t1 - t0

#  piping
t0 <- Sys.time()
x %>% sin() %>% cos() %>% round(3)
t1 <- Sys.time()
t_pipe <- t1 - t0

# compare times
t_seq
t_nest
t_pipe

# piping is usually slower than nesting but faster than doing operations in sequence

###########################################################################
###########################################################################
# pipes
###########################################################################
###########################################################################

# dplyr has 5 main functions that you can apply directly to a data frame
# select()
# rename()
# filter()
# arrange()
# mutate()

# dplyr functions are faster than base R functions
# and they all use a similar syntax

# sample data on 2016 House races
# https://www.nytimes.com/elections/results/house
# only includes contested races

# set your working directory to the root folder for this workshop

# load data
dat <- read.csv("data/2016-election.csv", stringsAsFactors = FALSE)

###########################################################################
# select()
###########################################################################

# select() lets you choose variables

# you do not need to put variable names in quotes
out <- select(dat, state, district, expectation)

# you can use a minus sign to drop variables
out <- select(dat, -expectation)

# you can also drop multiple variables 
out <- select(dat, -c(democrat, republican))

# suppose you want to move a variable to the left side and keep the rest in the same order

# you can't just specify that variable, because then select() will drop all the rest
out <- select(dat, expectation)

# one option is to specify every variable
out <- select(dat, expectation, state, district, democrat, republican)

# but the better way is to use everything()
out <- select(dat, expectation, everything())

# dplyr provides a number of other helper functions
# starts_with()
# ends_with()
# contains()
# matches()
# one_of()

###########################################################################
# rename()
###########################################################################

# rename() lets you rename variables
# the new name comes FIRST
out <- rename(dat, likely_outcome = expectation)

# you can rename multiple variables at the same time
out <- rename(dat, democrat_vote_share = democrat, republican_vote_share = republican)

###########################################################################
# filter()
###########################################################################

# filter() lets you select observations using logical statements
out <- filter(dat, state == "Alabama")
out <- filter(dat, state %in% c("Georgia", "Alabama", "South Carolina", "Mississippi"))
out <- filter(dat, republican > 50)
out <- filter(dat, republican > 50 & expectation == "Tossup seat")
out <- filter(dat, democrat < 53 & democrat > 47)

###########################################################################
# arrange()
###########################################################################

# arrange() lets you sort observations based on different variables
out <- arrange(dat, state)
out <- arrange(dat, expectation, republican)

# you can use desc() to sort a variable in descending order
out <- arrange(dat, desc(state))

###########################################################################
# mutate()
###########################################################################

# mutate() lets you make new variables

# make a dummy variable for whether the Democrat got over 50%

# normal way
out <- dat
out$democrat_majority <- as.numeric(out$democrat > 50)

# using mutate()
out <- dat
out <- mutate(out, democrat_majority = as.numeric(out$democrat > 50))

###########################################################################
###########################################################################
# pipes and dplyr
###########################################################################
###########################################################################

###########################################################################
# question 1
###########################################################################

# in which districts was the Democrat win margin the greatest?
out <- dat %>% 
  mutate(margin = democrat - republican) %>% 
  arrange(desc(margin)) %>% 
  select(state, district, margin)

###########################################################################
# question 2
###########################################################################

# in districts where the Democrat was favored, who under-performed the most?
table(dat$expectation)
out <- dat %>% filter(expectation %in% c("Democrat expected to win easily", "Democrat expected to win narrowly")) %>%
  mutate(margin = democrat - republican) %>% 
  arrange(margin) %>% 
  select(state, district, margin)

###########################################################################
# question 3
###########################################################################

# in which states did Republicans and Democrats do equally well, in terms of seats won? 
# in summarize(), you can refer to variables you've created in previous lines
out <- dat %>% 
  group_by(state) %>% 
  summarize(percent_democrat = mean(democrat > 50),
            difference = abs(percent_democrat - 0.5)) %>%
  arrange(difference)

###########################################################################
# question 4
###########################################################################

# how many districts are there in each state?
out <- dat %>% 
  group_by(state) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# how can we plot this?
ggplot(out) + 
  geom_bar(aes(x = state, y = count), stat = "identity") + 
  coord_flip()

# we can including the plot in the pipeline
dat %>% 
  group_by(state) %>%
  summarize(count = n()) %>%
  ggplot() + 
  geom_bar(aes(x = state, y = count), stat = "identity") + 
  coord_flip()

# but what if we want the states in descending order?
# then we have to adjust the order of the factor

# run the pipeline
out <- dat %>% 
  group_by(state) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# fix the order
out$state <- factor(out$state, levels = out$state[order(out$count)])

# make the plot
ggplot(out) + 
  geom_bar(aes(x = state, y = count), stat = "identity") + 
  coord_flip()

# this is a case where using a pipe to make a plot isn't the best option

###########################################################################
# question 5
###########################################################################

# in which states do Democrats win by the biggest margins?
out <- dat %>% 
  group_by(state) %>% 
  summarize(margin = mean(democrat - republican)) %>% 
  arrange(desc(margin))

# fix the order
out$state <- factor(out$state, levels = out$state[order(out$margin)])

# plot 
ggplot(out) +
  geom_point(aes(x = margin, y = state), size = 2) + 
  geom_vline(xintercept = 0, size = 0.5, linetype = "dashed")

###########################################################################
# question 6
###########################################################################

# which Democrats most out-performed other Democrats in their state?
# you can group and ungroup data in the same pipeline
out <- dat %>% 
  group_by(state) %>% 
  mutate(mean_margin = mean(democrat - republican)) %>% 
  ungroup() %>%
  mutate(margin = democrat - republican,
         difference = margin - mean_margin) %>%
  arrange(desc(difference)) %>%
  select(state, district, margin, mean_margin, difference)

###########################################################################
###########################################################################
# vectorized functions, loops, split-apply-combine operations
###########################################################################
###########################################################################

# loops are useful when you need to make a variable that draws on data from multiple data frames
# they are also helpful when the operation you need to apply is not vectorized
# vectorized functions are functions that a vector as input a return a vector as output
# sometimes we need to run an operation on a variable that takes one input and returns one output

# import data to work with
cases <- read.csv("data/cases.csv", stringsAsFactors = FALSE)
judges <- read.csv("data/judges.csv", stringsAsFactors = FALSE)

###########################################################################
# function to apply
###########################################################################

# function to calculate the experience of judges in a chamber
# this function is not vectorized
experience <- function(x) {
  
  # split list of judges into a vector of names (returns a list)
  x <- str_split(x, ", ")
  
  # coerce the list to a vector
  x <- unlist(x)
  
  # get the start years of the judges
  x <- judges$start_year[judges$last_name %in% x]
  
  # calculate experience
  x <- mean(2015 - x)
  
  # return output
  return(x)
}

# add a built-in delay
experience_delay <- function(x) {

  # calculate experience
  x <- experience(x)
    
  # pause for 0.25 seconds
  Sys.sleep(0.25)
  
  # return output
  return(x)
}

###########################################################################
# for loops
###########################################################################

# example 1
# output to vector

out <- rep(NA, 15)
for(i in 1:15) {
  out[i] <- experience(cases$judges[i])
}

# example 2
# you can name the counter whatever you want

out <- rep(NA, 15)
for(j in 1:15) {
  out[j] <- experience(cases$judges[j])
}

# example 3
# but you do have to create an empty vector

rm(out)
for(i in 1:15) {
  out[i] <- experience(cases$judges[i])
}

# Error: object 'out' not found

# example 4
# you can store the output as a list
# you don't have to specify the length of the list ahead of time

out <- list()
for(i in 1:15) {
  out[[i]] <- experience(cases$judges[i])
}
out <- unlist(out)

# example 5
# you can get the number of iterations from the length of a vector

out <- rep(NA, length(cases$judges))
for(i in 1:length(cases$judges)) {
  out[i] <- experience(cases$judges[i])
}

# example 6
# you can store the range in an object

range <- 1:15
out <- rep(NA, length(range))
for(i in range) {
  out[i] <- experience(cases$judges[i])
}

# example 7
# you can get the number of iterations from the rows in a data frame

for(i in 1:nrow(cases)) {
  cases$experience[i] <- experience(cases$judges[i])
}

# example 8
# you can add a progress bar
bar <- txtProgressBar(max = nrow(cases), style = 3)
for(i in 1:nrow(cases)) {
  cases$experience[i] <- experience_delay(cases$judges[i])
  setTxtProgressBar(bar, i)
}

###########################################################################
# foreach loops
###########################################################################

# for each loops can run in any order, so only use them if the iterations are completely independent

# example 1
# by default, foreach() returns a list
out <- foreach(i = 1:nrow(cases)) %do% experience(cases$judges[i])

# example 2
# we can use the .combine argument to unlist it
out <- foreach(i = 1:nrow(cases), .combine = c) %do% experience(cases$judges[i])
cases$experience <- foreach(i = 1:nrow(cases), .combine = c) %do% experience(cases$judges[i])

# example 3
# we can tell foreach() how to deal with errors
# you can define a custom function in the same step as the loop
x <- rnorm(10, 0, 2)
f <- function(x) {
  if (x < -1) {
    stop("error: x is too small")
  } else if (x > 1){
    stop("error: x is too large")
  } else {
    return(x)
  }
}
# this will stop after the first error:
out <- NULL
out <- foreach(i = 1:length(x), .combine = c, .errorhandling = "stop") %do% f(x[i])
# this will skip iterations if there is an error:
out <- NULL
out <- foreach(i = 1:length(x), .combine = c, .errorhandling = "remove") %do% f(x[i])
# this will pass through the errors so we can see what went wrong:
out <- NULL
out <- foreach(i = 1:length(x), .combine = c, .errorhandling = "pass") %do% f(x[i])

# example 4
# foreach() will tell you if there are warnings
x <- rnorm(10, 0, 2)
out <- foreach(i = 1:length(x), .combine = c, .errorhandling = "stop") %do% {
  sqrt(x[i])
}

# example 5 
# we can set up a progress bar
bar <- txtProgressBar(max = nrow(cases), style = 3)
cases$experience <- foreach(i = 1:nrow(cases),
                            .combine = c) %do% {
                              setTxtProgressBar(bar, i) # it's important that you update the progress bar first
                              experience_delay(cases$judges[i])
                            }

###########################################################################
# split-apply-combine operations
###########################################################################

# lapply() returns a list
out <- lapply(cases$judges, experience)

# if we try to make a variable directly, it'll be of class "list" instead of class "numeric"
# DON'T DO THIS
cases$experience <- lapply(cases$judges, experience)
class(cases$experience)

# we can unlist the list to make a variable in a data frame
cases$experience <- NULL
cases$experience <- unlist(lapply(cases$judges, experience))
class(cases$experience)

# alply() returns a list
# use .margin to "loop" overs rows (.margin = 1) or columns (.margin = 2) 
out <- alply(.data = cases$judges, .margin = 1, .fun = experience)
out <- unlist(out)

# aaply() returns a vector
# use .margin to "loop" overs rows (.margin = 1) or columns (.margin = 2) 
cases$experience <- NULL
cases$experience <- aaply(.data = cases$judges, .margin = 1, .fun = experience)

# adding a progress bar
cases$experience <- NULL
cases$experience <- aaply(.data = cases$judges, .margin = 1, .fun = experience_delay, .progress = "text")

# plyr functions are nice because they're faster than for loops

# for()
t0 <- Sys.time()
out <- rep(NA, nrow(cases))
for(i in 1:nrow(cases)) {
  out[i] <- experience(cases$judges[i])
}
t1 <- Sys.time()
t_for <- t1 - t0

# foreach()
t0 <- Sys.time()
out <- foreach(i = 1:nrow(cases), .combine = c) %do% experience(cases$judges[i])
t1 <- Sys.time()
t_foreach <- t1 - t0

# lapply()
t0 <- Sys.time()
out <- lapply(cases$judges, experience)
t1 <- Sys.time()
t_lapply <- t1 - t0

# aaply()
t0 <- Sys.time()
out <- aaply(.data = cases$judges, .margin = 1, .fun = experience)
t1 <- Sys.time()
t_aaply <- t1 - t0

# compare times
t_for
t_foreach
t_lapply
t_aaply

###########################################################################
###########################################################################
# parallel computing
###########################################################################
###########################################################################

# if we use serial processing, and we have n operations that each take s seconds,
# the code with take n * s seconds

# if we use parallel processing, and we have k cores we can use,
# the job will take (n * s) / k (more or less)
# the more cores we have, the faster the code will run 

# some problems are serial can cannot be run in parallel
# this is the case if the output of one operation depends on the output of a previous operation

# if the operations are completely independent, we can do them in any order
# this means we can start another operation as soon as a previous one is finished 

# key terms:
# core - the number cores in your computer's processor
# cluster - a collection of cores (your computer's multi-core processor)
# process - one instance of R (one per core)

# we need to register a cluster with R to run code in parallel 
# we also have to export any information that each instance needs, like packages and functions
# some packages do this for us, but sometimes we have to do it manually

# there are two basic types of clusters
# socket cluster - one instance of R on each core
# forking cluster - copies the current instances of R to a new core
# the socket method works on Mac and PC, but forking clusters have some advantages

###########################################################################
# parLapply
###########################################################################

# requires "parallel"

# detect the number of cores
# we usually want to use the available number - 1
cores <- detectCores() - 1

# make a cluster
cluster <- makeCluster(cores)

# we need to make any required objects and functions available to each core
clusterExport(cluster, c("judges", "experience", "str_split"))

# run serially
t0 <- Sys.time()
out <- lapply(cases$judges, experience_delay)
t1 <- Sys.time()
t1 - t0

# run in parallel
t0 <- Sys.time()
out <- parLapply(cluster, cases$judges, experience_delay)
t1 <- Sys.time()
t1 - t0

# stop cluster
stopCluster(cluster)

###########################################################################
# plyr
###########################################################################

# requires "parallel" and "doParallel"

# if we don't register a cluster using "foreach" and "doParallel",
# using the .parallel option in plyr won't do anything
# registering a cluster using "parallel" won't work

# detect cores
cores <- detectCores() - 1

# make a cluster
cluster <- makeCluster(cores)

# register the cluster
registerDoParallel(cluster)

# we need to make any required objects and functions available to each core
clusterExport(cluster, c("judges", "experience", "str_split"))

# run serially
t0 <- Sys.time()
out <- aaply(.data = cases$judges, .margin = 1, .fun = experience_delay)
t1 <- Sys.time()
t1 - t0

# run in parallel
t0 <- Sys.time()
out <- aaply(.data = cases$judges, .margin = 1, .fun = experience_delay, .parallel = TRUE)
t1 <- Sys.time()
t1 - t0
# the warnings are a known bug

# stop cluster
stopCluster(cluster)

###########################################################################
# foreach
###########################################################################

# requires "parallel" and "foreach"

# detect cores
cores <- detectCores() - 1

# make a cluster
cluster <- makeCluster(cores)

# register the cluster
registerDoParallel(cluster)

# we need to make any required objects and functions available to each core
clusterExport(cluster, c("str_split"))
# foreach will automatically export objects and functions in our workspace that we need

# run serially
t0 <- Sys.time()
out <- foreach(i = 1:nrow(cases), .combine = c) %do% experience_delay(cases$judges[i])
t1 <- Sys.time()
t1 - t0

# run in parallel
t0 <- Sys.time()
out <- foreach(i = 1:nrow(cases), .combine = c) %dopar% experience_delay(cases$judges[i])
t1 <- Sys.time()
t1 - t0

# stop the cluster
stopCluster(cluster)

# we can also export required functions from packages in the foreach() command

# detect cores
cores <- detectCores() - 1

# make a cluster
cluster <- makeCluster(cores)

# register the cluster
registerDoParallel(cluster)

# run in parallel
t0 <- Sys.time()
out <- foreach(i = 1:nrow(cases), 
               .combine = c,
               .packages = c("stringr")) %dopar% experience_delay(cases$judges[i])
t1 <- Sys.time()
t1 - t0

# stop the cluster
stopCluster(cluster)

###########################################################################
# adding a progress bar with foreach
###########################################################################

# requires "parallel", "foreach", and "doSNOW"

# using "plyr", you can add a progress bar with .progress = TRUE
# but this doesn't work if you're also using .parallel = TRUE

# we can add a progress bar using "foreach", but it's complicated

# this time we have to set up a cluster using the "doSNOW" package instead of the "doParallel" package

# detect cores
cores <- detectCores() - 1

# make a cluster
cluster <- makeCluster(cores)

# register the cluster
registerDoSNOW(cluster)

# it's easiest to put all the code in a function
# we don't need to pass any arguments to it, but we can if we need to 
# now that we're putting foreach() in a function, we need to export required objects and functions in the workspace
run <- function() {
  
  # number of iterations
  iterations <- nrow(cases)
  
  # set up a progress bar
  pb <- txtProgressBar(max = iterations, style = 3)
  
  # write a function to update the progress bar
  progress <- function(n) {
    setTxtProgressBar(pb, n)
  }
  
  # specify options to pass to foreach()
  opts <- list(progress = progress)
  
  # run the loop
  out <- foreach(i = 1:iterations,
                 .combine = c,
                 .packages = c("stringr"), # we need to tell R to export any required packages to each core
                 .options.snow = opts, # this sets up the progress bar
                 .export = c("cases", "judges", "experience", "experience_delay")) %dopar% # we also need to tell R to export requred objects and function to each core
                 {
                   experience_delay(cases$judges[i])
                 }

  # close the progress bar
  close(pb)
  
  # return function output
  return(out)
}

# run serially
t0 <- Sys.time()
out <- run()
t1 <- Sys.time()
t1 - t0

# stop the cluster
stopCluster(cluster)

###########################################################################
# end R script
###########################################################################
