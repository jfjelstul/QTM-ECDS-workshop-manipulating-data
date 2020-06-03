# QTM-ECDS Intermediate Workshop

This repository contains the replication materials for the QTM-ECDS Intermediate R Workshop by Josh Fjelstul, Ph.D.

# Workshop Description

This workshop will focus on manipulating data in R. We will talk about best practices for working with multiple datasets at a time, combining datasets, and creating variables that use data from multiple datasets. The workshop will cover the tidyr, plyr, dplyr, and foreach packages.

First, we will cover different ways of merging data frames (dplyr) and how to reshape data frames from wide format to long format (gather and spread in tidyr). Second, we will go over piping (dplyr), which is a way of chaining together operations (instead of nesting them). We can use pipes to pass the output of one operation to another operation without creating an intermediate object. We will use pipes to group observations and create variables based on those groups. We will also use it to collapse observations, aggregating the data. Third, we will cover ways to apply non-vectorized functions to variables in data frames, including for loops and split-apply-combine operations (foreach, plyr). We will also use loops to construct variables that use data from multiple observations or data frames. We will then talk about how to run code in parallel over multiple cores to speed up code that we want to run on large datasets and/or that uses computationally intensive operations. Finally, we will talk about how to organize data for making plots (ggplot2).

Participants will need to already be familiar with opening and writing R scripts, reading in and exporting data, loading packages, working with vectors, lists, and data frames, and making variables. The workshop will be interactive and participants should bring a laptop with an up-to-date version of R installed.
