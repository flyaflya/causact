---
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Overview
Causact is a package to accelerate business analytics workflows.You provide the data, tell Causact what is the logic for the workflow, and it takes care of the details. It draws the beautiful graph to show the whole analytics workflow clearly.

## Installation

```{r, eval = FALSE}
# Get the devtools package:
install.packages("devtools")
# Once it is installed, use the install_github() function:
#devtools::install_github()
# Or use the library call:
library("devtools")
# Then you can use install_github() function directly
install_github("flyaflya/causact")
```

## Usage

Causact has many functions to graph the beautiful workflow. It embodies a deep philosophy of Bayesian workflow. It is hard to succintcyly describe how every functions in causact works. The simplest trick is to use double colon after causact. If you use the double colon trick, you can see what is available in the causact package. For example, there are couple datasets and couple functions can be used for further analytics.

Here is an example showing how to read the dataset in the causact package and manipulate it.

```{r example}
# Load the causact package:
library(causact)
# delivDF dataset contains information for shipping, quantity etc.
data(delivDF)
# Make the delivDF data become available in global enviorment
# delivDF has 117790 observations and 5 variables:
delivDF
# Using dplyr package to manipulate the dataset:
library(dplyr)
# Return the shipID equals to 10002:
delivDF %>% filter(shipID == '10002')
```
Here is another example of the baseballDF dataset.
```{r example}
# Load the packages:
library(causact)
library(dplyr)
library(ggplot2)
# For the baseballDF dataset
# Date is the date the baseball game was played
# Home and Visitor reflect the "home" team and "visiting" team
# HomeScore and VisitorScore give the # of runs scored by the home team and visiting team
baseballDF = baseballData %>% as_tibble()
```

Causact could also help you generate samples from a random Bernoulli variable. The rbern function can easily make this.

```{r example}
# Load the packages:
library(causact)
# rbern function is in the causact package, which takes two parameters:
# 1) n is is the number of trails
# 2) prob is the probability of success
set.seed(123)
rbern(n=7,prob=0.5)
```

## Learning causact

If you are new to causact you are better off starting with a systematic introduction. The book from Dr.Adam Fleischhacke named __The Business Analyst's Guide To Business Analytics__ could give you more help on the whole workflow and the usage of the package.


## Getting help

If you have any suggestions or comments, please email Dr. Adam Fleischhacke at ajf@udel.edu.
