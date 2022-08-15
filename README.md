
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/flyaflya/causact/workflows/R-CMD-check/badge.svg)](https://github.com/flyaflya/causact/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/causact)](https://cran.r-project.org/package=causact)
[![Codecov test
coverage](https://codecov.io/gh/flyaflya/causact/branch/master/graph/badge.svg)](https://app.codecov.io/gh/flyaflya/causact?branch=master)
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6949489.svg)](https://doi.org/10.5281/zenodo.6949489)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.04415/status.svg)](https://doi.org/10.21105/joss.04415)
<!-- badges: end -->

# causact

*Accelerate Bayesian analytics workflows* in R through interactive
modelling, visualization, and inference. Uses probabilistic graphical
models as a unifying language for business stakeholders, statisticians,
and programmers.

<img src="man/figures/causactDemo.gif" width="40%" style="display: block; margin: auto;" />

This package relies on the sleek and elegant `greta` package for
Bayesian inference. `greta`, in turn, is an interface into `TensorFlow`
from R. Future iterations of the `causact` package will aim to be a
front-end into several universal probablistic programming languages
(e.g. Stan, Turing, Gen, etc.).

Using the `causact` package for Bayesian inference is featured in
`A Business Analyst's Introduction to Business Analytics` available at
<https://www.causact.com/>.

> Feedback and encouragement is appreciated via github issues or Twitter
> (<https://twitter.com/preposterior>).

## Installation

You can install the current release version of the package from CRAN:

    install.packages("causact")

or the development version from GitHub:

    install.packages("remotes")
    remotes::install_github("flyaflya/causact")

`causact` requires the `greta` package for Bayesian updating, which in
turn, requires a specific version of `TensorFlow`. Install both `greta`
and `TensorFlow` using the instructions available here:
<https://www.causact.com/install-tensorflow-greta-and-causact.html>.

## Usage

Example taken from
<https://www.causact.com/graphical-models-tell-joint-distribution-stories.html#graphical-models-tell-joint-distribution-stories>
with the packages `dag_foo()` functions further described here:

<https://www.causact.com/causact-quick-inference-with-generative-dags.html#causact-quick-inference-with-generative-dags>

### Create beautiful model visualizations.

``` r
library(causact)
graph = dag_create() %>%
  dag_node(descr = "Get Card", label = "y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability", label = "theta",
           rhs = beta(2,2),
           child = "y") %>%
  dag_plate(descr = "Car Model", label = "x",  
            data = carModelDF$carModel,  
            nodeLabels = "theta",  
            addDataNode = TRUE)  
graph %>% dag_render()
```

<img src="man/figures/cardPlot.png" width="60%" />

### Hide model complexity, as appropriate, from domain experts and other less statistically minded stakeholders.

``` r
graph %>% dag_render(shortLabel = TRUE)
```

<img src="man/figures/cardPlotShortLabel.png" width="50%" />

### See useful `greta` code without executing it (for debugging or learning)

``` r
library(greta)
#> 
#> Attaching package: 'greta'
#> The following objects are masked from 'package:stats':
#> 
#>     binomial, cov2cor, poisson
#> The following objects are masked from 'package:base':
#> 
#>     %*%, apply, backsolve, beta, chol2inv, colMeans, colSums, diag,
#>     eigen, forwardsolve, gamma, identity, rowMeans, rowSums, sweep,
#>     tapply
gretaCode = graph %>% dag_greta(mcmc = FALSE)
#> ## The below greta code will return a posterior distribution 
#> ## for the given DAG. Either copy and paste this code to use greta
#> ## directly, evaluate the output object using 'eval', or 
#> ## or (preferably) use dag_greta(mcmc=TRUE) to return a data frame of
#> ## the posterior distribution: 
#> y <- as_data(carModelDF$getCard)   #DATA
#> x      <- as.factor(carModelDF$carModel)   #DIM
#> x_dim  <- length(unique(x))   #DIM
#> theta  <- beta(shape1 = 2, shape2 = 2, dim = x_dim)   #PRIOR
#> distribution(y) <- bernoulli(prob = theta[x])   #LIKELIHOOD
#> gretaModel  <- model(theta)   #MODEL
#> meaningfulLabels(graph)
#> draws       <- mcmc(gretaModel)              #POSTERIOR
#> drawsDF     <- replaceLabels(draws) %>% as.matrix() %>%
#>                 dplyr::as_tibble()           #POSTERIOR
#> tidyDrawsDF <- drawsDF %>% addPriorGroups()  #POSTERIOR
```

### Get posterior while automatically running the underlying `greta` code

``` r
library(greta)
drawsDF = graph %>% dag_greta()
drawsDF  ### see top of data frame
#> # A tibble: 4,000 x 4
#>    theta_JpWrnglr theta_KiaForte theta_SbrOtbck theta_ToytCrll
#>             <dbl>          <dbl>          <dbl>          <dbl>
#>  1          0.869          0.209          0.573          0.209
#>  2          0.823          0.158          0.488          0.219
#>  3          0.872          0.137          0.656          0.195
#>  4          0.821          0.285          0.613          0.196
#>  5          0.821          0.285          0.613          0.196
#>  6          0.821          0.285          0.613          0.196
#>  7          0.854          0.206          0.643          0.193
#>  8          0.815          0.284          0.638          0.184
#>  9          0.815          0.284          0.638          0.184
#> 10          0.837          0.313          0.611          0.189
#> # ... with 3,990 more rows
```

### Get quick view of posterior distribution

``` r
drawsDF %>% dagp_plot()
```

<img src="man/figures/gretaPost-1.png" title="Credible interval plots." alt="Credible interval plots." width="70%" />

## Getting Help and Suggesting Improvements

Whether you encounter a clear bug, have a suggestion for improvement, or
just have a question, we are thrilled to help you out. In all cases,
please file a [GitHub
issue](https://github.com/flyaflya/causact/issues). If reporting a bug,
please include a minimal reproducible example. If encountering issues
installing `greta`, please seek help at the [greta discussion
forum](https://forum.greta-stats.org/).

## Contributing

We welcome help turning `causact` into the most intuitive and fastest
method of converting stakeholder narratives about data-generating
processes into actionable insight from posterior distributions. If you
want to help us achieve this vision, we welcome your contributions after
reading the [new contributor
guide](https://github.com/flyaflya/causact/blob/master/.github/contributing.md).
Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/flyaflya/causact/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## Further Usage

For more info, see
`A Business Analyst's Introduction to Business Analytics` available at
<https://www.causact.com>. You can also check out the package’s
vignette: `vignette("narrative-to-insight-with-causact")`. Two
additional examples are shown below.

## Prosocial Chimpanzees Example from Statistical Rethinking

> McElreath, Richard. Statistical rethinking: A Bayesian course with
> examples in R and Stan. Chapman and Hall/CRC, 2018.

``` r
library(greta)
library(tidyverse)
library(causact)

# data object used below, chimpanzeesDF, is built-in to causact package

graph = dag_create() %>%
  dag_node("Pull Left Handle","L",
           rhs = bernoulli(p),
           data = causact::chimpanzeesDF$pulled_left) %>%
  dag_node("Probability of Pull", "p",
           rhs = ilogit(alpha + gamma + beta),
           child = "L") %>%
  dag_node("Actor Intercept","alpha",
           rhs = normal(alphaBar, sigma_alpha),
           child = "p") %>%
  dag_node("Block Intercept","gamma",
           rhs = normal(0,sigma_gamma),
           child = "p") %>%
  dag_node("Treatment Intercept","beta",
           rhs = normal(0,0.5),
           child = "p") %>%
  dag_node("Actor Population Intercept","alphaBar",
           rhs = normal(0,1.5),
           child = "alpha") %>%
  dag_node("Actor Variation","sigma_alpha",
           rhs = exponential(1),
           child = "alpha") %>%
  dag_node("Block Variation","sigma_gamma",
           rhs = exponential(1),
           child = "gamma") %>%
  dag_plate("Observation","i",
            nodeLabels = c("L","p")) %>%
  dag_plate("Actor","act",
            nodeLabels = c("alpha"),
            data = chimpanzeesDF$actor,
            addDataNode = TRUE) %>%
  dag_plate("Block","blk",
            nodeLabels = c("gamma"),
            data = chimpanzeesDF$block,
            addDataNode = TRUE) %>%
  dag_plate("Treatment","trtmt",
            nodeLabels = c("beta"),
            data = chimpanzeesDF$treatment,
            addDataNode = TRUE)
```

### See graph

``` r
graph %>% dag_render(width = 2000, height = 800)
```

<img src="man/figures/chimpStat.png" width="120%" />

### Communicate with stakeholders for whom the statistics might be distracting

``` r
graph %>% dag_render(shortLabel = TRUE)
```

<img src="man/figures/chimpStatSL.png" width="100%" />

### Compute posterior

``` r
drawsDF = graph %>% dag_greta()
```

### Visualize posterior

``` r
drawsDF %>% dagp_plot()
```

<img src="man/figures/chimpsGraphPost-1.png" width="100%" />

## Eight Schools Example from Bayesian Data Analysis

> Gelman, Andrew, Hal S. Stern, John B. Carlin, David B. Dunson, Aki
> Vehtari, and Donald B. Rubin. Bayesian data analysis. Chapman and
> Hall/CRC, 2013.

``` r
library(greta)
library(tidyverse)
library(causact)

# data object used below, schoolDF, is built-in to causact package

graph = dag_create() %>%
  dag_node("Treatment Effect","y",
           rhs = normal(theta, sigma),
           data = causact::schoolsDF$y) %>%
  dag_node("Std Error of Effect Estimates","sigma",
           data = causact::schoolsDF$sigma,
           child = "y") %>%
  dag_node("Exp. Treatment Effect","theta",
           child = "y",
           rhs = avgEffect + schoolEffect) %>%
  dag_node("Pop Treatment Effect","avgEffect",
           child = "theta",
           rhs = normal(0,30)) %>%
  dag_node("School Level Effects","schoolEffect",
           rhs = normal(0,30),
           child = "theta") %>%
  dag_plate("Observation","i",nodeLabels = c("sigma","y","theta")) %>%
  dag_plate("School Name","school",
            nodeLabels = "schoolEffect",
            data = causact::schoolsDF$schoolName,
            addDataNode = TRUE)
```

### See graph

``` r
graph %>% dag_render()
```

<img src="man/figures/eightSchoolStat.png" width="100%" />

### Compute posterior

``` r
drawsDF = graph %>% dag_greta()
```

### Visualize posterior

``` r
drawsDF %>% dagp_plot()
```

<img src="man/figures/eightschoolsGraphPost-1.png" width="100%" />

## Example Where Observed RV Is A Mixed RV

``` r
#### use dirichlet instead
library(greta)
library(tidyverse)
library(causact)

## sample data - try to recover params
x <- c(rpois(800, 3),rpois(200, 10))

graph = dag_create() %>%  ## create generative DAG
  dag_node("Mixed Var","x",
           rhs = mixture(alpha,beta,
                         weights = t(weights)),
           data = x) %>%
  dag_node("Count Var 1","alpha",
           rhs = poisson(lambda1),
           child = "x") %>%
  dag_node("Count Var 2","beta",
           rhs = poisson(lambda2),
           child = "x") %>%
  dag_node("Weight Vars","weights",
           rhs = dirichlet(t(c(1,1))),
           child = "x") %>%
  dag_node("Exp Rate 1","lambda1",
           rhs = uniform(1,5),
           child = "alpha") %>%
  dag_node("Exp Rate 2","lambda2",
           rhs = uniform(6,20),
           child = "beta")
```

### See graph

``` r
graph %>% dag_render()
```

<img src="man/figures/mixture.png" width="100%" />

### Compute posterior

### Visualize posterior

``` r
drawsDF %>% dagp_plot()
```

<img src="man/figures/mixturePost.png" width="100%" />
