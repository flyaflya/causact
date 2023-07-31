#' @name distributions
#' @title probability distributions
#' @description These functions can be used to define random variables in a
#'   causact model.
#'
#' @param min,max scalar values giving optional limits to `uniform`
#'   variables. Like `lower` and `upper`, these must be specified as
#'   numerics, unlike `lower` and `upper`, they must be finite.
#'   `min` must always be less than `max`.
#'
#' @param mean,meanlog,location,mu unconstrained parameters
#'
#' @param
#'   sd,sdlog,sigma,lambda,shape,rate,df,scale,shape1,shape2,alpha,beta,df1,df2,a,b,eta
#'    positive parameters, `alpha` must be a vector for `dirichlet`
#'   and `dirichlet_multinomial`.
#'
#' @param size,m,n,k positive integer parameter
#'
#' @param prob probability parameter (`0 < prob < 1`), must be a vector for
#'   `multinomial` and `categorical`
#'
#' @param Sigma positive definite variance-covariance matrix parameter
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'   or a vector of positive integers. See details.
#'
#' @param dimension the dimension of a multivariate distribution
#'
#' @param n_realisations the number of independent realisation of a multivariate
#'   distribution
#'
#' @details The discrete probability distributions (`bernoulli`,
#'   `binomial`, `negative_binomial`, `poisson`,
#'   `multinomial`, `categorical`, `dirichlet_multinomial`) can
#'   be used when they have fixed values, but not as unknown variables.
#'
#'   For univariate distributions `dim` gives the dimensions of the array to create. Each element will be (independently)
#'   distributed according to the distribution. `dim` can also be left at
#'   its default of `NULL`, in which case the dimension will be detected
#'   from the dimensions of the parameters (provided they are compatible with
#'   one another).
#'
#'   For multivariate distributions (`multivariate_normal()`,
#'   `multinomial()`, `categorical()`, `dirichlet()`, and
#'   `dirichlet_multinomial()`) each row of the output and parameters
#'   corresponds to an independent realisation. If a single realisation or
#'   parameter value is specified, it must therefore be a row vector (see
#'   example). `n_realisations` gives the number of rows/realisations, and
#'   `dimension` gives the dimension of the distribution. I.e. a bivariate
#'   normal distribution would be produced with `multivariate_normal(...,
#'   dimension = 2)`. The dimension can usually be detected from the parameters.
#'
#'   `multinomial()` does not check that observed values sum to
#'   `size`, and `categorical()` does not check that only one of the
#'   observed entries is 1. It's the user's responsibility to check their data
#'   matches the distribution!
#'
#'   The parameters of `uniform` must be fixed, not greta arrays. This
#'   ensures these values can always be transformed to a continuous scale to run
#'   the samplers efficiently. However, a hierarchical `uniform` parameter
#'   can always be created by defining a `uniform` variable constrained
#'   between 0 and 1, and then transforming it to the required scale. See below
#'   for an example.
#'
#'   Wherever possible, the parameterisations and argument names of greta
#'   distributions match commonly used R functions for distributions, such as
#'   those in the `stats` or `extraDistr` packages. The following
#'   table states the distribution function to which greta's implementation
#'   corresponds:
#'
#'   \tabular{ll}{ greta \tab reference\cr `uniform` \tab
#'   [stats::dunif]\cr `normal` \tab
#'   [stats::dnorm]\cr `lognormal` \tab
#'   [stats::dlnorm]\cr `bernoulli` \tab
#'   [extraDistr::dbern]\cr `binomial` \tab
#'   [stats::dbinom]\cr `beta_binomial` \tab
#'   [extraDistr::dbbinom]\cr `negative_binomial`
#'   \tab [stats::dnbinom]\cr `hypergeometric` \tab
#'   [stats::dhyper]\cr `poisson` \tab
#'   [stats::dpois]\cr `gamma` \tab
#'   [stats::dgamma]\cr `inverse_gamma` \tab
#'   [extraDistr::dinvgamma]\cr `weibull` \tab
#'   [stats::dweibull]\cr `exponential` \tab
#'   [stats::dexp]\cr `pareto` \tab
#'   [extraDistr::dpareto]\cr `student` \tab
#'   [extraDistr::dlst]\cr `laplace` \tab
#'   [extraDistr::dlaplace]\cr `beta` \tab
#'   [stats::dbeta]\cr `cauchy` \tab
#'   [stats::dcauchy]\cr `chi_squared` \tab
#'   [stats::dchisq]\cr `logistic` \tab
#'   [stats::dlogis]\cr `f` \tab
#'   [stats::df]\cr `multivariate_normal` \tab
#'   [mvtnorm::dmvnorm]\cr `multinomial` \tab
#'   [stats::dmultinom]\cr `categorical` \tab
#'   {[stats::dmultinom] (size = 1)}\cr `dirichlet`
#'   \tab [extraDistr::ddirichlet]\cr
#'   `dirichlet_multinomial` \tab
#'   [extraDistr::ddirmnom]\cr `wishart` \tab
#'   [stats::rWishart]\cr `lkj_correlation` \tab
#'   [rethinking::dlkjcorr](https://rdrr.io/github/rmcelreath/rethinking/man/dlkjcorr.html)
#'   }
#'
#' @examples
#' \dontrun{
#'
#' # a uniform parameter constrained to be between 0 and 1
#' phi <- uniform(min = 0, max = 1)
#'
#' # a length-three variable, with each element following a standard normal
#' # distribution
#' alpha <- normal(0, 1, dim = 3)
#'
#' # a length-three variable of lognormals
#' sigma <- lognormal(0, 3, dim = 3)
#'
#' # a hierarchical uniform, constrained between alpha and alpha + sigma,
#' eta <- alpha + uniform(0, 1, dim = 3) * sigma
#'
#' # a hierarchical distribution
#' mu <- normal(0, 1)
#' sigma <- lognormal(0, 1)
#' theta <- normal(mu, sigma)
#'
#' # a vector of 3 variables drawn from the same hierarchical distribution
#' thetas <- normal(mu, sigma, dim = 3)
#'
#' # a matrix of 12 variables drawn from the same hierarchical distribution
#' thetas <- normal(mu, sigma, dim = c(3, 4))
#'
#' # a multivariate normal variable, with correlation between two elements
#' # note that the parameter must be a row vector
#' Sig <- diag(4)
#' Sig[3, 4] <- Sig[4, 3] <- 0.6
#' theta <- multivariate_normal(t(rep(mu, 4)), Sig)
#'
#' # 10 independent replicates of that
#' theta <- multivariate_normal(t(rep(mu, 4)), Sig, n_realisations = 10)
#'
#' # 10 multivariate normal replicates, each with a different mean vector,
#' # but the same covariance matrix
#' means <- matrix(rnorm(40), 10, 4)
#' theta <- multivariate_normal(means, Sig, n_realisations = 10)
#' dim(theta)
#'
#' # a Wishart variable with the same covariance parameter
#' theta <- wishart(df = 5, Sigma = Sig)
#' }
NULL
# nolint end

#' @rdname distributions
#' @export
uniform <- function(min, max, dim = NULL) {
  paste0("dist.Uniform(",min,",",max,")")
}

#' @rdname distributions
#' @export
normal <- function(mean, sd, dim = NULL, truncation = c(-Inf, Inf)) {
  if (identical(truncation, c(-Inf, Inf))){
    numpyroCode = paste0("dist.Normal(",mean,",",sd,")") }
  else {
    kwargs = NULL
    if (truncation[1] != -Inf) {
      kwargs = paste0(kwargs,",low=",truncation[1])
      }
    if (truncation[2] != Inf) {
      kwargs = paste0(kwargs,",high=",truncation[2])
    }
    numpyroCode = paste0("dist.TruncatedNormal(",mean,",",sd,kwargs,")") }
  return(numpyroCode)
  }

#' @rdname distributions
#' @export
lognormal <- function(meanlog, sdlog, dim = NULL) {
    paste0("dist.LogNormal(",meanlog,",",sdlog,")")
}

#' @rdname distributions
#' @export
bernoulli <- function(prob, dim = NULL) {
  paste0("dist.Bernoulli(",prob,")")
}

#' @rdname distributions
#' @export
binomial <- function(size, prob, dim = NULL) {
  paste0("dist.Binomial(",size,",",prob,")")
}

#' @rdname distributions
#' @export
negative_binomial <- function(size, prob, dim = NULL) {
  paste0("dist.NegativeBinomial(",size,",",prob,")")
}

#' @rdname distributions
#' @export
poisson <- function(lambda, dim = NULL) {
  paste0("dist.Poisson(",lambda,")")
}

#' @rdname distributions
#' @export
gamma <- function(shape, rate, dim = NULL) {
  paste0("dist.Gamma(",shape,",",rate,")")
}

#' @rdname distributions
#' @export
inverse_gamma <- function(alpha, beta, dim = NULL, truncation = c(0, Inf)) {
  paste0("dist.InverseGamma(",shape,",",rate,")")
}

#' @rdname distributions
#' @export
weibull <- function(shape, scale, dim = NULL) {
  distrib("dist.Weibull(", scale, ",",1/shape,")")
}

#' @rdname distributions
#' @export
exponential <- function(rate, dim = NULL) {
  paste0("dist.Exponential(",lambda,")")
}

#' @rdname distributions
#' @export
pareto <- function(a, b, dim = NULL) {
  paste0("dist.Pareto(",a,",",b,")")
}

#' @rdname distributions
#' @export
student <- function(df, mu, sigma, dim = NULL, truncation = c(-Inf, Inf)) {
  paste0("dist.StudentT(",df,",",mu,",",sigma,")")
}

#' @rdname distributions
#' @export
laplace <- function(mu, sigma, dim = NULL, truncation = c(-Inf, Inf)) {
  paste0("dist.Laplace(",mu,",",sigma,")")
}

#' @rdname distributions
#' @export
beta <- function(shape1, shape2, dim = NULL) {
  paste0("dist.Laplace(",shape1,",",shape2,")")
}

#' @rdname distributions
#' @export
cauchy <- function(location, scale, dim = NULL, truncation = c(-Inf, Inf)) {
  if (identical(truncation, c(-Inf, Inf))){
    numpyroCode = paste0("dist.Cauchy(",location,",",scale,")") }
  else {
    kwargs = NULL
    if (truncation[1] != -Inf) {
      kwargs = paste0(kwargs,",low=",truncation[1])
    }
    if (truncation[2] != Inf) {
      kwargs = paste0(kwargs,",high=",truncation[2])
    }
    numpyroCode = paste0("dist.TruncatedCauchy(",location,",",scale,kwargs,")") }
  return(numpyroCode)
}

#' @rdname distributions
#' @export
chi_squared <- function(df, dim = NULL) {
  paste0("dist.Chi2(",df,")")
}

#' @rdname distributions
#' @export
logistic <- function(location, scale, dim = NULL, truncation = c(-Inf, Inf)) {
  paste0("dist.Logistic(",location,",",scale,")")
}

# nolint start
#' @rdname distributions
#' @export
multivariate_normal <- function(mean, Sigma, dimension = NULL) {
  # nolint end
  paste0("dist.MultivariateNormal(",mean,",covariance_matrix=",Sigma,")")
}

#' @rdname distributions
#' @export
lkj_correlation <- function(eta, dimension = 2) {
  paste0("dist.Logistic(",dimension,",",eta,")")
}

#' @rdname distributions
#' @export
multinomial <- function(size, prob, dimension = NULL) {
  paste0("dist.Multinomial(",size,",",prob,")")
}

#' @rdname distributions
#' @export
categorical <- function(prob, dimension = NULL) {
  paste0("dist.Categorical(",prob,")")
}

#' @rdname distributions
#' @export
dirichlet <- function(alpha, dimension = NULL) {
  paste0("dist.Dirichlet(",alpha,")")
}
