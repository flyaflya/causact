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
#'    positive parameters, `alpha` must be a vector for `dirichlet`.
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
#' @details The discrete probability distributions (`bernoulli`,
#'   `binomial`, `negative_binomial`, `poisson`,
#'   `multinomial`, `categorical`) can
#'   be used when they have fixed values, but not as unknown variables.
#'
#'   For univariate distributions `dim` gives the dimensions of the array to create. Each element will be (independently)
#'   distributed according to the distribution. `dim` can also be left at
#'   its default of `NULL`, in which case the dimension will be detected
#'   from the dimensions of the parameters (provided they are compatible with
#'   one another).
#'
#'   For multivariate distributions (`multivariate_normal()`,
#'   `multinomial()`, `categorical()`, and `dirichlet()`
#'   each row of the output and parameters
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
#'   Wherever possible, the parameterisations and argument names of causact
#'   distributions match commonly used R functions for distributions, such as
#'   those in the `stats` or `extraDistr` packages. The following
#'   table states the distribution function to which causact's implementation
#'   corresponds (this code largely borrowed from the greta package):
#'
#'   \tabular{ll}{ causact \tab reference\cr `uniform` \tab
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
  min_quo <- rlang::enquo(min)
  min_name <- rlang::quo_name(min_quo)
  max_quo <- rlang::enquo(max)
  max_name <- rlang::quo_name(max_quo)
  paste0("dist.Uniform(",min_name,",",max_name,")")
}

#' @rdname distributions
#' @export
normal <- function(mean, sd, dim = NULL, truncation = c(-Inf, Inf)) {
  mean_quo <- rlang::enquo(mean)
  mean_name <- rlang::quo_name(mean_quo)
  sd_quo <- rlang::enquo(sd)
  sd_name <- rlang::quo_name(sd_quo)
  if (identical(truncation, c(-Inf, Inf))){
    numpyroCode = paste0("dist.Normal(",mean_name,",",sd_name,")") }
  else {
    kwargs = NULL
    if (truncation[1] != -Inf) {
      kwargs = paste0(kwargs,",low=",truncation[1])
      }
    if (truncation[2] != Inf) {
      kwargs = paste0(kwargs,",high=",truncation[2])
    }
    numpyroCode = paste0("dist.TruncatedNormal(",mean_name,",",sd_name,kwargs,")") }
  return(numpyroCode)
  }

#' @rdname distributions
#' @export
lognormal <- function(meanlog, sdlog, dim = NULL) {
  meanlog_quo <- rlang::enquo(meanlog)
  meanlog_name <- rlang::quo_name(meanlog_quo)
  sdlog_quo <- rlang::enquo(sdlog)
  sdlog_name <- rlang::quo_name(sdlog_quo)
  paste0("dist.LogNormal(",meanlog_name,",",sdlog_name,")")
}

#' @rdname distributions
#' @export
bernoulli <- function(prob, dim = NULL) {
  prob_quo <- rlang::enquo(prob)
  prob_name <- rlang::quo_name(prob_quo)
  paste0("dist.Bernoulli(",prob_name,")")
}

#' @rdname distributions
#' @export
binomial <- function(size, prob, dim = NULL) {
  size_quo <- rlang::enquo(size)
  size_name <- rlang::quo_name(size_quo)
  prob_quo <- rlang::enquo(prob)
  prob_name <- rlang::quo_name(prob_quo)
  paste0("dist.Binomial(",size_name,",",prob_name,")")
}

#' @rdname distributions
#' @export
negative_binomial <- function(size, prob, dim = NULL) {
  size_quo <- rlang::enquo(size)
  size_name <- rlang::quo_name(size_quo)
  prob_quo <- rlang::enquo(prob)
  prob_name <- rlang::quo_name(prob_quo)
  paste0("dist.NegativeBinomial(",size_name,",",prob_name,")")
}

#' @rdname distributions
#' @export
poisson <- function(lambda, dim = NULL) {
  lambda_quo <- rlang::enquo(lambda)
  lambda_name <- rlang::quo_name(lambda_quo)
  paste0("dist.Poisson(",lambda_name,")")
}

#' @rdname distributions
#' @export
gamma <- function(shape, rate, dim = NULL) {
  shape_quo <- rlang::enquo(shape)
  shape_name <- rlang::quo_name(shape_quo)
  rate_quo <- rlang::enquo(rate)
  rate_name <- rlang::quo_name(rate_quo)
  paste0("dist.Gamma(",shape_name,",",rate_name,")")
}

#' @rdname distributions
#' @export
inverse_gamma <- function(alpha, beta, dim = NULL, truncation = c(0, Inf)) {
  alpha_quo <- rlang::enquo(alpha)
  alpha_name <- rlang::quo_name(alpha_quo)
  beta_quo <- rlang::enquo(beta)
  beta_name <- rlang::quo_name(beta_quo)
  paste0("dist.InverseGamma(",alpha_name,",",beta_name,")")
}

#' @rdname distributions
#' @export
weibull <- function(shape, scale, dim = NULL) {
  shape_quo <- rlang::enquo(shape)
  shape_name <- rlang::quo_name(shape_quo)
  scale_quo <- rlang::enquo(scale)
  scale_name <- rlang::quo_name(scale_quo)
  paste0("dist.Weibull(", scale_name, ",","1/",shape_name,")")
}

#' @rdname distributions
#' @export
exponential <- function(rate, dim = NULL) {
  lambda_quo <- rlang::enquo(lambda)
  lambda_name <- rlang::quo_name(lambda_quo)
  paste0("dist.Exponential(",lambda_name,")")
}

#' @rdname distributions
#' @export
pareto <- function(a, b, dim = NULL) {
  a_quo <- rlang::enquo(a)
  a_name <- rlang::quo_name(a_quo)
  b_quo <- rlang::enquo(b)
  b_name <- rlang::quo_name(b_quo)
  paste0("dist.Pareto(",a_name,",",b_name,")")
}

#' @rdname distributions
#' @export
student <- function(df, mu, sigma, dim = NULL, truncation = c(-Inf, Inf)) {
  df_quo <- rlang::enquo(df)
  df_name <- rlang::quo_name(df_quo)
  mu_quo <- rlang::enquo(mu)
  mu_name <- rlang::quo_name(mu_quo)
  sigma_quo <- rlang::enquo(sigma)
  sigma_name <- rlang::quo_name(sigma_quo)
  paste0("dist.StudentT(",df_name,",",mu_name,",",sigma_name,")")
}

#' @rdname distributions
#' @export
laplace <- function(mu, sigma, dim = NULL, truncation = c(-Inf, Inf)) {
  mu_quo <- rlang::enquo(mu)
  mu_name <- rlang::quo_name(mu_quo)
  sigma_quo <- rlang::enquo(sigma)
  sigma_name <- rlang::quo_name(sigma_quo)
  paste0("dist.Laplace(",mu_name,",",sigma_name,")")
}

#' @rdname distributions
#' @export
beta <- function(shape1, shape2, dim = NULL) {
  a_quo <- rlang::enquo(shape1)
  a_name <- rlang::quo_name(a_quo)
  b_quo <- rlang::enquo(shape2)
  b_name <- rlang::quo_name(b_quo)
  paste0("dist.Laplace(",a_name,",",b_name,")")
}

#' @rdname distributions
#' @export
cauchy <- function(location, scale, dim = NULL, truncation = c(-Inf, Inf)) {
  mu_quo <- rlang::enquo(location)
  mu_name <- rlang::quo_name(mu_quo)
  sigma_quo <- rlang::enquo(scale)
  sigma_name <- rlang::quo_name(sigma_quo)
  if (identical(truncation, c(-Inf, Inf))){
    numpyroCode = paste0("dist.Cauchy(",mu_name,",",sigma_name,")") }
  else {
    kwargs = NULL
    if (truncation[1] != -Inf) {
      kwargs = paste0(kwargs,",low=",truncation[1])
    }
    if (truncation[2] != Inf) {
      kwargs = paste0(kwargs,",high=",truncation[2])
    }
    numpyroCode = paste0("dist.TruncatedCauchy(",mu_name,",",sigma_name,kwargs,")") }
  return(numpyroCode)
}

#' @rdname distributions
#' @export
chi_squared <- function(df, dim = NULL) {
  df_quo <- rlang::enquo(df)
  df_name <- rlang::quo_name(df_quo)
  paste0("dist.Chi2(",df_name,")")
}

#' @rdname distributions
#' @export
logistic <- function(location, scale, dim = NULL, truncation = c(-Inf, Inf)) {
  mu_quo <- rlang::enquo(location)
  mu_name <- rlang::quo_name(mu_quo)
  scale_quo <- rlang::enquo(scale)
  scale_name <- rlang::quo_name(scale_quo)
  paste0("dist.Logistic(",mu_name,",",scale_name,")")
}

# nolint start
#' @rdname distributions
#' @export
multivariate_normal <- function(mean, Sigma, dimension = NULL) {
  mu_quo <- rlang::enquo(mean)
  mu_name <- rlang::quo_name(mu_quo)
  sigma_quo <- rlang::enquo(Sigma)
  sigma_name <- rlang::quo_name(sigma_quo)
  # nolint end
  paste0("dist.MultivariateNormal(",mu_name,",covariance_matrix=",sigma_name,")")
}

#' @rdname distributions
#' @export
lkj_correlation <- function(eta, dimension = 2) {
  eta_quo <- rlang::enquo(eta)
  eta_name <- rlang::quo_name(eta_quo)
  paste0("dist.Logistic(",dimension,",",eta_name,")")
}

#' @rdname distributions
#' @export
multinomial <- function(size, prob, dimension = NULL) {
  size_quo <- rlang::enquo(size)
  size_name <- rlang::quo_name(size_quo)
  prob_quo <- rlang::enquo(prob)
  prob_name <- rlang::quo_name(prob_quo)
  paste0("dist.Multinomial(",size_name,",",prob_name,")")
}

#' @rdname distributions
#' @export
categorical <- function(prob, dimension = NULL) {
  prob_quo <- rlang::enquo(prob)
  prob_name <- rlang::quo_name(prob_quo)
  paste0("dist.Categorical(",prob_name,")")
}

#' @rdname distributions
#' @export
dirichlet <- function(alpha, dimension = NULL) {
  alpha_quo <- rlang::enquo(alpha)
  alpha_name <- rlang::quo_name(alpha_quo)
  paste0("dist.Dirichlet(",alpha_name,")")
}
