#' Make dbern,pbern,qbern,rbern available for
#' implementation of the Bernoulli distribution functions.
#'
#' Density, distribution function, quantile function and random generation for the benoulli distribution with parameter \code{prob}.
#'
#' @param n number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param prob probability of success of each trial
#' @return A vector of 0's and 1's representing failure and success.
#' @examples
#' #Return a random result of a Bernoulli trial given \code{prob}.
#' rbern(n =1, prob = 0.5)
#' @importFrom stats rbinom
#' @export
rbern <- function(n, prob)
  {
    rbinom(n,size=1,prob=prob)
  }
