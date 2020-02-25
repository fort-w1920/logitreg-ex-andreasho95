#' Simulation of synthetic data sets for tests
#
#' Generates synthetic data sets for tests with q_numeric gaussian variables (~N(0,1))
#' and q_factor dummy variables
#'
#' @param n Number of simulated oberservations.
#' @param numerics Number of numeric columns in the simulated design matrix.
#' @param factors Number of factor columns in the simulated design matrix.
#' @param seed A single starting value for random number generators.
#' @param dataframe If `TRUE`, the default, the results are returned as a list. If `FALSE`, the
#'   results are returned as a data.frame.
#'
#' @return A list(x, y, b): design, response, true coefs or data.frame with attribute('b')
#' @import stats
#' @export
sim_data <- function(n = 1500, numerics = 3, factors = 0, seed = NULL, dataframe = FALSE) {
  # set RNG seed for reproducibility:
  if (!is.null(seed))
    set.seed(seed)

  covariates <- 1 + numerics + factors

  design <- matrix(0, nrow = n, ncol = covariates)
  design[, 1] <- 1
  design[, seq_len(numerics) + 1] <- matrix(rnorm(n * numerics), nrow = n)
  if (factors) {
    # add binary factors
    dummies <- matrix(sample(c(0, 1), n * factors, replace = TRUE), nrow = n)
    design[, -seq_len(1 + numerics)] <- dummies
  }

  coefs <- runif(covariates, min = -3, max = 3)

  probabilities <- logistic(design %*% coefs)
  response <- rbinom(n, prob = probabilities, size = 1)

  if (!dataframe) {
    return(list(design = design, response = response, coefs = coefs))
  }
  structure(
    data.frame(response = response, design[, -1, drop = FALSE]),
    coefs = coefs)
}


################################################################################
## Saving you from actually doing any maths:

#' Logistic Distribution Function
#'
#' Calculates the values of the logistic distribution function for given observations.
#' It is assumed that the data follow a distribution function that has location and scale values of 0.
#'
#' @param x Numeric vector containing the observations.
#'
#' @return Numeric vector containing the values of the distribution function for the given observations.
logistic <- function(x) plogis(x)


#' Negative Log-Likelihood for logistic regression
#'
#' Calculates the value of the logistic distribution or its gradient given the coefficients, design
#' matrix and the response vector.
#' It is assumed that the data follow a distribution function that has location and scale values of 0.
#'
#' @details
#' Given the coefficients, the design matrix and the response vector, neg_loglik and neg_loglik_deriv
#' calculate the negative log-likelihood and its gradient, respectively.
#'
#' @param coefs Numeric vector containing the coefficients for the logistic regression.
#' @param design Numeric matrix containing the (scaled) design matrix.
#' @param response Numeric vector containing the (scaled) response.
#' @return Numeric vector containing the values of the distribution function for the given observations.
neg_loglik <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  - sum(response * log(probabilities) + (1 - response) * log(1 - probabilities))
}

#' @rdname neg_loglik
neg_loglik_deriv <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  - t(response - probabilities) %*% design
}
