#'Fitting logistic regresion
#'
#'Fitting procedure for logistic regression given a design matrix and a response vector.
#'
#'
#' @inheritParams neg_loglik
#' @param ... For \code{\link[stats]{optim}} : Additonal arguments for likelihood maximization procedure.
#'
#' @return A list containing the regression coefficients, the estimated probabilites P(Y = 1) and
#' the (original) data
fit_logitreg <- function(design, response, ...){

  #Initial values for the parameters
  coefs_init <- runif(ncol(design), min = -3, max = 3)

  optim_res <- optim(coefs_init, neg_loglik, neg_loglik_deriv, design = design,
                     response = response, ...)

  coefs_fitted <- optim_res$par
  response_fitted <- 1 / (1 + exp(design %*% coefs_fitted))
  originial_data = cbind(design, response)
  result <- list("coefficients" = coefs_fitted, "fitted" = response_fitted, "data" = originial_data)
  result
}


# data <- sim_data(seed = 123, numerics = 1)
# str(data)
#
# res <- fit_logitreg(data$design, data$response)
# str(res)
#
#
# ?glm
#
#
# model <- glm(response ~ 0 + design, family = binomial(link = 'logit'), data = data)
# model
