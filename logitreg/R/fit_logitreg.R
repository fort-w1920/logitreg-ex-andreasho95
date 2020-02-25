#'Fitting logistic regresion
#'
#'Fitting procedure for logistic regression given a design matrix and a response vector.
#'
#'
#' @inheritParams neg_loglik
#' @param ... For \code{\link[stats]{optim}} : Additonal arguments for likelihood maximization procedure.
#' @param coefs_init Numeric vector containing initial coefficientsfor the optimization.
#' If `NULL` then the coefficients are drawn out of a uniform distribution over the intervall [-3, 3]
#'
#' @return A list containing the regression coefficients, the estimated probabilites P(Y = 1) and
#' the (original) data
fit_logitreg <- function(design, response, coefs_init = NULL, ...){

  checkmate::assert_matrix(design, mode = "numeric", any.missing = FALSE, min.rows = 1, min.cols = 1)
  checkmate::assert_numeric(response, finite = TRUE, any.missing = FALSE, len = nrow(design))
  checkmate::assert_numeric(coefs_init, finite = TRUE, any.missing = FALSE, len = ncol(design), null.ok = TRUE)

  respose_values <- unique(response)
  if (!all(respose_values %in% c(0,1))) {
    stop("The values for the response must be 0 or 1!")
  }

  #Initial values for the parameters
  if (is.null(coefs_init)) {
   coefs_init <- runif(ncol(design), min = -3, max = 3)
  }

  optim_res <- optim(coefs_init, neg_loglik, neg_loglik_deriv, design = design,
                     response = response, ...)

  converged <- optim_res$convergence == 0
  if (!converged) {
    warning("The optimization algorithm for the fitting procedure did not converge!")
  }

  coefs_fitted <- optim_res$par
  response_fitted <- 1 / (1 + exp(design %*% coefs_fitted))
  originial_data = cbind(design, response)
  result <- list("coefficients" = coefs_fitted, "fitted" = response_fitted, "data" = originial_data)
  result
}


