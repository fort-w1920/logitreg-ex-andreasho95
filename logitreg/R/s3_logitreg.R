#' Generic function for logistic regression models
#'
#' `logitreg` is a generic function for logistic regression models. It estimates the coefficients
#' given a design matrix and a response vector. The function invokes particular methods which depend
#' on the \code{\link[base]{class}} of the first argument.
#'
#' @inheritParams fit_logitreg
#'
#' @return A list of class `logitreg` containing the regression coefficients, the estimated probabilites P(Y = 1) and
#' the (original) data
#' @export
logitreg <- function(design = NULL, response = NULL, coefs_init = NULL, ...) UseMethod("logitreg")

#' @describeIn logitreg Default method for logitreg
#' @export
logitreg.default <- function(design, response, coefs_init = NULL, ...) {
  res <- fit_logitreg(design, response, coefs_init = coefs_init, ...)
  structure(res, class = "logitreg")
}

#' @describeIn logitreg Takes model formula and data.frame as arguments
#' @export
logitreg.formula <- function(design, response, coefs_init = NULL, ...) {
  checkmate::assert_formula(design)
  checkmate::assert_data_frame(response, any.missing = FALSE)
  response_frame <- model.frame(design, data = response)[[1L]]
  design <- model.matrix(design, data = response)
  res <- fit_logitreg(design, response_frame, coefs_init = coefs_init, ...)
  structure(res, class = "logitreg")
}


