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

# Methods ------------------------------------------------------------------------------------------

#' Method `predict` for class `logitreg`
#'
#' Obtains predictions for new data from fitted logistic regression model object.
#'
#' @param object     object of class `logitreg`.
#' @param newdata    optionally,  a data frame in which to look for variables with which to predict.
#' If omitted, the fitted predictors are used.#'
#' @param ...        further arguments passed to or from other methods.
#'
#' @return a vector of predictions.
#' @importFrom       checkmate assert_data_frame assert_true
#' @importFrom       stats predict
#' @export
predict.logitreg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    fitted <- as.vector(object$fitted)
    return(fitted)
  }

  # Type and logical checks for newdata
  if (!is.matrix(newdata)) {
    newdata <- tryCatch({
      as.matrix(newdata)
    }, warning = function(warning_condition) {
      print(warning_condition)
    }, error = function(error_condition) {
      stop(paste0("Cannot coerce type `", typeof(newdata), "` of new_data to matrix"))
    })
  }

  if (ncol(newdata) != length(object$coefficients)) {
    stop("The number of columns of `newdata` has to be equal to the number of coefficients of the
         `logitreg` object")
  }

  is_numeric <- checkmate::test_numeric(newdata)
  if (!is_numeric) {
    stop("Newdata must be convertable to a numeric matrix. In the future we will improve this.")
  }

  predicted <- 1 / (1 + exp(newdata %*% object$coefficients))
  as.vector(predicted)
}

#' @export
fitted.logitreg <- function(object, ...) {
  as.vector(object$fitted)
}
