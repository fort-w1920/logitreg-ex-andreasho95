context("S3 Class logitreg")
# Load data
load(system.file("logitreg-data-trouble.Rdata", package = "logitreg"))
data_list <- sim_data(seed = 123, numerics = 1)
data_df <- sim_data(seed = 123, numerics = 1, dataframe = TRUE)
# Run logistic regressions
res_logitreg_formula <- logitreg(response ~ ., data_df)
res_logitreg_default <- logitreg(data_list$design, data_list$response)
glm_model <- glm(response ~ 0 + design, family = binomial(link = "logit"), data = data_list)

test_that("Default Method and Formula Method return correct coefficients", {
  coefs_formula <- signif(res_logitreg_formula$coefficients, 2)
  coefs_default <- signif(res_logitreg_default$coefficients, 2)
  coefs_glm_model <- signif(unname(glm_model$coefficients), 2)
  expect_equal(coefs_formula, coefs_default)
  expect_equal(coefs_formula, coefs_glm_model)
  expect_true("logitreg" %in% class(res_logitreg_formula))
  expect_true("logitreg" %in% class(res_logitreg_default))
})

test_that("Formula Method handles incorrect input correctly", {
  expect_error(logitreg(response ~ ., c(1:10)), "data.frame")
})

context("S3 Class logitreg methods")
test_that("predict.logitreg method works correctly if is new_data is ommited", {
  res_predict <- predict(res_logitreg_default)
  expect_true(is.vector(res_predict))
  expect_identical(res_predict, as.vector(res_logitreg_default$fitted))
})

test_that("predict.logitreg method works correctly if correct formatted new_data is obtained", {
  new_sim_data <- sim_data(n = 100, seed = 123, numerics = 1)$design
  res_predict <- predict(res_logitreg_default, new_sim_data)
  expect_true(is.vector(res_predict))
  expect_true(length(res_predict) == 100)
  res_predict2 <- predict(res_logitreg_default, as.matrix(data_list$design[1:5, ]))
  fitted_5 <- res_logitreg_default$fitted[1:5]
  expect_identical(res_predict2, as.vector(fitted_5))
  res_predict3 <- predict(res_logitreg_default, data_list$design[1:5, ])
  expect_identical(res_predict2, as.vector(fitted_5))
})

test_that("predict.logitreg method gives meaningful error message if new_data cannot be
          coerced to matrix", {
  expect_error(predict(res_logitreg_default, baseenv()),
               "Cannot coerce type `environment` of new_data to matrix")
  new_data_wrong <- sim_data(n = 100, seed = 123, numerics = 8)$design
  expect_error(predict(res_logitreg_default, new_data_wrong),
               "equal to the number of coefficients")
  expect_error(predict(res_logitreg_default, iris[,c(4,5)]),
               "numeric matrix")
})

context("S3 Class logitreg methods")
test_that("fitted.logitreg method works correctly", {
  fitted_values <- fitted(res_logitreg_default)
  expect_true(is.vector(fitted_values))
  expect_identical(fitted_values, as.vector(res_logitreg_default$fitted))
})
