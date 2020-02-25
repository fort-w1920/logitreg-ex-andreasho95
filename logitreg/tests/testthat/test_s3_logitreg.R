context("S3 Class logitreg")
load(system.file("logitreg-data-trouble.Rdata", package = "logitreg"))
data_list <- sim_data(seed = 123, numerics = 1)
data_df <- sim_data(seed = 123, numerics = 1, dataframe = TRUE)

test_that("Default Method and Formula Method return correct coefficients", {
  res_logitreg_formula <- logitreg(response ~ ., data_df)
  res_logitreg_default <- logitreg(data_list$design, data_list$response)
  glm_model <- glm(response ~ 0 + design, family = binomial(link = 'logit'), data = data_list)
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
