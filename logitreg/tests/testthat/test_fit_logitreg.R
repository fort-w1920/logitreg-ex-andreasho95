context("Function fit_logitreg")
load(system.file("logitreg-data-trouble.Rdata", package = "logitreg"))
data <- sim_data(seed = 123, numerics = 1)

test_that("Returns correct coefficients for correct input", {

  res_fit_logitreg <- fit_logitreg(data$design, data$response)
  glm_model <- glm(response ~ 0 + design, family = binomial(link = 'logit'), data = data)
  coefs_fit_logitreg <- signif(res_fit_logitreg$coefficients, 2)
  coefs_glm_model <- signif(unname(glm_model$coefficients), 2)

  expect_equal(coefs_fit_logitreg, coefs_glm_model)
})


test_that("Correct handling of missing values", {
  expect_error(fit_logitreg(trouble1$x, trouble1$y, trouble1$b), "missing values")
  expect_error(fit_logitreg(data$design, data$response, c(NA, NA)), "missing values")
  expect_error(fit_logitreg(data$design, rep(NA, 1500)), "missing values")
  expect_error(fit_logitreg(data$design, NA), "length 1500")
})

test_that("Returning warning message when optimization algorithm did not converve", {
  expect_warning(fit_logitreg(trouble2$x, trouble2$y, trouble2$b, method = "BFGS"), "not converge")
})

# Checks for perfect linear separability
