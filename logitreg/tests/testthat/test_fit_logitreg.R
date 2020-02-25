context("Function fit_logitreg")

test_that("Returns correct coefficients for correct input", {
  data <- sim_data(seed = 123, numerics = 1)
  res_fit_logitreg <- fit_logitreg(data$design, data$response)
  glm_model <- glm(response ~ 0 + design, family = binomial(link = 'logit'), data = data)
  coefs_fit_logitreg <- signif(res_fit_logitreg$coefficients, 2)
  coefs_glm_model <- signif(unname(glm_model$coefficients), 2)

  expect_equal(coefs_fit_logitreg, coefs_glm_model)
})
