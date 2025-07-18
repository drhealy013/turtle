test_that("plot_model_diagnostics returns expected structure", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- plot_model_diagnostics(model, model_name = "test_model", diagnostics = c("normality", "outliers"))

  expect_type(result, "list")
  expect_named(result, c("plot", "percentage_outliers", "model"))
  expect_s3_class(result$plot, "gtable")  # gridExtra::grid.arrange returns a gtable
  expect_s3_class(result$model, "lm")
  expect_true(is.numeric(result$percentage_outliers) || is.na(result$percentage_outliers))
})

test_that("plot_model_diagnostics handles lmerMod models with random effects", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("performance")

  model <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- plot_model_diagnostics(model, model_name = "mixed_model", diagnostics = c("random_effects"))

  expect_true(inherits(result$model, "lmerMod"))
})

