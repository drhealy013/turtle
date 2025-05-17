test_that("tidy_model_output returns a tibble with expected columns", {
  model <- lm(mpg ~ wt, data = mtcars)
  result <- tidy_model_output(
    model,
    warnings = NULL,
    messages = NULL,
    effect_modifier = NULL,
    sensitivity_cov = NULL
  )

  expect_s3_class(result, "tbl_df")

  required_cols <- c("term", "estimate", "conf.low", "conf.high", "std.error", "n_obs", "BIC", "error")

  expect_true(all(required_cols %in% names(result)))

  if ("p.value" %in% names(result)) {
    expect_type(result$p.value, "double")
  }
})
