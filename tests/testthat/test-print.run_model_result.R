test_that("print.run_model_result prints expected output", {
  # Create a mock run_model_result object
  model <- lm(mpg ~ wt, data = mtcars)
  result <- list(
    model = model,
    tidy = broom::tidy(model),
    residuals = resid(model),
    formula = formula(mpg ~ wt),
    exposure = "wt"
  )
  class(result) <- "run_model_result"

  # Capture printed output
  output <- capture.output(print(result))

  # Check that key sections are present
  expect_true(any(grepl("Model Formula:", output)))
  expect_true(any(grepl("Tidy Results:", output)))
  expect_true(any(grepl("Residuals \\(first 10 shown\\):", output)))
  expect_true(any(grepl("\\$model", output)))
  expect_true(any(grepl("\\$tidy", output)))
  expect_true(any(grepl("\\$residuals", output)))
  expect_true(any(grepl("\\$formula", output)))
  expect_true(any(grepl("\\$exposure", output)))
})
