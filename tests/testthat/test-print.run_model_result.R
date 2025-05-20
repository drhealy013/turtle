test_that("print.run_model_result prints expected output", {
  model <- lm(mpg ~ wt, data = mtcars)
  result <- list(
    model = model,
    tidy = broom::tidy(model),
    residuals = resid(model),
    formula = formula(mpg ~ wt),
    exposure = "wt"
  )
  class(result) <- "run_model_result"

  output <- capture.output(print(result))

  expect_true(any(grepl("Model Formula:", output)))
  expect_true(any(grepl("Tidy Results:", output)))
  expect_true(any(grepl("Residuals \\(first 10 shown\\):", output)))
  expect_true(any(grepl("\\$model", output)))
  expect_true(any(grepl("\\$tidy", output)))
  expect_true(any(grepl("\\$residuals", output)))
  expect_true(any(grepl("\\$formula", output)))
  expect_true(any(grepl("\\$exposure", output)))
})
