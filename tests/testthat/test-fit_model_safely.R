library(testthat)

test_that("fit_model_safely returns expected structure and fits model correctly", {
  result <- fit_model_safely(mpg ~ wt, data = mtcars, model_fun = lm)

  # Check that the result is a list with expected components
  expect_type(result, "list")
  expect_named(result, c("result", "warnings", "messages"))

  # Check that the model fitting result is a list with result and error
  expect_type(result$result, "list")
  expect_true(all(c("result", "error") %in% names(result$result)))

  # Check that the model was fitted successfully
  expect_s3_class(result$result$result, "lm")
  expect_null(result$result$error)

  # Check that warnings and messages are character vectors
  expect_type(result$warnings, "character")
  expect_type(result$messages, "character")
})
