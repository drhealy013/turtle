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

test_that("fit_model_safely captures a manually triggered warning", {
  noisy_model <- function(formula, data) {
    warning("This is a test warning")
    lm(formula, data)
  }

  data <- data.frame(y = rnorm(10), x = rnorm(10))
  result <- fit_model_safely(y ~ x, data = data, model_fun = noisy_model)

  print(result$warnings)  # optional: for debugging
  expect_true(length(result$warnings) > 0)
  expect_match(result$warnings[[1]], "test warning")
})

test_that("fit_model_safely captures messages", {
  model_fun_with_message <- function(formula, data) {
    message("This is a test message")
    lm(formula, data)
  }

  result <- fit_model_safely(mpg ~ wt, mtcars, model_fun_with_message)

  expect_true(length(result$messages) > 0)
  expect_match(result$messages[[1]], "test message")
})
