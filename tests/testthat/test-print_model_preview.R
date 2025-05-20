test_that("print_model_preview prints expected output for single model", {
  mock_result <- list(
    "mpg&cyl" = list(
      formula = as.formula("mpg ~ cyl"),
      tidy = tibble::tibble(
        term = c("(Intercept)", "cyl"),
        estimate = c(37.885, -2.876),
        p.value = c(1e-10, 1e-5),
        conf.low = c(33.6, -3.5),
        conf.high = c(42.1, -2.2)
      )
    )
  )
  class(mock_result) <- "run_model_result_list"

  output <- capture.output(print_model_preview(mock_result))

  expect_true(any(grepl("Model Preview", output)))
  expect_true(any(grepl("mpg&cyl", output)))
  expect_true(any(grepl("Formula", output)))
  expect_true(any(grepl("estimate", output)))
  expect_true(any(grepl("p.value", output)))
  expect_true(any(grepl("conf.low", output)))
  expect_true(any(grepl("conf.high", output)))
})

test_that("print_model_preview handles multiple models", {
  mock_result <- list(
    "mpg&cyl" = list(
      formula = as.formula("mpg ~ cyl"),
      tidy = tibble::tibble(
        term = c("(Intercept)", "cyl"),
        estimate = c(37.885, -2.876),
        p.value = c(1e-10, 1e-5),
        conf.low = c(33.6, -3.5),
        conf.high = c(42.1, -2.2)
      )
    ),
    "mpg&hp" = list(
      formula = as.formula("mpg ~ hp"),
      tidy = tibble::tibble(
        term = c("(Intercept)", "hp"),
        estimate = c(30.0, -0.05),
        p.value = c(1e-8, 0.001),
        conf.low = c(25.0, -0.07),
        conf.high = c(35.0, -0.03)
      )
    )
  )
  class(mock_result) <- "run_model_result_list"

  output <- capture.output(print_model_preview(mock_result))

  expect_true(any(grepl("mpg&cyl", output)))
  expect_true(any(grepl("mpg&hp", output)))
  expect_equal(sum(grepl("Formula", output)), 2)
})
