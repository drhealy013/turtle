test_that("print.assignment_reminder prints reminder and preview", {
  mock_result <- structure(
    list(
      model1 = list(
        formula = as.formula("y ~ x"),
        tidy = data.frame(
          term = "x",
          estimate = 1.23,
          p.value = 0.045,
          conf.low = 0.5,
          conf.high = 2.0
        ),
        residuals = rnorm(10),
        model = lm(mpg ~ wt, data = mtcars),
        exposure = "x"
      )
    ),
    class = c("assignment_reminder", "run_model_result_list"),
    function_name = "run_linear_models",
    .test_force = TRUE
  )

  expect_output(print(mock_result), "You ran `run_linear_models\\(\\)` without assigning the result")
  expect_output(print(mock_result), "results <- run_linear_models")
  expect_output(print(mock_result), "Model Preview:")
  expect_output(print(mock_result), "--- model1 ---")
  expect_output(print(mock_result), "term.*estimate.*p.value")
})

test_that("print.assignment_reminder handles empty result list", {
  empty_result <- structure(
    list(),
    class = c("assignment_reminder", "run_model_result_list")
  )
  attr(empty_result, "function_name") <- "run_linear_models"
  attr(empty_result, ".test_force") <- TRUE

  expect_output(print(empty_result), "No models to preview")
})

test_that("print.assignment_reminder handles missing components", {
  broken_result <- list(
    model1 = list(model_name = "model1")
  )
  class(broken_result) <- c("assignment_reminder", "run_model_result_list")
  attr(broken_result, "function_name") <- "run_linear_models"
  attr(broken_result, ".test_force") <- TRUE

  expect_output(print(broken_result), "Formula: (NULL|\\(missing\\))")
  expect_output(print(broken_result), "Tidy summary: \\(missing.*\\)")
})
