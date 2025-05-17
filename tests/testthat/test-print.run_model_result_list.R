
test_that("print.run_model_result_list prints expected output", {
  # Create a mock list of run_model_result objects
  model1 <- list(tidy = NULL, residuals = NULL, model = NULL)
  model2 <- list(tidy = NULL, residuals = NULL, model = NULL)
  result_list <- list(
    "mpg&wt" = model1,
    "hp&drat" = model2
  )
  class(result_list) <- "run_model_result_list"

  # Capture printed output
  output <- capture.output(print(result_list))

  # Check that the list names are printed
  expect_true(any(grepl("mpg&wt", output)))
  expect_true(any(grepl("hp&drat", output)))

  # Check that usage instructions are printed
  expect_true(any(grepl("results\\[\\[\"outcome&exposure\"\\]\\]\\$tidy", output)))
  expect_true(any(grepl("results\\[\\[\"outcome&exposure\"\\]\\]\\$residuals", output)))
  expect_true(any(grepl("results\\[\\[\"outcome&exposure\"\\]\\]\\$model", output)))
})
