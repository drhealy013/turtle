test_that("print.run_model_result_list prints expected output", {
  dummy_tidy <- tibble::tibble(
    term = NA_character_,
    estimate = NA_real_,
    p.value = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    note = "Model failed",
    formula = "y ~ x"
  )

  model1 <- list(tidy = dummy_tidy, residuals = NA, model = NULL)
  model2 <- list(tidy = dummy_tidy, residuals = NA, model = NULL)
  result_list <- list(
    "mpg&wt" = model1,
    "hp&drat" = model2
  )
  class(result_list) <- "run_model_result_list"

  output <- capture.output(print(result_list))

  expect_true(any(grepl("mpg&wt", output)))
  expect_true(any(grepl("hp&drat", output)))

  expect_true(any(grepl("results\\[\\[\"outcome&exposure\"\\]\\]\\$tidy", output)))
  expect_true(any(grepl("results\\[\\[\"outcome&exposure\"\\]\\]\\$residuals", output)))
  expect_true(any(grepl("results\\[\\[\"outcome&exposure\"\\]\\]\\$model", output)))
  expect_true(any(sapply(result_list, function(x) grepl("Model failed", x$tidy$note))))
})
