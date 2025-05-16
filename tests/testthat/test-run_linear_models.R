test_that("run_linear_models returns tidy output", {
  data(mtcars)
  result <- run_linear_models(data = mtcars, outcome = "mpg", exposure = "wt")
  expect_type(result, "list")
  expect_true("tidy" %in% names(result))
  expect_s3_class(result$tidy, "tbl_df")
})
