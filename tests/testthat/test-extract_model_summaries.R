mock_model_output <- list(
  model1 = list(
    tidy = tibble::tibble(
      term = c("dayrolling", "age"),
      estimate = c(0.5, -0.2),
      p.value = c(0.01, 0.20),
      conf.low = c(0.1, -0.5),
      conf.high = c(0.9, 0.1)
    )
  ),
  model2 = list(
    tidy = tibble::tibble(
      term = c("dayrolling", "sex"),
      estimate = c(-0.3, 0.1),
      p.value = c(0.04, 0.50),
      conf.low = c(-0.6, -0.2),
      conf.high = c(0.0, 0.4)
    )
  )
)
class(mock_model_output) <- "run_model_result_list"

test_that("extract_model_summaries returns a tibble with expected columns", {
  result <- extract_model_summaries(mock_model_output, verbose = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("model_name", "term", "estimate", "direction") %in% names(result)))
})

test_that("direction column is correctly assigned", {
  result <- extract_model_summaries(mock_model_output, verbose = FALSE)
  expect_equal(result$direction, ifelse(result$estimate > 0, "up", "down"))
})

test_that("exposure_filter filters terms correctly", {
  result <- extract_model_summaries(mock_model_output, exposure_filter = "sex", verbose = FALSE)
  expect_true(all(grepl("sex", result$term)))
})

test_that("p-value adjustment is applied correctly", {
  result <- extract_model_summaries(mock_model_output, p_adjust_method = "bonferroni", verbose = FALSE)
  expect_true("p_adjust" %in% names(result))
  expect_equal(result$p_adjust, p.adjust(result$p.value, method = "bonferroni"))
})

test_that("invalid input class throws an error", {
  expect_error(
    extract_model_summaries(list(a = 1), verbose = FALSE),
    class = "invalid_model_output"
  )
})

test_that("invalid p-value method throws an error", {
  expect_error(
    extract_model_summaries(mock_model_output, p_adjust_method = "invalid", verbose = FALSE),
    class = "invalid_p_adjust_method"
  )
})

