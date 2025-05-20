library(mockery)
data <- mtcars
data$cyl <- as.factor(data$cyl)

test_that("run_linear_models returns tidy output", {
  result <- run_linear_models(data = data, outcome = "mpg", exposure = "wt")
  model <- result[[1]]
  expect_type(result, "list")
  expect_true("tidy" %in% names(model))
  expect_s3_class(model$tidy, "tbl_df")
})

test_that("run_linear_models fits a mixed model when random_effects is specified", {
  skip_if_not_installed("lmerTest")
  library(lmerTest)
  data(sleepstudy, package = "lme4")

  result <- run_linear_models(
    data = sleepstudy,
    outcome = "Reaction",
    exposure = "Days",
    random_effects = "(1 | Subject)",
    p_values = TRUE,
    verbose = FALSE
  )

  model <- result[[1]]$model
  expect_false(is.null(model))
  expect_true(inherits(model, c("lmerModLmerTest", "lmerMod")))
})

test_that("run_linear_models handles effect modifiers", {
  result <- run_linear_models(
    data = data,
    outcome = "mpg",
    exposure = "wt",
    effect_modifier = "am"
  )
  model <- result[[1]]
  expect_true(any(grepl("wt:am", model$tidy$term)))
})

test_that("run_linear_models handles multiple outcomes and exposures", {
  result <- run_linear_models(
    data = data,
    outcome = c("mpg", "hp"),
    exposure = c("wt", "cyl"),
    verbose = FALSE
  )
  expect_type(result, "list")
  expect_true(all(c("mpg&wt", "mpg&cyl", "hp&wt", "hp&cyl") %in% names(result)))
})

test_that("run_linear_models handles model fitting errors gracefully", {
  result <- run_linear_models(
    data = data,
    outcome = "mpg",
    exposure = "nonexistent_variable"
  )
  model <- result[[1]]
  expect_true("note" %in% names(model$tidy))
})

test_that("run_linear_models includes sensitivity covariates", {
  result <- run_linear_models(
    data = data,
    outcome = "mpg",
    exposure = "wt",
    sensitivity_cov = c("hp", "qsec")
  )
  model <- result[[1]]
  expect_true(any(c("hp", "qsec") %in% model$tidy$term))
})

test_that("run_linear_models includes covariates", {
  result <- run_linear_models(
    data = data,
    outcome = "mpg",
    exposure = "wt",
    covariates = c("hp", "qsec")
  )
  model <- result[[1]]
  expect_true(all(c("hp", "qsec") %in% model$tidy$term))
})

test_that("run_linear_models works with named vectors", {
  outcomes <- c("mpg", "hp")
  names(outcomes) <- outcomes
  exposures <- c("wt", "cyl")
  names(exposures) <- exposures
  result <- run_linear_models(
    data = data,
    outcome = outcomes,
    exposure = exposures,
    verbose = FALSE
  )
  expect_true("hp&cyl" %in% names(result))
})

test_that("throws error if lmerTest is not installed and p_values = TRUE", {
  mock_require <- mock(FALSE)
  stub(run_linear_models, "requireNamespace", mock_require)

  expect_error(
    run_linear_models(
      data = mtcars,
      outcome = "mpg",
      exposure = "wt",
      random_effects = "cyl",  # triggers mixed model
      p_values = TRUE
    ),
    "lmerTest"
  )
})

test_that("run_linear_models works without lmerTest when p_values = FALSE", {
  skip_if_not_installed("lme4")

  result <- run_linear_models(
    data = data,
    outcome = "mpg",
    exposure = "hp",
    random_effects = "(1 | cyl)",
    p_values = FALSE
  )

  model <- result[[1]]$model
  expect_false(is.null(model))
  expect_true(inherits(model, "lm") || inherits(model, "lmerMod"),
              info = "Model should be an S3 object of class 'lm' or 'lmerMod'")
})

test_that("model failure returns tidy with error message", {
  result <- run_linear_models(outcome = "nonexistent", exposure = "hp", data = data)
  model <- result[[1]]
  expect_true(any(!is.na(model$tidy$note)))
  expect_true(any(is.na(model$tidy$estimate)))
})

test_that("model grid expands correctly", {
  result <- run_linear_models(
    outcome = c("mpg", "disp"),
    exposure = c("hp", "wt"),
    effect_modifier = c("cyl", "gear"),
    data = data,
    return_grid = TRUE
  )
  expect_equal(nrow(attr(result, "model_grid")), 8)
})

test_that("verbose mode runs without error", {
  expect_message(run_linear_models(outcome = "mpg", exposure = "hp", data = data, verbose = TRUE))
})

test_that("result has correct class for multiple models", {
  result <- run_linear_models(outcome = c("mpg", "hp"), exposure = "wt", data = data)
  expect_s3_class(result, "run_model_result_list")
})

test_that("result has correct class for single model", {
  result <- run_linear_models(outcome = "mpg", exposure = "wt", data = data)
  expect_s3_class(result, "run_model_result_list")
})

test_that("run_linear_models does not trigger formula.character warning", {
  expect_warning(
    run_linear_models(
      data = data,
      outcome = c("mpg", "disp"),
      exposure = c("wt", "cyl"),
      verbose = FALSE
    ),
    regexp = NA
  )
})

test_that("formulas are valid and scalar-safe", {
  result <- run_linear_models(
    data = data,
    outcome = c("mpg", "disp"),
    exposure = c("wt", "cyl"),
    verbose = FALSE
  )

  formulas <- purrr::map_chr(result, ~ deparse(.x$formula))
  expect_true(all(grepl("~", formulas)))
  expect_true(all(nchar(formulas) > 0))
})

test_that("residuals are returned and numeric", {
  result <- run_linear_models(data = data, outcome = "mpg", exposure = "wt")
  residuals <- result[[1]]$residuals
  expect_type(residuals, "double")
  expect_length(residuals, nrow(data))
})

test_that("model names are unique and descriptive", {
  result <- run_linear_models(
    data = data,
    outcome = c("mpg", "disp"),
    exposure = c("wt", "cyl"),
    effect_modifier = "am",
    sensitivity_cov = "hp",
    verbose = FALSE
  )
  model_names <- names(result)
  expect_equal(length(model_names), length(unique(model_names)))
  expect_true(all(grepl("&", model_names)))
})

test_that("non-character outcome is coerced to character", {
  result <- run_linear_models(
    data = mtcars,
    outcome = quote(mpg),
    exposure = "wt"
  )
  expect_s3_class(result, "run_model_result_list")
})

test_that("non-character exposure is coerced to character", {
  result <- run_linear_models(
    data = mtcars,
    outcome = "mpg",
    exposure = quote(wt)
  )
  expect_s3_class(result, "run_model_result_list")
})

test_that("assignment reminder and preview only print when not assigned", {
  output <- withr::with_options(
    list(cli.default_output = "message"),
    {
      messages <- capture_messages({
        result <- capture.output(
          run_linear_models(
            data = data,
            outcome = "mpg",
            exposure = "wt",
            verbose = TRUE,
            .internal = list(.test_force = TRUE)
          )
        )
        output <- capture.output(print(result))
        output
      })
      c(messages, output)
    }
  )

  output <- cli::ansi_strip(output)

  cat("\n--- Captured Output ---\n", paste(output, collapse = "\n"), "\n------------------------")

  expect_true(any(stringr::str_detect(output, stringr::regex("assign.*result", ignore_case = TRUE))))
  expect_true(any(stringr::str_detect(output, stringr::regex("results\\s*<-\\s*run_linear_models\\(", ignore_case = TRUE))))
  expect_true(any(stringr::str_detect(output, stringr::regex("Model Preview", ignore_case = TRUE))))
})
