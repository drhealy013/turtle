test_that("save_model_output saves full output with timestamp", {
  # Create a mock model output
  model <- lm(mpg ~ cyl, data = mtcars)
  model_output <- list(model1 = list(model = model, tidy = broom::tidy(model)))

  # Create a temp file path
  temp_file <- tempfile(pattern = "test_model_output_", fileext = ".RData")

  # Run the function
  result <- save_model_output(model_output, temp_file, verbose = FALSE, overwrite = TRUE)

  # Check that file exists
  expect_true(file.exists(result$path))

  # Check that the saved object is a list
  loaded <- new.env()
  load(result$path, envir = loaded)
  expect_true(is.list(loaded$object_to_save))
})

test_that("save_model_output saves only model objects when models_only = TRUE", {
  model <- lm(mpg ~ cyl, data = mtcars)
  model_output <- list(model1 = list(model = model, tidy = broom::tidy(model)))

  temp_file <- tempfile(pattern = "test_models_only_", fileext = ".RData")

  result <- save_model_output(model_output, temp_file, models_only = TRUE, verbose = FALSE, overwrite = TRUE)

  loaded <- new.env()
  load(result$path, envir = loaded)

  # Should only contain model objects
  expect_true(all(sapply(loaded$object_to_save, inherits, "lm")))
})

test_that("save_model_output handles single model input", {
  model <- lm(mpg ~ cyl, data = mtcars)
  temp_file <- tempfile(pattern = "test_single_model_", fileext = ".RData")

  result <- save_model_output(model, temp_file, verbose = FALSE, overwrite = TRUE)

  loaded <- new.env()
  load(result$path, envir = loaded)

  expect_true(is.list(loaded$object_to_save))
  expect_true(inherits(loaded$object_to_save$model1, "lm"))
})

test_that("save_model_output prevents overwrite unless specified", {
  model <- lm(mpg ~ cyl, data = mtcars)
  model_output <- list(model1 = list(model = model))

  timestamp <- format(Sys.Date(), "%Y%m%d")
  temp_file <- tempfile(pattern = "test_overwrite_", fileext = ".RData")
  temp_file <- sub("(\\.RData)$", paste0("_", timestamp, "\\1"), temp_file)

  save(model_output, file = temp_file)

  expect_error(
    save_model_output(model_output, temp_file, verbose = FALSE),
    "already exists"
  )
})

test_that("save_model_output appends .RData and timestamp if missing", {
  model <- lm(mpg ~ cyl, data = mtcars)
  model_output <- list(model1 = list(model = model))

  temp_file <- tempfile(pattern = "test_append_extension_")
  result <- save_model_output(model_output, temp_file, verbose = FALSE, overwrite = TRUE)

  expect_true(grepl("\\.RData$", result$path))
  expect_true(grepl("\\d{8}", result$path))
})
