#' @name save_model_output
#' @title Save Model Output to File with Timestamping and Format Handling
#'
#' @description
#' This function saves the output from `run_linear_models()` or a compatible list of model results to a `.RData` file. It supports saving either the full output (including summaries, residuals, and formulas) or just the fitted model objects. The function automatically appends a date-based timestamp to the filename (if not already present), ensures the correct file extension, and provides overwrite protection.
#'
#' If a single model object (e.g., `lm`, `lmerMod`) is provided, it will be wrapped in a named list (`model1`) to ensure compatibility. Generic lists are also accepted, though a warning will be issued if the input is not from `run_linear_models()`.
#'
#' @param model_output A model object or list of model results, ideally from `run_linear_models()`. Must be a list or a model object that can be wrapped in a list.
#' @param file_path A character string specifying the file path to save the output. If `.RData` is not included, it will be appended automatically.
#' @param models_only Logical. If `TRUE`, only the fitted model objects (e.g., `lm`, `lmerMod`) are saved. This is useful if you only need to re-run predictions or diagnostics later. If `FALSE` (default), the full output is saved — including model summaries, residuals, and formulas — which is recommended for most users.
#' @param verbose Logical. If `TRUE` (default), prints styled messages summarizing the save operation and suggesting next steps.
#' @param overwrite Logical. If `FALSE` (default), the function will stop if the file already exists. Set to `TRUE` to allow overwriting.
#'
#' @return (Invisibly) A list containing:
#' \describe{
#'   \item{object}{The object that was saved (either full output or model-only list).}
#'   \item{path}{The full file path where the object was saved.}
#' }
#'
#' @examples
#' \dontrun{
#' # Run and save model output
#' results <- run_linear_models(data = mtcars,
#'                              outcome = "mpg",
#'                              exposure = "cyl")
#'
#' save_model_output(results, "model_results")
#'
#' # Save only the fitted model objects
#' save_model_output(results, "models_only", models_only = TRUE)
#'
#' # Overwrite an existing file
#' save_model_output(results, "model_results", overwrite = TRUE)
#'
#' # Save a single model directly
#' single_model <- lm(mpg ~ cyl + wt, data = mtcars)
#' save_model_output(single_model, "single_model_output")
#' }
#'
#' @export
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_info cli_text
#' @importFrom stats lag setNames

utils::globalVariables(c("direction", "effect", "lag"))

save_model_output <- function(model_output,
                              file_path,
                              models_only = FALSE,
                              verbose = TRUE,
                              overwrite = FALSE) {
  if (!is.list(model_output) || inherits(model_output, "lm") || inherits(model_output, "lmerMod")) {
    model_output <- list(model1 = model_output)
    if (verbose) {
      cli::cli_alert_info("You provided a single model. It has been saved as part of a list so everything works smoothly.")
    }
  }

  if (!inherits(model_output, "run_model_result_list")) {
    if (verbose) {
      cli::cli_alert_warning("The object is not from run_linear_models(). Proceeding with a generic list.")
    }
  }

  if (!grepl("\\.RData$", file_path)) {
    file_path <- paste0(file_path, ".RData")
  }

  if (!grepl("\\d{8}", file_path)) {
    timestamp <- format(Sys.Date(), "%Y%m%d")
    file_path <- sub("(\\.RData)$", paste0("_", timestamp, "\\1"), file_path)
  }

  if (file.exists(file_path) && !overwrite) {
    stop("File already exists. Use `overwrite = TRUE` to overwrite.")
  }

  object_to_save <- if (models_only) {
    setNames(
      lapply(model_output, `[[`, "model"),
      names(model_output)
    )
  } else {
    model_output
  }

  save(object_to_save, file = file_path)

  if (verbose) {
    cli::cli_alert_success("Your model output has been saved successfully!")
    cli::cli_text("File location: {.file {file_path}}")

    cli::cli_text("\nSummary of what you just did:")
    cli::cli_text("- You saved {length(model_output)} model{if (length(model_output) > 1) 's' else ''}.")
    cli::cli_text("- You chose to {if (models_only) 'save only the fitted model objects (for reuse or diagnostics).' else 'save the full output, including summaries, residuals, and formulas.'}")

    cli::cli_text("\nSuggested next steps:")
    cli::cli_text("1. Load your saved output later using: `load(\"{file_path}\")`")
    cli::cli_text("2. Inspect model results (e.g., estimates, confidence intervals)")
    cli::cli_text("3. Run diagnostics (e.g., residual plots, influence checks)")
    cli::cli_text("4. Visualize results (e.g., forest plots, interaction effects)")
    cli::cli_text("5. Export summaries to CSV or Excel for reporting")
  }

  invisible(list(object = object_to_save, path = file_path))
}
