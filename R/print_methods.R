#' Print Method for run_model_result Objects
#'
#' @description
#' This method defines how objects of class `run_model_result` are printed in the console.
#' It displays the model formula, a tidy summary of fixed effects, and the first 10 residuals.
#' It also provides guidance on how to access individual components of the result.
#'
#' @param x An object of class `run_model_result`, typically returned by `run_linear_models()` when a single model is fitted.
#' @param ... Additional arguments (not used).
#'
#' @return No return value. This function is called for its side effect: printing a formatted summary to the console.
#'
#' @examples
#' \dontrun{
#' result <- run_linear_models(data, outcome = "y", exposure = "x")
#' result  # triggers this print method
#' }
#'
#' @export
print.run_model_result <- function(x, ...) {
  cat("Model Formula:\n")
  print(x$formula)

  cat("\nTidy Results:\n")
  print(x$tidy)

  cat("\nResiduals (first 10 shown):\n")
  print(utils::head(x$residuals, 10))

  cat("\nTo access components directly:\n")
  cat("  $model      # full model object\n")
  cat("  $tidy       # tidy summary of fixed effects\n")
  cat("  $residuals  # model residuals\n")
  cat("  $formula    # model formula\n")
  cat("  $exposure   # exposure variable used\n")
}

#' Print Method for run_model_result_list Objects
#'
#' @description
#' This method defines how objects of class `run_model_result_list` are printed in the console.
#' It lists the names of all stored models and provides guidance on how to access individual model results.
#'
#' @param x An object of class `run_model_result_list`, typically returned by `run_linear_models()` when multiple models are fitted.
#' @param ... Additional arguments (not used).
#'
#' @return No return value. This function is called for its side effect: printing a formatted summary to the console.
#'
#' @examples
#' \dontrun{
#' results <- run_linear_models(data, outcome = c("y1", "y2"), exposure = c("x1", "x2"))
#' results  # triggers this print method
#' }
#'
#' @export
print.run_model_result_list <- function(x, ...) {
  cat("List of model results:\n")
  cat(paste0("  - ", names(x)), sep = "\n")

  cat("\nTo access a specific model:\n")
  cat("  results[[\"outcome&exposure\"]]$tidy       # tidy summary\n")
  cat("  results[[\"outcome&exposure\"]]$residuals  # residuals\n")
  cat("  results[[\"outcome&exposure\"]]$model      # full model object\n")
}
