#' @export
print.run_model_result <- function(x, ...) {
  message("Model run complete. The returned object is a list. Access the summary table with `$tidy`, e.g., `my_model$tidy`.")
  message("Note: Since you did not assign the result to a variable, components like residuals will not be accessible later.")
  print(x$tidy)
  invisible(x)
}

#' @export
print.run_model_result_list <- function(x, ...) {
  message("Model run complete. The returned object is a list of models. Each element contains `$tidy`, `$model`, and `$residuals`.")
  message("Note: Since you did not assign the result to a variable, these components will not be accessible later.")
  print(names(x))
  invisible(x)
}
