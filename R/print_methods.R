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
#' @method print run_model_result
#' @exportS3Method
#' @noRd

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
#' @method print run_model_result_list
#' @exportS3Method
#' @noRd

print.run_model_result_list <- function(x, ...) {
  .test_force <- isTRUE(attr(x, ".test_force", exact = TRUE))

  if (interactive() || .test_force) {
    print_model_preview(x)
    print_assignment_reminder(
      function_name = attr(x, "function_name", exact = TRUE),
      .test_force = .test_force
    )
  }

  cat("List of model results:\n")
  cat(paste0("  - ", names(x)), sep = "\n")
  cat("\nTo access a specific model:\n")
  cat("  results[[\"outcome&exposure\"]]$tidy       # tidy summary\n")
  cat("  results[[\"outcome&exposure\"]]$residuals  # residuals\n")
  cat("  results[[\"outcome&exposure\"]]$model      # full model object\n")

  invisible(x)
}

#' Print Reminder and Preview for Unassigned Model Results
#'
#' @description
#' This S3 method is automatically triggered when the result of `run_linear_models()` is
#' printed without being assigned. It displays a reminder message encouraging assignment,
#' followed by a concise preview of the model results using `print_model_preview()`.
#'
#' The reminder helps users understand that full model details are only accessible when
#' the result is assigned to a variable. The preview includes model formulas and a tidy
#' summary of key statistics (e.g., estimates and p-values).
#'
#' This method is only invoked interactively or when `.test_force = TRUE` is set for testing.
#'
#' @param x An object of class `assignment_reminder`, typically returned by
#'   `run_linear_models()` when the result is not assigned.
#' @param ... Additional arguments passed to or from other methods (currently unused).
#'
#' @return No return value. This function is called for its side effect: printing a
#'   reminder message and a formatted preview of model results to the console.
#'
#' @method print assignment_reminder
#' @exportS3Method
#' @noRd

print.assignment_reminder <- function(x, ...) {
  print_assignment_reminder(
    function_name = attr(x, "function_name", exact = TRUE),
    .test_force = attr(x, ".test_force", exact = TRUE)
  )

  if (length(x) == 0) {
    cat("\n(No models to preview - result list is empty.)\n")
  } else {
    tryCatch({
      print_model_preview(x)
    }, error = function(e) {
      cat("\n[!] Unable to print model preview due to an error:\n")
      message(conditionMessage(e))
    })
  }

  invisible(x)
}
