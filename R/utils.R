#' Check for Package Version Warning
#'
#' @name check_version_warning
#'
#' @description
#' This utility function checks whether the current version of the package is older than a specified latest version.
#' If so, it prints a message encouraging the user to update.
#'
#' @param current_version The current version of the package, typically from `packageVersion("yourpkg")`.
#' @param latest_version A character string indicating the latest available version (default is `"0.1.5"`).
#'
#' @details
#' This function is useful for notifying users when they may be using an outdated version of the package.
#' It is intended to be called at the beginning of user-facing functions.
#'
#' @return No return value. Called for its side effect: printing a message to the console.
#'
#' @keywords internal
#' @noRd

check_version_warning <- function(current_version, latest_version = "0.1.8") {
  current_version <- as.character(current_version)
  latest_version <- as.character(latest_version)

  if (utils::compareVersion(current_version, latest_version) < 0) {
    message("A newer version of turtle is available (", latest_version,
            "). Please reinstall from GitHub to get the latest updates.")
  }
}

#' Generate Assignment Reminder Message
#'
#' @name generate_assignment_reminder
#'
#' @description
#' This internal helper function constructs a reminder message to inform users that they should assign
#' the output of a function to a variable. It is used by `print_assignment_reminder()` to generate
#' consistent and informative guidance.
#'
#' @param function_name A string indicating the name of the function being run (e.g., `"run_linear_models"`).
#'   This name will be inserted into the message to personalize the reminder.
#' @param test_mode Logical. If `TRUE`, the function returns a simplified, plain-text version of the message
#'   suitable for automated testing. Defaults to `FALSE`.
#'
#' @details
#' This function does not print anything itself. Instead, it returns a character vector containing
#' the lines of the reminder message. It is intended for internal use by functions that want to
#' provide assignment guidance to users.
#'
#' When `test_mode = TRUE`, the returned message omits CLI styling and formatting to ensure compatibility
#' with unit tests that rely on pattern matching.
#'
#' @return A character vector containing the lines of the reminder message.
#'
#' @keywords internal
#' @noRd

generate_assignment_reminder <- function(function_name = "this function", test_mode = FALSE) {
  if (test_mode) {
    return(c(paste("NOTE:"),
             paste0("You ran `", function_name, "()` without assigning the result."),
             paste0("results <- ", function_name, "(...)")))
  }

  c(
    cli::rule(left = cli::style_bold(cli::col_red("🔸 NOTE"))),
    cli::col_yellow(paste0("You ran `", function_name, "()` without assigning the result.")),
    "",
    "To keep your model results for later use, assign the output like this:",
    cli::col_green(paste0("  results <- ", function_name, "(...)")),
    "",
    "Then access model components like:",
    cli::col_blue('  results[["your_outcome&your_exposure"]]$tidy'),
    "",
    cli::rule()
  )
}

#' Print a Reminder to Assign Function Output
#'
#' @name print_assignment_reminder
#'
#' @description
#' This utility function checks whether a function was called without assigning its output to a variable.
#' If so, it prints a helpful reminder to the user, encouraging them to assign the result so they can access it later.
#' This is especially useful for guiding users who are less familiar with R.
#'
#' @param function_name A string indicating the name of the function being run (e.g., `"run_linear_models"`).
#'   This will be used in the printed message to personalize the guidance.
#'
#' @details
#' This function is intended to be called at the end of user-facing functions. It uses `sys.nframe()` to determine
#' whether the function was called at the top level (i.e., not assigned to a variable). If so, it prints a message
#' explaining how to assign the result and how to access key components like `$tidy`. An internal "force" parameter was
#' added to make it testable to ensure long-term maintainability.
#'
#' @return No return value. This function is called for its side effect: printing a message to the console.
#'
#' @keywords internal
#' @noRd

print_assignment_reminder <- function(function_name = NULL, ..., .test_force = NULL) {
  if (is.null(function_name)) {
    calls <- sys.calls()
    for (i in seq(length(calls), 1)) {
      call_i <- calls[[i]]
      fname <- as.character(call_i[[1]])
      if (!fname %in% c("print_assignment_reminder", "capture_messages", "with_options")) {
        function_name <- fname
        break
      }
    }
    if (is.null(function_name)) function_name <- "this function"
  }

  .test_force <- !identical(.test_force %||% getOption("turtle.test_force", NULL), FALSE)
  show_reminder <- .test_force

  if (show_reminder) {
    message_lines <- generate_assignment_reminder(function_name, test_mode = !.test_force)
    if (length(message_lines) > 0) {
      cli::cat_line("")
      cli::cat_line(message_lines)
    }
  }
}

#' Print Preview of Model Results
#'
#' @description
#' This helper function provides a concise, user-friendly preview of model results
#' when `run_linear_models()` is called without assignment. It displays the model formula
#' and a tidy summary of key statistics (e.g., estimates and p-values) for each model.
#'
#' This is intended to reduce console clutter and guide users toward assigning the result
#' for full access to model components.
#'
#' @param result_list An object of class `run_model_result_list`, typically returned by
#'   `run_linear_models()`.
#'
#' @return No return value. This function is called for its side effect: printing a
#'   formatted preview of model results to the console.
#'
#' @keywords internal
#' @noRd

print_model_preview <- function(result_list) {
  cat("Model Preview:\n")
  for (name in names(result_list)) {
    cat("\n---", name, "---\n")
    cat("Formula: ")
    print(result_list[[name]]$formula)

    tidy_df <- result_list[[name]]$tidy
    if (!is.null(tidy_df) && is.data.frame(tidy_df)) {
      cat("\nTidy Summary:\n")
      print(
        tidy_df %>%
          dplyr::select(dplyr::any_of(c("term", "estimate", "conf.low", "conf.high", "p.value")))
      )
    } else {
      cat("Tidy summary: (missing or invalid)\n")
    }
  }
}
