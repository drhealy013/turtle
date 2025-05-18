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
#'
#' @details
#' This function does not print anything itself. Instead, it returns a character vector containing
#' the lines of the reminder message. It is intended for internal use by functions that want to
#' provide assignment guidance to users.
#'
#' @return A character vector containing the lines of the reminder message.
#'
#' @keywords internal
#' @noRd

generate_assignment_reminder <- function(function_name = "this function") {
  c(
    paste0("\nNote: You ran `", function_name, "` without assigning the result to a variable."),
    "To access model results later, assign the output like this:\n",
    paste0(" results <- ", function_name, "(...)\n"),
    "Then you can access the tidy results with:\n",
    " results$tidy # for a single model\n",
    " results[[\"outcome&exposure\"]]$tidy # for multiple models\n"
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
#' explaining how to assign the result and how to access key components like `$tidy`.
#'
#' @return No return value. This function is called for its side effect: printing a message to the console.
#'
#' @keywords internal
#' @noRd

print_assignment_reminder <- function(function_name = "this function") {
  if (identical(sys.nframe(), 1L)) {
    message_lines <- generate_assignment_reminder(function_name)
    for (line in message_lines) message(line)
  }
}

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

check_version_warning <- function(current_version, latest_version = "0.1.7") {
  current_version <- as.character(current_version)
  latest_version <- as.character(latest_version)

  if (utils::compareVersion(current_version, latest_version) < 0) {
    message("A newer version of turtle is available (", latest_version,
            "). Please reinstall from GitHub to get the latest updates.")
  }
}

