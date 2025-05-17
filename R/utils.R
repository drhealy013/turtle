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
#' @examples
#' print_assignment_reminder("run_linear_models")
#'
#' @export

print_assignment_reminder <- function(function_name = "this function") {
  if (identical(sys.nframe(), 1L)) {
    message("\nNote: You ran `", function_name, "` without assigning the result to a variable.")
    message("To access model results later, assign the output like this:\n")
    message("  results <- ", function_name, "(...)\n")
    message("Then you can access the tidy results with:\n")
    message("  results$tidy  # for a single model\n")
    message("  results[[\"outcome&exposure\"]]$tidy  # for multiple models\n")
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
#' @examples
#' check_version_warning(packageVersion("turtle"))
#'
#' @export

check_version_warning <- function(current_version, latest_version = "0.1.6") {
  if (current_version < latest_version) {
    message("A newer version of turtle is available (", latest_version,
            "). Please reinstall from GitHub to get the latest updates.")
  }
}
