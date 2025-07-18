#' @name extract_model_summaries
#' @title Extract and Combine Tidy Model Summaries
#'
#' @description
#' This function extracts and combines tidy model summaries from the output of `run_linear_models()`. It supports optional p-value adjustment using standard multiple testing correction methods. The output is a single tidy tibble containing results from all models.
#'
#' @param model_output A named list of model results as returned by `run_linear_models()`.
#' @param p_adjust_method Optional character string specifying the method for p-value adjustment. Must be one of: `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, or `"none"`. If `NULL` (default), no adjustment is applied. Note: `"fdr"` is treated as an alias for `"BH"`.
#' @param verbose Logical. If `TRUE` (default), prints a summary of the extraction process.
#'
#' @return A tibble combining all tidy model summaries, with optional p-value adjustment. Includes a `direction` column indicating the sign of the estimate (`"up"` or `"down"`).
#'
#' @examples
#' \dontrun{
#' # Extract all model summaries
#' extract_model_summaries(model_output)
#'
#' # Apply Benjamini-Hochberg (FDR) correction
#' extract_model_summaries(model_output, p_adjust_method = "BH")
#'
#' # Apply Bonferroni correction
#' extract_model_summaries(model_output, p_adjust_method = "bonferroni")
#'
#' # Disable p-value adjustment explicitly
#' extract_model_summaries(model_output, p_adjust_method = "none")
#' }
#'
#' @export
#'
#' @importFrom dplyr bind_rows mutate relocate if_else
#' @importFrom purrr map
#' @importFrom stats p.adjust
#' @importFrom cli cli_h1 cli_alert_success cli_alert_info cli_text cli_h2 cli_ul
#'
#' @keywords models summary tidy

extract_model_summaries <- function(model_output,
                                    p_adjust_method = NULL,
                                    verbose = TRUE) {

  valid_methods <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  if (!is.null(p_adjust_method)) {
    if (!p_adjust_method %in% valid_methods) {
      rlang::abort(
        message = paste0("Invalid p-value adjustment method: '", p_adjust_method, "'. Must be one of: ", paste(valid_methods, collapse = ", ")),
        class = "invalid_p_adjust_method"
      )
    }
    if (p_adjust_method == "fdr") p_adjust_method <- "BH"
  }

  if (!inherits(model_output, "run_model_result_list")) {
    rlang::abort(
      message = paste(
        "The object you provided doesn't appear to be from run_linear_models().",
        "Please make sure you've stored the output like this:",
        "    results <- run_linear_models(...)",
        "Then pass `results` into this function:",
        "    extract_model_summaries(results)",
        sep = "\n"
      ),
      class = "invalid_model_output"
    )
  }

  results_tbl <- model_output %>%
    purrr::map("tidy") %>%
    dplyr::bind_rows(.id = "model_name") %>%
    dplyr::mutate(direction = dplyr::if_else(estimate > 0, "up", "down")) %>%
    dplyr::relocate(direction, .before = estimate)

  if (!is.null(p_adjust_method) && p_adjust_method != "none") {
    if (!"p.value" %in% names(results_tbl)) {
      rlang::warn("No `p.value` column found - skipping p-value adjustment.")
    } else {
      results_tbl <- results_tbl %>%
        dplyr::mutate(p_adjust = p.adjust(p.value, method = p_adjust_method))
    }
  }

  if (verbose && interactive()) {
    cli::cli_h1("Model Summary Extracted")
    cli::cli_alert_success("Total models processed: {length(model_output)}")
    cli::cli_alert_info("No exposure filter applied - all terms included.")

    if (!is.null(p_adjust_method) && p_adjust_method != "none") {
      method_label <- if (p_adjust_method == "BH") "Benjamini-Hochberg (FDR)" else p_adjust_method
      cli::cli_alert_info("P-values adjusted using method: {method_label}")
    } else {
      cli::cli_alert_info("No p-value adjustment applied.")
    }

    cli::cli_text("")
    cli::cli_h2("Next Steps")
    cli::cli_ul(c(
      "Explore the `direction` of effects (up/down)",
      "Sort by `p_adjust` or inspect `conf.low` and `conf.high` to find significant results (if applicable)",
      "Use this table for plotting or reporting"
    ))
  }

  class(results_tbl) <- c("model_summary_tbl", class(results_tbl))
  return(results_tbl)
}
