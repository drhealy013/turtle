#' @name run_linear_models
#' @title Fit a Linear or Mixed Effects Model with Optional Modifiers
#'
#' @description This function fits a linear model (`lm`) or a mixed effects model (`lmer`) depending on the presence of random effects. It supports interaction terms, sensitivity covariates, and returns a tidy summary of the model. Users can choose whether to compute p-values for mixed models. It also supports looping over multiple outcomes and exposures.
#'
#' @param data Your dataset containing the variables used in the model.
#' @param outcome A string or character vector specifying one or more outcome variables.
#' @param exposure A string or character vector specifying one or more exposure variables.
#' @param covariates A character vector of covariate names to adjust for.
#' @param effect_modifier A string specifying an effect modifier to interact with the exposure.
#' @param sensitivity_cov A character vector of sensitivity covariates.
#' @param random_effects A string specifying the random effects structure (e.g., "(1 | group)").
#' @param p_values Logical. If TRUE (default), uses lmerTest to compute p-values for mixed models. If FALSE, uses lme4 without p-values.
#'
#' @return A list containing model results. If multiple outcomes or exposures are provided, returns a named list of results.
#' @export
#'
#' @importFrom utils globalVariables packageVersion menu
#' @importFrom stats as.formula lm nobs BIC
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate select all_of
#' @importFrom broom tidy augment
#' @importFrom broom.mixed tidy
#' @importFrom purrr safely pmap
#' @importFrom tidyr expand_grid
#' @importFrom progress progress_bar

utils::globalVariables(c("term", "estimate", "conf.low", "conf.high", "std.error", "p.value"))

run_linear_models <- function(data, outcome, exposure, covariates = NULL,
                              effect_modifier = NULL, sensitivity_cov = NULL,
                              random_effects = NULL, p_values = TRUE, verbose = TRUE) {

  current_version <- utils::packageVersion("turtle")
  check_version_warning(current_version)

  if (p_values && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("The 'lmerTest' package is required to compute p-values for mixed models. Please install it with install.packages('lmerTest').")
  }

  run_single_model <- function(outcome, exposure) {
    full_formula <- build_formula(outcome, exposure, covariates, effect_modifier, sensitivity_cov, random_effects)
    model_type <- if (!is.null(random_effects)) "lmer" else "lm"
    model_fun <- switch(model_type,
                        "lm" = stats::lm,
                        "lmer" = if (p_values) lmerTest::lmer else lme4::lmer)

    fit_result <- fit_model_safely(full_formula, data, model_fun)
    result <- fit_result$result
    warnings <- fit_result$warnings
    messages <- fit_result$messages

    if (!is.null(result$error)) {
      tidy <- tibble::tibble(term = NA, estimate = NA, conf.low = NA, conf.high = NA, p.value = NA, error = result$error$message)
      return(list(tidy = tidy, formula = full_formula, residuals = NA))
    }

    model <- result$result
    tidy <- tidy_model_output(model, warnings, messages, effect_modifier, sensitivity_cov)
    residuals <- broom::augment(model)$.resid

    list(
      model = model,
      tidy = tidy,
      formula = full_formula,
      residuals = residuals,
      exposure = exposure
    )
  }

  if (length(outcome) > 1 || length(exposure) > 1 || length(effect_modifier) > 1) {
    model_grid <- tidyr::expand_grid(
      outcome = outcome,
      exposure = exposure,
      effect_modifier = if (is.null(effect_modifier)) NA else effect_modifier
    )

    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "  Fitting models [:bar] :percent eta: :eta",
        total = nrow(model_grid),
        clear = FALSE,
        width = 60
      )
    }

    results <- purrr::pmap(model_grid, function(outcome, exposure, effect_modifier) {
      if (verbose) pb$tick()
      result <- run_single_model(outcome = outcome, exposure = exposure)
      model_name <- paste(outcome, exposure, effect_modifier, sep = "&")
      list(
        model_name = model_name,
        model = result$model,
        tidy = result$tidy,
        residuals = result$residuals,
        exposure = result$exposure,
        formula = result$formula
      )
    })

    names(results) <- purrr::map_chr(results, "model_name")
    results <- structure(results, class = "run_model_result_list")
    if (verbose) print_assignment_reminder("run_linear_models")
    return(results)
  }

  result <- run_single_model(outcome, exposure)
  result <- structure(result, class = "run_model_result")
  if (verbose) print_assignment_reminder("run_linear_models")
  return(result)
}

