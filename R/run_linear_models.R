#' @name run_linear_models
#' @title Fit Linear or Mixed Effects Models Across Multiple Variable Combinations
#'
#' @description
#' This function fits linear (`lm`) or mixed effects (`lmer`) models across combinations of outcomes, exposures, effect modifiers, and sensitivity covariates. It supports interaction terms, random effects, and returns a tidy summary of each model. It is designed to simplify model fitting across many variable combinations with consistent covariates and random effects.
#'
#' @param data A data frame containing the variables used in the model.
#' @param outcome A character vector of outcome variable names.
#' @param exposure A character vector of exposure variable names.
#' @param covariates A character vector of covariate names to adjust for (e.g., confounders). These are included in all models.
#' @param effect_modifier A character vector of effect modifiers to interact with the exposure. All combinations will be tested.
#' @param sensitivity_cov A character vector of sensitivity covariates to include in the model. All combinations will be tested.
#' @param random_effects A string specifying the random effects structure (e.g., "(1 | group)"). If `NULL`, a linear model is used.
#' @param p_values Logical. If `TRUE` (default), uses `lmerTest` to compute p-values for mixed models. If `FALSE`, uses `lme4` without p-values.
#' @param verbose Logical. If `TRUE` (default), prints progress and model status messages.
#' @param return_grid Logical. If `TRUE`, attaches the model grid as an attribute to the returned object.
#'
#' @return A named list of model results. Each element contains:
#' \describe{
#' \item{model}{The fitted model object.}
#' \item{tidy}{A tidy summary of the model (term, estimate, confidence intervals, p-values, etc.).}
#' \item{residuals}{Model residuals.}
#' \item{formula}{The model formula used.}
#' }
#' If multiple combinations are tested, the list is named using the format `outcome&exposure&modifier&sensitivity`.
#'
#' @examples
#' # Examples
#'
#' data <- mtcars
#'
#' \dontrun{
#' # 1. Run a single model with one outcome and one exposure
#' run_linear_models(data = data,
#'                   outcome = "mpg",
#'                   exposure = "cyl")
#' }
#'
#' # 2. Store the output for a single model
#' model_output <- run_linear_models(data = data,
#'                                   outcome = "mpg",
#'                                   exposure = "cyl")
#' model_output$tidy  # access tidy results
#'
#' # 3. Store output for multiple outcomes and exposures
#' multi_output <- run_linear_models(data = data,
#'                                   outcome = c("mpg", "hp"),
#'                                   exposure = c("cyl", "wt"))
#' multi_output[["mpg&cyl"]]$tidy  # access a specific model's tidy results
#'
#' # 4. Use named vectors for outcomes and exposures
#' outcomes <- c("mpg", "hp")
#' exposures <- c("cyl", "wt")
#' named_output <- run_linear_models(data = data,
#'                                   outcome = outcomes,
#'                                   exposure = exposures)
#' named_output[["hp&wt"]]$tidy
#'
#' # 5. Include covariates directly
#' covar_output <- run_linear_models(data = data,
#'                                   outcome = "mpg",
#'                                   exposure = "cyl",
#'                                   covariates = c("wt", "hp"))
#' covar_output$tidy
#'
#' # 6. Include covariates using a named vector
#' covars <- c("wt", "hp")
#' names(covars) <- covars
#' named_covar_output <- run_linear_models(data = data,
#'                                         outcome = "mpg",
#'                                         exposure = "cyl",
#'                                         covariates = covars)
#' named_covar_output$tidy
#'
#' # 7. Include an effect modifier (e.g., interaction term)
#' interaction_output <- run_linear_models(data = data,
#'                                         outcome = "mpg",
#'                                         exposure = "cyl",
#'                                         effect_modifier = "am")
#' interaction_output$tidy
#'
#' # 8. Model with a non-existent variable (graceful failure)
#' error_output <- run_linear_models(data = data,
#'                                   outcome = "mpg",
#'                                   exposure = "not_a_column")
#'
#' error_output$tidy
#'
#' @return A list containing model results. If multiple outcomes or exposures are provided, returns a named list of results.
#' @export
#'
#' @importFrom utils globalVariables packageVersion menu
#' @importFrom stats as.formula lm nobs BIC alias terms
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
                              random_effects = NULL, p_values = TRUE,
                              verbose = TRUE, return_grid = FALSE) {

  current_version <- utils::packageVersion("turtle")
  check_version_warning(current_version)

  if (p_values && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("The 'lmerTest' package is required to compute p-values for mixed models. Please install it with install.packages('lmerTest').")
  }

  run_single_model <- function(outcome, exposure, effect_modifier, sensitivity_cov) {
    if (verbose) {
      message("Running model with outcome = ", outcome,
              ", exposure = ", exposure,
              if (!is.null(effect_modifier)) paste0(", effect_modifier = ", effect_modifier) else "",
              if (!is.null(sensitivity_cov)) paste0(", sensitivity_cov = ", sensitivity_cov) else "")
    }

    full_formula <- build_formula(
      outcome = as.character(outcome)[1],
      exposure = as.character(exposure)[1],
      covariates = covariates,
      effect_modifier = effect_modifier,
      sensitivity_cov = sensitivity_cov,
      random_effects = random_effects
    )
    model_type <- if (!is.null(random_effects)) "lmer" else "lm"
    model_fun <- switch(model_type,
                        "lm" = stats::lm,
                        "lmer" = if (p_values) lmerTest::lmer else lme4::lmer)

    fit_result <- fit_model_safely(full_formula, data, model_fun)
    result <- fit_result$result
    warnings <- fit_result$warnings
    messages <- fit_result$messages

    formula_str <- paste(deparse(full_formula), collapse = "")

    if (!is.null(result$error)) {
      if (verbose) message("Model failed for formula: ", formula_str, "\nError: ", result$error$message)
      tidy <- tibble::tibble(
        term = NA, estimate = NA, conf.low = NA, conf.high = NA,
        p.value = NA, note = result$error$message, formula = formula_str
      )
      return(list(tidy = tidy, formula = full_formula, residuals = NA))
    }

    model <- result$result
    tidy <- tidy_model_output(model, warnings, messages, effect_modifier, sensitivity_cov)
    tidy$formula <- formula_str
    tidy$note <- detect_model_notes(model)
    residuals <- broom::augment(model)$.resid

    list(
      model = model,
      tidy = tidy,
      formula = full_formula,
      residuals = residuals,
      exposure = exposure
    )
  }

  if (!is.character(outcome)) outcome <- as.character(outcome)
  if (!is.character(exposure)) exposure <- as.character(exposure)

  model_grid <- tidyr::expand_grid(
    outcome = outcome,
    exposure = exposure,
    effect_modifier = if (is.null(effect_modifier)) NA_character_ else effect_modifier,
    sensitivity_cov = if (is.null(sensitivity_cov)) NA_character_ else sensitivity_cov
  )

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "  Fitting models [:bar] :percent eta: :eta",
      total = nrow(model_grid),
      clear = FALSE,
      width = 60
    )
  }

  results <- purrr::pmap(model_grid, function(outcome, exposure, effect_modifier, sensitivity_cov) {
    if (verbose) pb$tick()
    result <- run_single_model(
      outcome = outcome,
      exposure = exposure,
      effect_modifier = if (!is.na(effect_modifier)) effect_modifier else NULL,
      sensitivity_cov = if (!is.na(sensitivity_cov)) sensitivity_cov else NULL
    )

    model_name_parts <- c(outcome, exposure,
                          if (!is.na(effect_modifier)) effect_modifier,
                          if (!is.na(sensitivity_cov)) sensitivity_cov)
    model_name <- paste(model_name_parts, collapse = "&")

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
  if (verbose) print_assignment_reminder(.test_force = verbose)
  if (return_grid) attr(results, "model_grid") <- model_grid
  return(results)
}
