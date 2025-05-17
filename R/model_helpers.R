#' Build Model Formula
#'
#' @name build_formula
#'
#' @description
#' Constructs a full model formula string from outcome, exposure, covariates, effect modifiers, and optional random effects.
#'
#' @param outcome A string specifying the outcome variable.
#' @param exposure A string specifying the exposure variable.
#' @param covariates A character vector of covariates to adjust for.
#' @param effect_modifier A string specifying an effect modifier to interact with the exposure.
#' @param sensitivity_cov A character vector of sensitivity covariates.
#' @param random_effects A string specifying the random effects structure (e.g., `"(1 | group)"`).
#'
#' @return A model formula object.
#'
#' @examples
#' build_formula("y", "x", covariates = c("age", "sex"),
#'               effect_modifier = "group",
#'               sensitivity_cov = NULL,
#'               random_effects = "(1 | id)")
#'
#' @export

build_formula <- function(outcome, exposure, covariates, effect_modifier, sensitivity_cov, random_effects) {
  fixed_effects <- c(covariates, exposure)
  if (!is.null(effect_modifier)) {
    fixed_effects <- c(fixed_effects, paste0(exposure, "*", effect_modifier))
  }
  if (!is.null(sensitivity_cov)) {
    fixed_effects <- c(fixed_effects, sensitivity_cov)
  }
  fixed_formula <- paste(outcome, "~", paste(fixed_effects, collapse = " + "))
  if (!is.null(random_effects)) {
    return(as.formula(paste(fixed_formula, "+", random_effects)))
  }
  as.formula(fixed_formula)
}

#' Fit Model Safely
#'
#' @name fit_model_safely
#'
#' @description
#' Fits a model using a safe wrapper that captures warnings and messages without interrupting execution.
#'
#' @param formula A model formula object.
#' @param data A data frame containing the variables used in the model.
#' @param model_fun A function used to fit the model (e.g., `lm`, `lmer`).
#'
#' @return A list with three elements: `result` (model or error), `warnings`, and `messages`.
#'
#' @examples
#' fit_model_safely(y ~ x, data = mtcars, model_fun = lm)
#'
#' @export

fit_model_safely <- function(formula, data, model_fun) {
  warnings <- character()
  messages <- character()

  safe_fit <- purrr::safely(function(...) {
    withCallingHandlers(
      rlang::exec(model_fun, ...),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  })

  result <- safe_fit(formula = formula, data = data)
  list(result = result, warnings = warnings, messages = messages)
}

#' Tidy Model Output
#'
#' @name tidy_model_output
#'
#' @description
#' Tidies the output of a fitted model and appends metadata such as warnings, number of observations, and BIC.
#'
#' @param model A fitted model object.
#' @param warnings A character vector of warning messages.
#' @param messages A character vector of message strings.
#' @param effect_modifier A string specifying the effect modifier (optional).
#' @param sensitivity_cov A character vector of sensitivity covariates (optional).
#'
#' @return A tibble containing the tidy model output.
#'
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' tidy_model_output(model,
#'                   warnings = NULL,
#'                   messages = NULL,
#'                   effect_modifier = NULL,
#'                   sensitivity_cov = NULL)
#'
#' @export

tidy_model_output <- function(model, warnings, messages, effect_modifier, sensitivity_cov) {
  tidy_raw <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  columns_to_select <- c("term", "estimate", "conf.low", "conf.high", "std.error")
  if ("p.value" %in% names(tidy_raw)) {
    columns_to_select <- c(columns_to_select, "p.value")
  }

  tidy <- tidy_raw %>%
    dplyr::select(dplyr::all_of(columns_to_select)) %>%
    dplyr::mutate(
      error = ifelse(length(warnings) > 0 || length(messages) > 0, paste(c(warnings, messages), collapse = "; "), NA),
      n_obs = stats::nobs(model),
      BIC = stats::BIC(model)
    )

  if (!is.null(effect_modifier)) {
    tidy <- tidy %>% dplyr::mutate(modifier = effect_modifier)
  }

  if (!is.null(sensitivity_cov)) {
    tidy <- tidy %>% dplyr::mutate(sensitivity = paste(sensitivity_cov, collapse = ", "))
  }

  tidy
}
