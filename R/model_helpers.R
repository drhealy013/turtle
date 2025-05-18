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
#' @keywords internal
#' @noRd

build_formula <- function(outcome, exposure, covariates, effect_modifier, sensitivity_cov, random_effects) {
  if (length(outcome) > 1) {
    warning("Multiple outcomes supplied to build_formula(); using only the first: ", outcome[1])
    outcome <- outcome[1]
  }
  if (length(exposure) > 1) {
    warning("Multiple exposures supplied to build_formula(); using only the first: ", exposure[1])
    exposure <- exposure[1]
  }

  if (!is.null(effect_modifier)) {
    fixed_effects <- c(covariates, paste0(exposure, "*", effect_modifier))
  } else {
    fixed_effects <- c(covariates, exposure)
  }

  if (!is.null(sensitivity_cov)) {
    fixed_effects <- c(fixed_effects, sensitivity_cov)
  }

  full_formula <- paste(outcome, "~", paste(fixed_effects, collapse = " + "))
  if (!is.null(random_effects)) {
    full_formula <- paste(full_formula, "+", random_effects)
  }

  full_formula <- paste(full_formula, collapse = " ")
  stats::as.formula(full_formula)
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
#' @keywords internal
#' @noRd

fit_model_safely <- function(formula, data, model_fun) {
  warnings <- character()
  messages <- character()

  safe_fit <- purrr::safely(function(formula, data) {
    withCallingHandlers(
      model_fun(formula = formula, data = data),
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

  result <- safe_fit(formula, data)
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
#' @keywords internal
#' @noRd

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

#' Construct a Model Identifier Name
#'
#' @name make_model_name
#'
#' @description
#' Generates a unique model name string by combining outcome, exposure, and optionally an effect modifier. This is used to label model results in multi-model workflows.
#'
#' @param outcome A string specifying the outcome variable.
#' @param exposure A string specifying the exposure variable.
#' @param effect_modifier An optional string specifying an effect modifier to include in the model name.
#'
#' @return A character string representing the model identifier (e.g., `"outcome&exposure"` or `"outcome&exposure&modifier"`).
#'
#' @keywords internal
#' @noRd

make_model_name <- function(outcome, exposure, effect_modifier = NULL) {
  parts <- c(outcome, exposure)
  if (!is.null(effect_modifier) && !is.na(effect_modifier)) {
    parts <- c(parts, effect_modifier)
  }
  paste(parts, collapse = "&")
}

#' Detect Model Notes: Dropped Terms and Singularity
#'
#' @name detect_model_notes
#'
#' @description
#' Inspects a fitted model object (`lm` or `lmerMod`) and returns a character string summarizing any modeling issues, such as dropped terms due to collinearity or singularity in mixed models. This is useful for diagnosing model behavior and surfacing important notes in multi-model workflows.
#'
#' @param model A fitted model object of class `lm` or `lmerMod`.
#'
#' @return A character string summarizing any detected issues (e.g., dropped terms or singularity warnings), or `NA_character_` if no issues are found.
#'
#' @keywords internal
#' @noRd

detect_model_notes <- function(model) {
  notes <- character()

  if (inherits(model, "lm")) {
    dropped <- alias(model)$Complete
    if (!is.null(dropped)) {
      dropped_vars <- rownames(dropped)
      if (length(dropped_vars) > 0) {
        notes <- c(notes, paste0(
          "Dropped due to collinearity: ",
          paste(dropped_vars, collapse = ", ")
        ))
      }
    }
  }

  if (inherits(model, "lmerMod")) {
    if (lme4::isSingular(model)) {
      notes <- c(notes, "Singular fit detected: model is singular; some terms may be unidentifiable.")
    }

    terms_all <- attr(terms(model), "term.labels")
    terms_fitted <- rownames(lme4::fixef(model))
    dropped_terms <- setdiff(terms_all, terms_fitted)
    if (length(dropped_terms) > 0) {
      notes <- c(notes, paste0(
        "Possibly dropped fixed effects: ",
        paste(dropped_terms, collapse = ", ")
      ))
    }
  }

  if (length(notes) == 0) return(NA_character_)
  paste(notes, collapse = " | ")
}

