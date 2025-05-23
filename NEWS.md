# turtle (development version)

* Initial CRAN submission.

# turtle 0.1.0

## Initial release

* Introduced `run_linear_models()` function to fit linear (`lm`) and mixed effects (`lmer`) models.
* Supports:
  - Covariate adjustment
  - Effect modification via interaction terms
  - Sensitivity covariates
  - Random effects specification
* Returns a comprehensive output including:
  - The fitted model object
  - A tidy summary of fixed effects with estimates, confidence intervals, standard errors, p-values, number of observations, and BIC
  - Model formula
  - Residuals
  - Exposure variable name
* Includes robust error and warning handling using `purrr::safely()` and `withCallingHandlers()`. Includes errors or warnings in the tidy output so they can be viewed later when interpreting results.

# turtle 0.1.1

## Update runLinearModels

*Ask for clarification if user submits an interaction as the exposure, and additionally adds an effect-modifier, to confirm if the user wants to pursue 3-way interactions.
  - Robust for both interactive and non-interactive use

# turtle 0.1.2

## Update runLinearModels

*Added version check to `run_linear_models()` so the user knows if an update is needed or not.
*Added option to not include p-values, which will switch to using lme4 to run mixed-effect models.

# turtle 0.1.3

## BugFix

*Fixed bug so that tidy output will optionally select p-values based on whether it exists in the output.

# turtle 0.1.4

## Update runLinearModels

*Added functionality so that either a single e.g., outcome or exposure can be provided, or else a vector of e.g., outcomes or exposures. If a list is provided, loops through the possible combinations of exposures and outcomes.

# turtle 0.1.5

## Create print_methods_run_linear_models

*Make a custom printing function for the run_linear_models function to provide feedback to users not familiar with data analysis

# turtle 0.1.6

## Create model_helpers & utils

*Create model_helpers script
  -Turn some of the repetitive code in run_linear_models into helpers so they can be more easily updated and utilised elsewhere
*Create utils script
  -Turn some of the repetitive code in run_linear_models into utils so they can be more easily updated and utilised elsewhere
*Update run_linear_models
  -Remove the model_helpers and utils and instead call them from their separate files.
  
# turtle 0.1.7

## Created model_helpers

*make_model_name and detect_model_notes to help clean up run_linear_models code.

# turtle 0.1.8

## Bug fix

*Updating S3 print methods and helper print functions.
