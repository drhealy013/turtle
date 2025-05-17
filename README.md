
# turtle

<!-- badges: start -->

[![R-CMD-check](https://github.com/drhealy013/turtle/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/drhealy013/turtle/actions/workflows/R-CMD-check.yaml)
![Codecov test coverage](https://app.codecov.io/gh/drhealy013/turtle)
![pkgdown](https://github.com/drhealy013/turtle/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

`turtle` is an R package that provides simplified, user-friendly
functions for common data analysis tasks such as modeling,
dimensionality reduction, and visualization. It is designed to support
users with minimal programming experience by allowing them to specify
key inputs (e.g., outcome, exposure, covariates) and automatically
handle the underlying code. It is applicable to a wide range of data
types, including epidemiological, ecological, and experimental datasets.

## Installation

You can install the development version of `turtle` from GitHub with:

``` r
# install.packages("pak")
pak::pak("drhealy013/turtle")
```

## Example

Here’s a basic example using the built-in `mtcars` dataset:

``` r
library(turtle)

result <- run_linear_models(
  data = mtcars,
  outcome = "mpg",
  exposure = "wt"
)

result$tidy
#> # A tibble: 2 × 9
#>   term        estimate conf.low conf.high std.error  p.value error n_obs   BIC
#>   <chr>          <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <lgl> <int> <dbl>
#> 1 (Intercept)    37.3     33.5      41.1      1.88  8.24e-19 NA       32  170.
#> 2 wt             -5.34    -6.49     -4.20     0.559 1.29e-10 NA       32  170.
```

The function also takes lists of exposures and/or outcomes and loops
through the different combinations. These can be provided directly to
the function, or by storing them in a variable.

``` r
library(turtle)
library(purrr)

outcomes <- set_names(c("mpg", "disp"))
exposures <- set_names(c("cyl"))

result <- run_linear_models(
  data = mtcars,
  outcome = outcomes,
  exposure = exposures
)
#> Warning in formula.character(object, env = baseenv()): Using formula(x) is deprecated when x is a character vector of length > 1.
#>   Consider formula(paste(x, collapse = " ")) instead.

result$tidy
#> # A tibble: 2 × 9
#>   term        estimate conf.low conf.high std.error  p.value error n_obs   BIC
#>   <chr>          <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <lgl> <int> <dbl>
#> 1 (Intercept)    37.9     33.6      42.1      2.07  8.37e-18 NA       32  174.
#> 2 cyl            -2.88    -3.53     -2.22     0.322 6.11e-10 NA       32  174.
```

## Features

- Fits linear (`lm`) or mixed effects (`lmer`) models
- Supports:
  - Covariates
  - Effect modifiers
  - Sensitivity covariates
  - Random effects
- Returns:
  - Tidy model summary
  - Residuals
  - Model formula

## License

MIT © drhealy013
