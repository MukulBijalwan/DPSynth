# ---------------------------------------------------------------------------
# data.R -- documentation for shipped datasets
# ---------------------------------------------------------------------------

#' Anonymized UCI Adult subsample
#'
#' A synthetic, anonymized subsample modeled on the UCI Adult census-income
#' dataset, used as an example throughout \pkg{DPSynth}. No real personal
#' records are included.
#'
#' @format A data frame with 500 rows and 8 variables:
#' \describe{
#'   \item{age}{age in years}
#'   \item{workclass}{employment sector}
#'   \item{education_num}{years of education}
#'   \item{marital_status}{marital status}
#'   \item{hours_per_week}{working hours per week}
#'   \item{sex}{sex}
#'   \item{capital_gain}{capital gain in USD}
#'   \item{income}{binary income class}
#' }
"adult_sample"

#' ACS PUMS-like sample
#'
#' A synthetic microdata sample modeled on American Community Survey PUMS
#' structure, used as an example in \pkg{DPSynth}.
#'
#' @format A data frame with 500 rows and 5 variables:
#' \describe{
#'   \item{age}{age in years}
#'   \item{income}{annual income in USD}
#'   \item{educ}{educational attainment}
#'   \item{married}{married indicator}
#'   \item{weeks_worked}{weeks worked last year}
#' }
"acs_pums_sample"
