# ---------------------------------------------------------------------------
# utility/evaluate_utility.R -- orchestration of the utility report
# ---------------------------------------------------------------------------

#' Standardized utility evaluation
#'
#' Runs any combination of univariate, multivariate, propensity (pMSE) and
#' downstream (TSTR) fidelity metrics and returns a
#' \code{dp_utility_report}.
#'
#' @param synthetic_data synthetic data frame
#' @param original_data original data frame
#' @param metrics character vector of metric groups to run
#' @param target_var optional target for the downstream metric
#' @return object of class \code{dp_utility_report}
#' @export
evaluate_utility <- function(synthetic_data, original_data,
                             metrics = c("univariate", "multivariate",
                                         "propensity"),
                             target_var = NULL) {
  results <- list()
  if ("univariate" %in% metrics)
    results$univariate <- evaluate_univariate(synthetic_data, original_data)
  if ("multivariate" %in% metrics)
    results$multivariate <- evaluate_multivariate(synthetic_data,
                                                  original_data)
  if ("propensity" %in% metrics)
    results$propensity <- evaluate_propensity(synthetic_data, original_data)
  if ("downstream" %in% metrics) {
    if (is.null(target_var))
      stop("target_var required for the 'downstream' metric")
    results$downstream <- evaluate_downstream(synthetic_data, original_data,
                                              target_var)
  }
  structure(results, class = "dp_utility_report")
}
