# ---------------------------------------------------------------------------
# utility/downstream.R -- TSTR (train-on-synthetic, test-on-real)
# ---------------------------------------------------------------------------

#' Downstream task performance (TSTR)
#'
#' Trains the same linear model on the synthetic and the original data and
#' compares out-of-sample RMSE on held-out real data. A
#' \code{utility_ratio} close to 1 indicates good analytic utility.
#'
#' @param synth synthetic data frame
#' @param orig original data frame
#' @param target_var name of the numeric target column
#' @param predictors optional predictor column names; default all others
#' @param test_prop held-out fraction of the original data
#' @return list with RMSEs and the utility ratio
#' @export
evaluate_downstream <- function(synth, orig, target_var,
                                predictors = NULL, test_prop = 0.2) {
  stopifnot(target_var %in% names(orig), target_var %in% names(synth))
  if (is.null(predictors)) {
    predictors <- setdiff(intersect(names(orig), names(synth)), target_var)
  }
  test_idx <- sample.int(nrow(orig), max(1, floor(nrow(orig) * test_prop)))
  test_data <- orig[test_idx, , drop = FALSE]
  train_orig <- orig[-test_idx, , drop = FALSE]

  form <- stats::as.formula(
    paste(target_var, "~", paste(predictors, collapse = " + ")))
  fit_synth <- suppressWarnings(stats::lm(form, data = synth))
  fit_orig <- suppressWarnings(stats::lm(form, data = train_orig))

  pred_synth <- stats::predict(fit_synth, newdata = test_data)
  pred_orig <- stats::predict(fit_orig, newdata = test_data)
  y <- test_data[[target_var]]

  rmse <- function(p) sqrt(mean((y - p)^2))
  rmse_synth <- rmse(pred_synth)
  rmse_orig <- rmse(pred_orig)
  list(RMSE_synthetic_training = rmse_synth,
       RMSE_original_training = rmse_orig,
       utility_ratio = rmse_synth / max(rmse_orig, 1e-12))
}
