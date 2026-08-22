# ---------------------------------------------------------------------------
# utility/multivariate.R -- correlation structure fidelity
# ---------------------------------------------------------------------------

#' Multivariate fidelity evaluation
#'
#' Compares the correlation structure (numeric columns) of original and
#' synthetic data via the relative Frobenius-norm distance between
#' correlation matrices and Gaussian-approximated mutual information.
#'
#' @param synth synthetic data frame
#' @param orig original data frame
#' @return list with correlation distance and mutual-information summaries
#' @export
evaluate_multivariate <- function(synth, orig) {
  numeric_vars <- names(orig)[vapply(orig, is.numeric, logical(1))]
  numeric_vars <- numeric_vars[vapply(synth[numeric_vars],
                                      is.numeric, logical(1))]
  if (length(numeric_vars) < 2) {
    return(list(correlation_distance = NA_real_,
                mutual_information_orig = NA_real_,
                mutual_information_synth = NA_real_,
                mi_relative_error = NA_real_))
  }
  R_orig <- stats::cor(orig[, numeric_vars, drop = FALSE],
                       use = "pairwise.complete.obs")
  R_synth <- stats::cor(synth[, numeric_vars, drop = FALSE],
                        use = "pairwise.complete.obs")
  R_synth[is.na(R_synth)] <- 0; diag(R_synth) <- 1
  R_orig[is.na(R_orig)] <- 0; diag(R_orig) <- 1

  cor_distance <- norm(R_orig - R_synth, "F") / max(norm(R_orig, "F"), 1e-12)

  rho_o <- R_orig[upper.tri(R_orig)]
  rho_s <- R_synth[upper.tri(R_synth)]
  mi_orig <- -0.5 * sum(log(pmax(1 - rho_o^2, 1e-12)))
  mi_synth <- -0.5 * sum(log(pmax(1 - rho_s^2, 1e-12)))

  list(correlation_distance = cor_distance,
       mutual_information_orig = mi_orig,
       mutual_information_synth = mi_synth,
       mi_relative_error = abs(mi_synth - mi_orig) /
         max(abs(mi_orig), 1e-12))
}
