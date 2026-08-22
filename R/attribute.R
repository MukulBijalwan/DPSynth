# ---------------------------------------------------------------------------
# risk/attribute.R -- attribute disclosure risk
# ---------------------------------------------------------------------------

#' Attribute disclosure risk audit
#'
#' Assumes an attacker knows \code{known_vars} for a target record and
#' uses nearest neighbours in the synthetic data to infer
#' \code{sensitive_var}. Reports the normalized prediction error and the
#' rate of high-confidence disclosures.
#'
#' @param synth synthetic data frame
#' @param orig original data frame
#' @param known_vars column names the attacker observes; default: all
#'   common columns except \code{sensitive_var}
#' @param sensitive_var sensitive column to predict; default: last column
#' @param n_neighbors number of synthetic neighbours used
#' @return list with \code{mean_disclosure_error} and \code{high_risk_rate}
#' @export
audit_attribute_disclosure <- function(synth, orig,
                                       known_vars = NULL,
                                       sensitive_var = NULL,
                                       n_neighbors = 5) {
  if (is.null(sensitive_var)) sensitive_var <- names(orig)[ncol(orig)]
  if (is.null(known_vars)) {
    known_vars <- setdiff(intersect(names(orig), names(synth)),
                          sensitive_var)
  }
  stopifnot(sensitive_var %in% names(orig), sensitive_var %in% names(synth))
  num_known <- intersect(known_vars,
                         names(orig)[vapply(orig, is.numeric, logical(1))])
  num_known <- intersect(num_known,
                         names(synth)[vapply(synth, is.numeric, logical(1))])
  if (length(num_known) < 1 || !is.numeric(orig[[sensitive_var]])) {
    return(list(mean_disclosure_error = NA_real_,
                high_risk_rate = NA_real_))
  }

  A <- as.matrix(orig[, num_known, drop = FALSE])
  B <- as.matrix(synth[, num_known, drop = FALSE])
  y_true <- orig[[sensitive_var]]
  s0 <- sd(y_true); if (is.na(s0) || s0 == 0) s0 <- 1

  err <- vapply(seq_len(nrow(A)), function(i) {
    d2 <- rowSums((B - matrix(A[i, ], nrow(B), ncol(B), byrow = TRUE))^2)
    k <- min(n_neighbors, nrow(B))
    nn <- order(d2)[seq_len(k)]
    pred <- mean(synth[[sensitive_var]][nn], na.rm = TRUE)
    abs(pred - y_true[i]) / s0
  }, numeric(1))

  list(mean_disclosure_error = mean(err),
       high_risk_rate = mean(err < 0.1))
}
