# ---------------------------------------------------------------------------
# risk/linkage.R -- record linkage risk
# ---------------------------------------------------------------------------

#' Record linkage risk audit
#'
#' Estimates how many real records can be uniquely re-identified by
#' matching against the synthetic data: a real record is "linked" when its
#' nearest synthetic neighbour is closer than a quantile threshold of the
#' within-original distance distribution.
#'
#' @param synth synthetic data frame
#' @param orig original data frame
#' @param threshold_quantile quantile of within-original NN distances used
#'   as the linkage threshold
#' @return list with \code{linkage_rate}, \code{linked_records},
#'   \code{threshold}
#' @export
audit_linkage_risk <- function(synth, orig, threshold_quantile = 0.05) {
  numeric_vars <- intersect(names(orig)[vapply(orig, is.numeric, logical(1))],
                            names(synth)[vapply(synth, is.numeric,
                                                logical(1))])
  if (length(numeric_vars) < 1)
    stop("Need at least one common numeric column")
  A <- as.matrix(orig[, numeric_vars, drop = FALSE])
  B <- as.matrix(synth[, numeric_vars, drop = FALSE])

  nn_dist_orig <- sqrt(vapply(seq_len(nrow(A)), function(i) {
    if (nrow(A) < 2) return(Inf)
    d2 <- rowSums((A - matrix(A[i, ], nrow(A), ncol(A), byrow = TRUE))^2)
    d2[i] <- Inf
    min(d2)
  }, numeric(1)))
  thr <- stats::quantile(nn_dist_orig[is.finite(nn_dist_orig)],
                         threshold_quantile)

  d2s <- apply(B, 1, function(b)
    rowSums((A - matrix(b, nrow(A), ncol(A), byrow = TRUE))^2))
  nn_dist_synth <- sqrt(apply(matrix(d2s, nrow(A)), 1, min))
  linked <- which(nn_dist_synth < thr)

  list(linkage_rate = length(linked) / nrow(A),
       linked_records = linked,
       threshold = unname(thr))
}
