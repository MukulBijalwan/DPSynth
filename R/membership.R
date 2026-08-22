# ---------------------------------------------------------------------------
# risk/membership.R -- membership inference attacks
# ---------------------------------------------------------------------------

#' Membership inference risk audit
#'
#' Simulates the simplest membership inference attack: for each original
#' record, compute the distance to its nearest synthetic record. Records
#' whose nearest synthetic neighbour is unusually close are considered at
#' risk of a correct "member" verdict.
#'
#' @param synth synthetic data frame
#' @param orig original data frame
#' @param method currently only \code{"nearest_neighbor"}
#' @return list with \code{risk_score}, \code{mean_min_distance} and
#'   \code{high_risk_records}
#' @export
audit_membership_risk <- function(synth, orig, method = "nearest_neighbor") {
  stopifnot(method == "nearest_neighbor")
  numeric_vars <- intersect(names(orig)[vapply(orig, is.numeric, logical(1))],
                            names(synth)[vapply(synth, is.numeric,
                                                logical(1))])
  if (length(numeric_vars) < 1)
    return(list(risk_score = NA_real_, mean_min_distance = NA_real_,
                high_risk_records = integer(0)))

  A <- as.matrix(orig[, numeric_vars, drop = FALSE])
  B <- as.matrix(synth[, numeric_vars, drop = FALSE])

  # scale by column ranges to make distances comparable across variables
  rng <- pmax(apply(A, 2, function(x) diff(range(x))), 1e-12)
  A <- sweep(A, 2, rng, `/`)
  B <- sweep(B, 2, rng, `/`)

  d2 <- apply(B, 1, function(b)
    rowSums((A - matrix(b, nrow(A), ncol(B), byrow = TRUE))^2))
  d2m <- matrix(d2, nrow(A))
  min_dist <- sqrt(apply(d2m, 1, min))

  thr <- stats::quantile(min_dist, 0.05)
  list(risk_score = mean(min_dist < thr),
       mean_min_distance = mean(min_dist),
       high_risk_records = which(min_dist < stats::quantile(min_dist, 0.01)))
}
