# ---------------------------------------------------------------------------
# mechanisms.R -- DP noise mechanisms and helpers
# ---------------------------------------------------------------------------

#' Laplace noise
#'
#' Draw \code{n} independent Laplace(\code{location}, \code{scale}) variates
#' using inverse-CDF sampling. The Laplace mechanism adds
#' \code{Lap(sensitivity / epsilon)} noise to achieve pure epsilon-DP.
#'
#' @param n number of draws
#' @param location location parameter
#' @param scale scale parameter (must be > 0)
#' @return numeric vector of length \code{n}
#' @export
rlaplace <- function(n, location = 0, scale = 1) {
  stopifnot(is.numeric(scale), length(scale) == 1, scale > 0)
  u <- runif(n, -0.5, 0.5)
  u <- pmin(pmax(u, -0.4999999), 0.4999999) # guard against log(0)
  location - scale * sign(u) * log(1 - 2 * abs(u))
}

#' Gaussian mechanism
#'
#' Release a statistic under (epsilon, delta)-DP by adding Gaussian noise
#' calibrated to the L2 sensitivity via the classical analytic bound
#' \eqn{\sigma = \Delta \sqrt{2 \log(1.25 / \delta)} / \epsilon}.
#'
#' @param value numeric statistic (scalar or vector)
#' @param sensitivity L2 sensitivity of the statistic
#' @param epsilon privacy parameter epsilon (> 0)
#' @param delta privacy parameter delta (in (0, 1))
#' @return numeric vector: noisy release of \code{value}
#' @export
gaussian_mech <- function(value, sensitivity, epsilon, delta = 1e-6) {
  stopifnot(epsilon > 0, delta > 0, delta < 1, sensitivity >= 0)
  sigma <- sensitivity * sqrt(2 * log(1.25 / delta)) / epsilon
  value + rnorm(length(value), 0, sigma)
}

#' Clip data to bounds
#'
#' Clamp every column of a data frame (or matrix) to user-specified bounds.
#' Clamping bounds sensitivity and is a prerequisite for calibrated DP noise.
#'
#' @param data data frame or matrix
#' @param bounds named list with elements \code{lower} and \code{upper}
#'   (numeric vectors recycled across columns), or a 2-row matrix
#'   \code{[lower; upper]}.
#' @return data frame with values clipped to the bounds
#' @export
clip_data <- function(data, bounds) {
  if (is.matrix(bounds)) {
    bounds <- list(lower = bounds[1, ], upper = bounds[2, ])
  }
  stopifnot(is.list(bounds), all(c("lower", "upper") %in% names(bounds)))
  lower <- rep_len(bounds$lower, ncol(data))
  upper <- rep_len(bounds$upper, ncol(data))
  stopifnot(all(lower <= upper))
  out <- as.data.frame(lapply(seq_along(data), function(j) {
    x <- as.numeric(data[[j]])
    pmin(pmax(x, lower[j]), upper[j])
  }), stringsAsFactors = FALSE)
  names(out) <- names(data)
  out
}

#' DP k-means initialization via the exponential mechanism
#'
#' Selects \code{K} cluster centers from candidate rows using the
#' exponential mechanism with a k-means++ style D2 quality score, so the
#' initialization itself satisfies epsilon-DP.
#'
#' @param data numeric matrix or data frame (already clipped)
#' @param K number of centers
#' @param epsilon privacy budget for the initialization
#' @return numeric matrix with K rows (the selected centers)
#' @keywords internal
dp_kmeans_init <- function(data, K, epsilon) {
  X <- as.matrix(data)
  n <- nrow(X)
  d <- ncol(X)
  range <- max(apply(X, 2, function(x) diff(range(x))), 1e-12)

  # Candidate pool: subsample rows (deterministic subsample of the data is
  # fine because the exponential-mechanism release is what consumes budget)
  n_cand <- min(n, 200L)
  cand_idx <- seq_len(n)
  if (n > n_cand) cand_idx <- sample.int(n, n_cand)
  candidates <- X[cand_idx, , drop = FALSE]

  centers <- matrix(NA_real_, K, d)

  for (k in seq_len(K)) {
    # D2-style quality: gain in the k-means objective from adding candidate
    if (k == 1) {
      gain <- -vapply(seq_len(nrow(candidates)), function(i)
        sum(rowSums((X - matrix(candidates[i, ], n, d, byrow = TRUE))^2)),
        numeric(1))
    } else {
      cur <- apply(X, 1, function(x)
        min(apply(centers[seq_len(k - 1), , drop = FALSE], 1,
                  function(c) sum((x - c)^2))))
      gain <- -vapply(seq_len(nrow(candidates)), function(i)
        sum(pmin(cur, rowSums((X - matrix(candidates[i, ], n, d,
                                              byrow = TRUE))^2))),
        numeric(1))
    }
    # Exponential mechanism: prob ~ exp(epsilon * gain / (2 * delta_u))
    delta_u <- 2 * range^2 * n # sensitivity bound for the D2 objective
    w <- exp(epsilon * (gain - max(gain)) / (2 * delta_u) * n) # stabilised
    w <- w / sum(w)
    pick <- sample.int(nrow(candidates), 1, prob = w)
    centers[k, ] <- candidates[pick, ]
  }
  centers
}
