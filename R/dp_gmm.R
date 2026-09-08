# ---------------------------------------------------------------------------
# dp_gmm.R -- DP Gaussian Mixture Model synthesis
# ---------------------------------------------------------------------------

#' DP Gaussian Mixture Model synthesis
#'
#' Fits a Gaussian mixture model with diagonal covariances using the
#' perturb-and-postprocess paradigm: sufficient statistics (component
#' counts, means, variances) are perturbed with calibrated Laplace /
#' Gaussian noise so that the released model satisfies (epsilon, delta)-DP.
#' Sampling from the fitted model is post-processing and consumes no
#' additional privacy budget.
#'
#' @param data data frame of purely numeric columns
#' @param n_synth number of synthetic records to generate
#' @param K number of mixture components
#' @param epsilon privacy parameter epsilon (> 0)
#' @param delta privacy parameter delta (in (0, 1))
#' @param bounds optional list with \code{lower} / \code{upper} vectors
#'   used to clamp the data; by default taken from the data range
#'   (note: data-driven bounds are not themselves DP -- supply public
#'   bounds for a formal guarantee)
#' @param max_iter maximum EM iterations
#' @return object of class \code{dp_gmm} / \code{dp_synthetic} with the
#'   synthetic data, model parameters and privacy parameters
#' @export
dp_gmm_synth <- function(data, n_synth = nrow(data), K = 3,
                         epsilon = 1.0, delta = 1e-6,
                         bounds = NULL, max_iter = 20) {
  stopifnot(is.data.frame(data), epsilon > 0, delta > 0, delta < 1)
  data <- data[sapply(data, is.numeric)]
  stopifnot(ncol(data) >= 1, nrow(data) >= 2)
  n <- nrow(data)
  d <- ncol(data)

  if (is.null(bounds)) {
    lo <- vapply(data, function(x) min(x, na.rm = TRUE), numeric(1))
    hi <- vapply(data, function(x) max(x, na.rm = TRUE), numeric(1))
  } else {
    lo <- as.numeric(bounds$lower); hi <- as.numeric(bounds$upper)
  }
  bounds <- list(lower = lo, upper = hi)
  data_clipped <- clip_data(data, bounds)
  X <- as.matrix(data_clipped)
  ranges <- hi - lo
  ranges[ranges <= 0] <- 1e-6

  # Budget split: 20% weights, 40% means, 40% variances
  eps_w <- 0.2 * epsilon
  eps_mu <- 0.4 * epsilon
  eps_var <- 0.4 * epsilon

  # DP initialization
  centers <- dp_kmeans_init(X, K, eps_w)
  covars <- matrix(rep(ranges^2 / 16, each = K), K, d, byrow = TRUE)

  l2_sens_mu <- sqrt(sum(ranges^2)) / n
  sigma_mu <- l2_sens_mu * sqrt(2 * log(1.25 / delta)) / eps_mu
  sens_var <- max(ranges)^2 / n
  sigma_var <- sens_var * sqrt(2 * log(1.25 / delta)) / eps_var

  weights <- rep(1 / K, K)
  for (iter in seq_len(max_iter)) {
    # E-step (local, no privacy cost)
    resp <- matrix(0, n, K)
    for (k in seq_len(K)) {
      loglik <- stats::dnorm(X, matrix(centers[k, ], n, d, byrow = TRUE),
                      matrix(sqrt(covars[k, ]), n, d, byrow = TRUE), log = TRUE)
      resp[, k] <- weights[k] * exp(rowSums(loglik))
    }
    resp <- resp / pmax(rowSums(resp), 1e-300)

    # M-step with noisy sufficient statistics
    N_k <- colSums(resp) + rlaplace(K, 0, 1 / eps_w)
    N_k <- pmax(N_k, 1)

    mu <- matrix(0, K, d)
    var_k <- matrix(0, K, d)
    for (k in seq_len(K)) {
      w <- resp[, k]
      mu[k, ] <- colSums(w * X) / N_k[k] +
        rnorm(d, 0, sigma_mu)
      var_k[k, ] <- colSums(w * (X - matrix(mu[k, ], n, d,
                                            byrow = TRUE))^2) / N_k[k] +
        pmax(rnorm(d, 0, sigma_var), 0)
      var_k[k, ] <- pmax(var_k[k, ], 1e-6)
    }
    centers <- mu
    covars <- var_k
    weights <- N_k / sum(N_k)
  }

  # Sample synthetic records (post-processing)
  components <- sample.int(K, n_synth, prob = weights, replace = TRUE)
  synth <- t(vapply(components, function(k)
    rnorm(d, centers[k, ], sqrt(covars[k, ])), numeric(d)))
  if (n_synth == 1) synth <- matrix(synth, 1, d)
  synth <- as.data.frame(synth, stringsAsFactors = FALSE)
  names(synth) <- names(data)

  structure(list(
    synthetic_data = synth,
    parameters = list(centers = centers, variances = covars,
                      weights = weights),
    epsilon = epsilon, delta = delta, bounds = bounds,
    method = "gmm"
  ), class = c("dp_gmm", "dp_synthetic"))
}
