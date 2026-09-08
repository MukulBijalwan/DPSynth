# ---------------------------------------------------------------------------
# dp_copula.R -- DP Gaussian copula synthesis
# ---------------------------------------------------------------------------

#' DP Gaussian copula synthesis
#'
#' Separates the synthesis problem into (1) DP marginal distributions --
#' per-column histograms released with Laplace noise -- and (2) a DP
#' dependence structure -- a Gaussian copula correlation matrix estimated
#' on rank-transformed pseudo-observations, perturbed with Gaussian noise
#' and projected onto the positive-definite cone. Generation inverts the
#' DP marginals through the DP copula (post-processing).
#'
#' @param data data frame (numeric, factor or character columns)
#' @param n_synth number of synthetic records
#' @param epsilon total privacy parameter epsilon
#' @param delta privacy parameter delta
#' @param n_bins number of histogram bins for numeric marginals
#' @param copula_type currently only \code{"gaussian"}
#' @return object of class \code{dp_copula} / \code{dp_synthetic}
#' @export
dp_copula_synth <- function(data, n_synth = nrow(data),
                            epsilon = 1.0, delta = 1e-6,
                            n_bins = 20, copula_type = "gaussian") {
  stopifnot(is.data.frame(data), epsilon > 0, n_bins >= 2)
  n <- nrow(data)
  d <- ncol(data)
  stopifnot(n >= 2, d >= 1)

  # Budget split: 60% marginals, 40% dependence structure
  eps_marg <- 0.6 * epsilon
  eps_cop <- 0.4 * epsilon

  # --- Step 1: DP marginals ---
  marginals <- vector("list", d)
  names(marginals) <- names(data)
  for (j in seq_len(d)) {
    xj <- data[[j]]
    if (is.numeric(xj)) {
      rng <- range(xj, na.rm = TRUE)
      if (diff(rng) == 0) rng[2] <- rng[1] + 1e-6
      breaks <- seq(rng[1], rng[2], length.out = n_bins + 1)
      counts <- as.numeric(hist(xj, breaks = breaks, plot = FALSE)$counts)
      dp_counts <- counts + rlaplace(n_bins, 0, 1 / (eps_marg / d))
      dp_counts <- pmax(dp_counts, 0)
      cdf <- cumsum(dp_counts) / max(sum(dp_counts), 1e-12)
      marginals[[j]] <- list(type = "numeric", breaks = breaks, cdf = cdf)
    } else if (is.factor(xj) || is.character(xj)) {
      lv <- if (is.factor(xj)) levels(xj) else sort(unique(xj))
      tab <- as.numeric(table(factor(xj, levels = lv)))
      dp_tab <- tab + rlaplace(length(tab), 0, 1 / (eps_marg / d))
      dp_tab <- pmax(dp_tab, 0)
      probs <- dp_tab / max(sum(dp_tab), 1e-12)
      marginals[[j]] <- list(type = "categorical", levels = lv, probs = probs)
    } else {
      stop("Unsupported column type for column ", names(data)[j])
    }
  }

  # --- Step 2: DP correlation on pseudo-observations ---
  num_cols <- vapply(data, is.numeric, logical(1))
  R_priv <- diag(d)
  if (sum(num_cols) >= 2) {
    U <- as.data.frame(lapply(data[num_cols], function(x)
      rank(x, ties.method = "random", na.last = "keep") / (n + 1)))
    R <- cor(as.matrix(U), use = "pairwise.complete.obs",
             method = "spearman")
    sens_R <- 2 * sqrt(sum(num_cols) * (sum(num_cols) - 1) / 2) / n
    sigma_R <- sens_R * sqrt(2 * log(1.25 / delta)) / eps_cop
    R_priv_num <- R + matrix(rnorm(length(R), 0, sigma_R), ncol(R), ncol(R))
    R_priv_num <- (R_priv_num + t(R_priv_num)) / 2
    diag(R_priv_num) <- 1
    R_priv_num <- nearest_correlation(R_priv_num)
    R_priv[num_cols, num_cols] <- R_priv_num
  }

  # --- Step 3: Generate ---
  Z <- MASS::mvrnorm(n_synth, mu = rep(0, d), Sigma = R_priv)
  U_synth <- pnorm(Z)

  synth <- as.data.frame(matrix(NA, n_synth, d), stringsAsFactors = FALSE)
  names(synth) <- names(data)
  for (j in seq_len(d)) {
    if (marginals[[j]]$type == "numeric") {
      synth[[j]] <- stats::approx(x = marginals[[j]]$cdf,
                                  y = marginals[[j]]$breaks[-1],
                                  xout = U_synth[, j], rule = 2)$y
    } else {
      synth[[j]] <- sample(marginals[[j]]$levels, n_synth,
                           prob = marginals[[j]]$probs, replace = TRUE)
      synth[[j]] <- factor(synth[[j]], levels = marginals[[j]]$levels)
    }
  }

  structure(list(
    synthetic_data = synth,
    marginals = marginals,
    correlation = R_priv,
    epsilon = epsilon, delta = delta,
    method = "copula"
  ), class = c("dp_copula", "dp_synthetic"))
}

# Project a symmetric matrix to the nearest positive-definite matrix by
# eigenvalue clipping (Higham-style simple variant).
#' @keywords internal
nearest_correlation <- function(R, min_eig = 0.01) {
  eig <- eigen(R, symmetric = TRUE)
  vals <- pmax(eig$values, min_eig)
  out <- eig$vectors %*% diag(vals) %*% t(eig$vectors)
  out <- (out + t(out)) / 2
  sweep(sweep(out, 2, sqrt(diag(out)), `/`), 1, sqrt(diag(out)), `/`)
}
