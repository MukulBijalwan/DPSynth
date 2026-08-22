# ---------------------------------------------------------------------------
# dp_marginals.R -- DP histogram / KDE marginal synthesis
# ---------------------------------------------------------------------------

#' DP marginal synthesis
#'
#' Synthesizes each column independently from a differentially private
#' histogram (numeric columns) or DP multinomial (categorical columns).
#' No dependence structure is preserved; useful as a fast baseline and
#' for consistent-marginal releases in official statistics.
#'
#' @param data data frame
#' @param n_synth number of synthetic records
#' @param epsilon privacy parameter epsilon (split evenly across columns)
#' @param n_bins number of bins for numeric columns
#' @return object of class \code{dp_marginals} / \code{dp_synthetic}
#' @export
dp_marginals_synth <- function(data, n_synth = nrow(data),
                               epsilon = 1.0, n_bins = 20) {
  stopifnot(is.data.frame(data), epsilon > 0, n_bins >= 2)
  d <- ncol(data)
  eps_j <- epsilon / d
  synth <- as.data.frame(matrix(NA, n_synth, d), stringsAsFactors = FALSE)
  names(synth) <- names(data)
  marginals <- vector("list", d)

  for (j in seq_len(d)) {
    xj <- data[[j]]
    if (is.numeric(xj)) {
      rng <- range(xj, na.rm = TRUE)
      if (diff(rng) == 0) rng[2] <- rng[1] + 1e-6
      breaks <- seq(rng[1], rng[2], length.out = n_bins + 1)
      counts <- as.numeric(hist(xj, breaks = breaks, plot = FALSE)$counts)
      dp_counts <- pmax(counts + rlaplace(n_bins, 0, 1 / eps_j), 0)
      probs <- dp_counts / max(sum(dp_counts), 1e-12)
      mids <- (breaks[-1] + breaks[-(n_bins + 1)]) / 2
      synth[[j]] <- sample(mids, n_synth, prob = probs, replace = TRUE)
      marginals[[j]] <- list(type = "numeric", breaks = breaks,
                             probs = probs)
    } else {
      lv <- if (is.factor(xj)) levels(xj) else sort(unique(as.character(xj)))
      tab <- as.numeric(table(factor(xj, levels = lv)))
      dp_tab <- pmax(tab + rlaplace(length(tab), 0, 1 / eps_j), 0)
      probs <- dp_tab / max(sum(dp_tab), 1e-12)
      synth[[j]] <- sample(lv, n_synth, prob = probs, replace = TRUE)
      synth[[j]] <- factor(synth[[j]], levels = lv)
      marginals[[j]] <- list(type = "categorical", levels = lv,
                             probs = probs)
    }
  }

  structure(list(
    synthetic_data = synth,
    marginals = marginals,
    epsilon = epsilon,
    method = "marginals"
  ), class = c("dp_marginals", "dp_synthetic"))
}
