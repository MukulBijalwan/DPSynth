# ---------------------------------------------------------------------------
# utility/univariate.R -- KS tests, moments, Hellinger distance
# ---------------------------------------------------------------------------

#' Univariate fidelity evaluation
#'
#' Compares original and synthetic columns with Kolmogorov-Smirnov
#' statistics, Hellinger distance (histogram-based for numeric columns,
#' Jensen-Shannon divergence for categorical columns) and relative
#' moment errors.
#'
#' @param synth synthetic data frame
#' @param orig original data frame
#' @return data frame of per-variable fidelity metrics
#' @export
evaluate_univariate <- function(synth, orig) {
  vars <- intersect(names(synth), names(orig))
  out <- data.frame(variable = vars, ks_stat = NA_real_,
                    hellinger = NA_real_, mean_rel_err = NA_real_,
                    sd_rel_err = NA_real_, stringsAsFactors = FALSE)
  for (j in seq_along(vars)) {
    v <- vars[j]
    if (is.numeric(orig[[v]]) && is.numeric(synth[[v]])) {
      x <- na.omit(orig[[v]]); y <- na.omit(synth[[v]])
      if (length(unique(x)) > 1 && length(unique(y)) > 1) {
        out$ks_stat[j] <- suppressWarnings(
          stats::ks.test(y, x)$statistic)
      }
      br <- seq(min(c(x, y)), max(c(x, y)), length.out = 21)
      h1 <- hist(x, breaks = br, plot = FALSE)$density
      h2 <- hist(y, breaks = br, plot = FALSE)$density
      out$hellinger[j] <- sqrt(sum((sqrt(h1) - sqrt(h2))^2)) / sqrt(2)
      m0 <- mean(x); s0 <- sd(x)
      if (abs(m0) > 1e-12)
        out$mean_rel_err[j] <- abs(mean(y) - m0) / abs(m0)
      if (!is.na(s0) && s0 > 1e-12)
        out$sd_rel_err[j] <- abs(sd(y) - s0) / s0
    } else {
      p <- prop.table(table(factor(na.omit(orig[[v]]))))
      q <- prop.table(table(factor(na.omit(synth[[v]]))))
      all_lv <- union(names(p), names(q))
      p <- p[all_lv]; p[is.na(p)] <- 0
      q <- q[all_lv]; q[is.na(q)] <- 0
      m <- (p + q) / 2
      jsd <- 0.5 * (sum(ifelse(p > 0, p * log2(p / m), 0)) +
                    sum(ifelse(q > 0, q * log2(q / m), 0)))
      out$hellinger[j] <- sqrt(jsd) # Hellinger-style distance from JSD
    }
  }
  out
}
