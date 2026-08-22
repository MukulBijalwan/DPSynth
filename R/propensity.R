# ---------------------------------------------------------------------------
# utility/propensity.R -- propensity score pMSE (Woo et al., 2009)
# ---------------------------------------------------------------------------

#' Propensity score utility (pMSE)
#'
#' Fits a logistic model to distinguish synthetic from original records.
#' Under perfect synthesis the expected pMSE is \eqn{c(1-c)} with
#' \eqn{c = n_synth / (n_orig + n_synth)}; the reported ratio is 0 for
#' indistinguishable data and grows as fidelity degrades.
#'
#' @param synth synthetic data frame
#' @param orig original data frame
#' @return list with \code{pMSE}, \code{ratio} (standardised) and \code{c}
#' @export
evaluate_propensity <- function(synth, orig) {
  common_vars <- intersect(names(orig), names(synth))
  n1 <- nrow(orig); n2 <- nrow(synth)

  combined <- rbind(
    cbind(orig[, common_vars, drop = FALSE], source = 0),
    cbind(synth[, common_vars, drop = FALSE], source = 1)
  )
  combined$source <- factor(combined$source)
  # Drop columns that break glm (all-NA, constant, list-like)
  keep <- vapply(common_vars, function(v) {
    x <- combined[[v]]
    is.numeric(x) || is.factor(x) || is.character(x)
  }, logical(1))
  combined <- combined[, c(names(keep)[keep], "source"), drop = FALSE]
  rhs <- paste(names(keep)[keep], collapse = " + ")
  if (!nzchar(rhs)) rhs <- "1"

  model <- suppressWarnings(
    stats::glm(stats::as.formula(paste("source ~", rhs)),
               data = combined, family = stats::binomial()))
  p_hat <- as.numeric(stats::predict(model, type = "response"))

  c_prop <- n2 / (n1 + n2)
  pMSE <- mean((p_hat - c_prop)^2)
  list(pMSE = pMSE,
       ratio = pMSE / max(c_prop * (1 - c_prop), 1e-12),
       c = c_prop)
}
