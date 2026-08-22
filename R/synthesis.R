# ---------------------------------------------------------------------------
# synthesis.R -- orchestration: fit -> generate -> post-process
# ---------------------------------------------------------------------------

#' Differentially private synthetic data generation
#'
#' Top-level orchestrator that dispatches to a DP synthesizer, optionally
#' evaluates utility and audits disclosure risk, and attaches a privacy
#' report.
#'
#' @param data original data frame
#' @param method one of \code{"copula"}, \code{"gmm"},
#'   \code{"marginals"}, \code{"pate"}
#' @param n_synth number of synthetic records
#' @param epsilon privacy parameter
#' @param delta privacy parameter
#' @param utility_eval run \code{\link{evaluate_utility}}?
#' @param risk_audit run disclosure-risk audits?
#' @param ... further arguments passed to the underlying synthesizer
#' @return object of class \code{dp_synthesis_result}
#' @seealso \code{\link{dp_copula_synth}}, \code{\link{dp_gmm_synth}},
#'   \code{\link{dp_marginals_synth}}, \code{\link{dp_pate_synth}}
#' @examples
#' set.seed(1)
#' dat <- data.frame(x = rnorm(200), y = rnorm(200) + 0.5 * rnorm(200))
#' res <- dp_synthesize(dat, method = "copula", epsilon = 2)
#' print(res)
#' @export
dp_synthesize <- function(data, method = "copula", n_synth = nrow(data),
                          epsilon = 1.0, delta = 1e-6,
                          utility_eval = TRUE, risk_audit = TRUE, ...) {
  stopifnot(is.data.frame(data), epsilon > 0)
  if (epsilon < 0.1)
    warning("Very small epsilon may produce low-utility data.")

  result <- switch(method,
    "gmm" = dp_gmm_synth(data, n_synth = n_synth, epsilon = epsilon,
                         delta = delta, ...),
    "copula" = dp_copula_synth(data, n_synth = n_synth, epsilon = epsilon,
                               delta = delta, ...),
    "marginals" = dp_marginals_synth(data, n_synth = n_synth,
                                     epsilon = epsilon, ...),
    "pate" = dp_pate_synth(data, n_synth = n_synth, epsilon = epsilon, ...),
    stop("Unknown method: ", method)
  )

  if (utility_eval) {
    result$utility <- evaluate_utility(result$synthetic_data, data)
  }

  if (risk_audit) {
    result$risk <- list(
      membership = tryCatch(
        audit_membership_risk(result$synthetic_data, data),
        error = function(e) NULL),
      attribute = tryCatch(
        audit_attribute_disclosure(result$synthetic_data, data),
        error = function(e) NULL)
    )
  }

  result$privacy_report <- list(
    epsilon = epsilon,
    delta = delta,
    method = method,
    composition = "post-processing after single DP model release",
    budget_exhausted = FALSE
  )

  class(result) <- c("dp_synthesis_result", class(result))
  result
}

#' Print a synthesis result
#'
#' @param x a \code{dp_synthesis_result}
#' @param ... unused
#' @export
print.dp_synthesis_result <- function(x, ...) {
  cat("=== Differentially Private Synthetic Data ===\n")
  cat("Method:", x$privacy_report$method, "\n")
  cat("Privacy: (epsilon =", x$privacy_report$epsilon,
      ", delta =", x$privacy_report$delta, ")\n")
  cat("Synthetic records:", nrow(x$synthetic_data), "\n")

  if (!is.null(x$utility)) {
    cat("\n--- Utility Summary ---\n")
    if (!is.null(x$utility$propensity))
      cat("Propensity pMSE ratio:",
          round(x$utility$propensity$ratio, 4), "\n")
    if (!is.null(x$utility$multivariate) &&
        !is.na(x$utility$multivariate$correlation_distance))
      cat("Correlation distance:",
          round(x$utility$multivariate$correlation_distance, 4), "\n")
  }

  if (!is.null(x$risk) && !is.null(x$risk$membership)) {
    cat("\n--- Risk Summary ---\n")
    cat("Membership risk score:",
        round(x$risk$membership$risk_score, 4), "\n")
  }
  invisible(x)
}
