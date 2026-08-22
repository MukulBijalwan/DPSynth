# ---------------------------------------------------------------------------
# budget/accounting.R -- privacy budget class and accounting
# ---------------------------------------------------------------------------

#' Create a privacy budget tracker
#'
#' @param epsilon total epsilon available
#' @param delta total delta available
#' @param accounting one of \code{"basic"}, \code{"advanced"}, \code{"rdp"}
#' @return object of class \code{synth_privacy_budget}
#' @export
new_synth_budget <- function(epsilon, delta = 0,
                             accounting = c("basic", "advanced", "rdp")) {
  accounting <- match.arg(accounting)
  stopifnot(epsilon > 0, delta >= 0, delta < 1)
  structure(list(
    epsilon_total = epsilon,
    delta_total = delta,
    epsilon_spent = 0,
    delta_spent = 0,
    accounting = accounting,
    steps = list()
  ), class = "synth_privacy_budget")
}

#' Spend privacy budget on a step
#'
#' Records a DP release consuming \code{epsilon} (and optionally
#' \code{delta}) and updates the accumulated spend under the configured
#' composition rule. Issues a warning when the total budget is exceeded.
#'
#' @param budget a \code{synth_privacy_budget} object
#' @param epsilon epsilon spent by this step
#' @param delta delta spent by this step
#' @param description free-text description of the step
#' @return updated budget object
#' @export
spend_synth <- function(budget, epsilon, delta = 0, description = "") {
  stopifnot(inherits(budget, "synth_privacy_budget"), epsilon >= 0)
  step_no <- length(budget$steps) + 1L
  budget$steps[[step_no]] <- list(epsilon = epsilon, delta = delta,
                                  description = description)

  if (budget$accounting == "basic") {
    budget$epsilon_spent <- budget$epsilon_spent + epsilon
    budget$delta_spent <- budget$delta_spent + delta
  } else if (budget$accounting == "advanced") {
    k <- length(budget$steps)
    eps_k <- budget$steps[[k]]$epsilon
    if (budget$delta_total > 0) {
      budget$epsilon_spent <-
        sqrt(2 * k * log(1 / budget$delta_total)) * eps_k +
        k * eps_k * (exp(eps_k) - 1)
    } else {
      budget$epsilon_spent <- k * eps_k
    }
    budget$delta_spent <- budget$delta_total
  } else { # rdp
    alpha <- 2
    total_rdp <- sum(vapply(budget$steps, function(s)
      alpha * s$epsilon^2 / 2, numeric(1)))
    conv <- if (budget$delta_total > 0)
      log(1 / budget$delta_total) / (alpha - 1) else 0
    budget$epsilon_spent <- total_rdp + conv
  }

  if (budget$epsilon_spent > budget$epsilon_total) {
    warning("Privacy budget exceeded! Synthesis may not satisfy DP.")
  }
  budget
}

#' Check remaining privacy budget
#'
#' @param budget a \code{synth_privacy_budget} object
#' @return named vector with \code{remaining_epsilon}, \code{spent_epsilon}
#'   and \code{fraction_used}; also prints a summary
#' @export
check_synth_budget <- function(budget) {
  remaining <- max(budget$epsilon_total - budget$epsilon_spent, 0)
  out <- c(remaining_epsilon = remaining,
           spent_epsilon = budget$epsilon_spent,
           fraction_used = budget$epsilon_spent /
             max(budget$epsilon_total, 1e-12))
  print(out)
  invisible(out)
}

#' @export
print.synth_privacy_budget <- function(x, ...) {
  cat("=== Privacy Budget ===\n")
  cat("Accounting:", x$accounting, "\n")
  cat(sprintf("Spent: %.4f / %.4f epsilon\n",
              x$epsilon_spent, x$epsilon_total))
  cat("Steps:", length(x$steps), "\n")
  invisible(x)
}
