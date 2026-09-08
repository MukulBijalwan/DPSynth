# ---------------------------------------------------------------------------
# dp_pate.R -- Private Aggregation of Teacher Ensembles
# ---------------------------------------------------------------------------

#' PATE synthetic data for discrete / mixed-type data
#'
#' Partitions the private data into disjoint teacher datasets, fits a
#' teacher model on each partition (no privacy cost), and releases each
#' per-record query through a noisy aggregation of teacher votes. The
#' privacy cost scales with the number of released records.
#'
#' @param data data frame
#' @param n_synth number of synthetic records
#' @param epsilon total privacy parameter (spent across all released
#'   records via basic composition)
#' @param n_teachers number of disjoint teacher partitions
#' @return object of class \code{dp_pate} / \code{dp_synthetic}
#' @export
dp_pate_synth <- function(data, n_synth = nrow(data), epsilon = 1.0,
                          n_teachers = 10) {
  stopifnot(is.data.frame(data), epsilon > 0, n_teachers >= 2)
  n <- nrow(data)
  d <- ncol(data)
  n_synth <- as.integer(n_synth)

  teacher_idx <- sample(rep(seq_len(n_teachers), length.out = n))
  teachers <- lapply(seq_len(n_teachers), function(m) {
    idx <- which(teacher_idx == m)
    if (length(idx) == 0) idx <- sample.int(n, 1)
    fit_teacher(data[idx, , drop = FALSE])
  })

  # Basic composition across n_synth * d released statistics
  eps_per_query <- epsilon / max(n_synth * d, 1)

  synth <- as.data.frame(matrix(NA, n_synth, d), stringsAsFactors = FALSE)
  names(synth) <- names(data)

  for (j in seq_len(d)) {
    xj <- data[[j]]
    if (is.numeric(xj)) {
      sens <- diff(range(xj, na.rm = TRUE))
      if (sens == 0) sens <- 1e-6
      for (i in seq_len(n_synth)) {
        votes <- vapply(teachers, function(t) t$numeric[[j]]$mean, numeric(1))
        synth[i, j] <- mean(votes) +
          rlaplace(1, 0, sens / (n_teachers * eps_per_query))
      }
    } else {
      lv <- if (is.factor(xj)) levels(xj) else sort(unique(as.character(xj)))
      for (i in seq_len(n_synth)) {
        votes <- vapply(teachers, function(t)
          t$categorical[[j]]$mode, character(1))
        tab <- as.numeric(table(factor(votes, levels = lv)))
        noisy_tab <- pmax(tab + rlaplace(length(tab), 0,
                                         1 / eps_per_query), 0)
        # floor to keep the sampling distribution well-defined
        noisy_tab <- noisy_tab + 1e-9
        synth[i, j] <- sample(lv, 1,
                              prob = noisy_tab / sum(noisy_tab))
      }
      synth[[j]] <- factor(synth[[j]], levels = lv)
    }
  }

  structure(list(
    synthetic_data = synth,
    n_teachers = n_teachers,
    epsilon = epsilon,
    method = "pate"
  ), class = c("dp_pate", "dp_synthetic"))
}

# Internal teacher models: per-column mean / mode
#' @keywords internal
fit_teacher <- function(data) {
  numeric <- lapply(seq_along(data), function(j)
    list(mean = mean(data[[j]], na.rm = TRUE)))
  names(numeric) <- names(data)
  categorical <- lapply(seq_along(data), function(j) {
    xj <- data[[j]]
    if (is.numeric(xj)) return(list(mode = NA_character_))
    lv <- if (is.factor(xj)) levels(xj) else sort(unique(as.character(xj)))
    tab <- table(factor(xj, levels = lv))
    list(mode = names(tab)[which.max(tab)])
  })
  names(categorical) <- names(data)
  list(numeric = numeric, categorical = categorical)
}
