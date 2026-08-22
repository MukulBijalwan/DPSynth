# ---------------------------------------------------------------------------
# utils.R -- small internal helpers
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

complete_numeric_rows <- function(data) {
  nums <- vapply(data, is.numeric, logical(1))
  if (!any(nums)) return(data)
  data[stats::complete.cases(data[nums]), , drop = FALSE]
}
