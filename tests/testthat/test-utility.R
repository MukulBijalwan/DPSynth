test_that("utility metrics are well-behaved on identical data", {
  set.seed(21)
  orig <- data.frame(x = rnorm(200), y = rnorm(200),
                     g = factor(sample(c("u", "v"), 200, replace = TRUE)))
  rep <- evaluate_utility(orig, orig)
  # identical copies must be indistinguishable
  expect_lt(rep$propensity$ratio, 0.5)
  expect_lt(rep$multivariate$correlation_distance, 1e-6)
  expect_true(all(is.finite(rep$univariate$hellinger)))

  ds <- evaluate_downstream(orig, orig, target_var = "x")
  expect_lt(ds$utility_ratio, 1.05)
})

test_that("evaluate_downstream validates inputs", {
  d <- data.frame(x = 1:10, y = rnorm(10))
  expect_error(evaluate_downstream(d, d, "nope"))
})
