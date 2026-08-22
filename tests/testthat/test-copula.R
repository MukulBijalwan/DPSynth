test_that("copula generation is stable with tiny epsilon", {
  set.seed(41)
  dat <- data.frame(a = rnorm(100), b = rlnorm(100))
  res <- suppressWarnings(dp_copula_synth(dat, n_synth = 50,
                                          epsilon = 0.05, n_bins = 5))
  expect_true(all(is.finite(res$synthetic_data$a)))
  expect_true(!is.null(res$marginals))
})
