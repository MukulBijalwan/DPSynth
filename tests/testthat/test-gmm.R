test_that("dp_gmm_synth produces valid output", {
  set.seed(7)
  dat <- data.frame(x = rnorm(200), y = rnorm(200) + 0.5 * rnorm(200))
  res <- dp_gmm_synth(dat, n_synth = 50, K = 2, epsilon = 2,
                      max_iter = 3)
  expect_s3_class(res, "dp_synthetic")
  expect_equal(nrow(res$synthetic_data), 50)
  expect_true(all(vapply(res$synthetic_data, is.numeric, logical(1))))
  expect_true(all(res$parameters$variances > 0))
})

test_that("dp_pate_synth handles mixed types", {
  set.seed(8)
  dat <- data.frame(x = rnorm(100),
                    g = factor(sample(c("a", "b"), 100, replace = TRUE)))
  res <- suppressWarnings(dp_pate_synth(dat, n_synth = 20, epsilon = 1,
                                        n_teachers = 5))
  expect_s3_class(res, "dp_synthetic")
  expect_s3_class(res$synthetic_data$g, "factor")
})
