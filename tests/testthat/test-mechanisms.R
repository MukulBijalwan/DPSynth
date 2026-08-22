test_that("rlaplace has correct location/scale", {
  set.seed(42)
  x <- rlaplace(200000, 0, 2)
  expect_equal(mean(x), 0, tolerance = 0.02)
  expect_equal(stats::sd(x), sqrt(8), tolerance = 0.05)
})

test_that("gaussian_mech adds calibrated noise", {
  set.seed(1)
  v <- gaussian_mech(0, sensitivity = 1, epsilon = 1, delta = 1e-6)
  expect_true(is.finite(v))
  expect_error(gaussian_mech(0, -1, 1), "sensitivity")
})

test_that("clip_data clamps to bounds", {
  d <- data.frame(x = c(-5, 0, 10))
  out <- clip_data(d, list(lower = 0, upper = 5))
  expect_equal(out$x, c(0, 0, 5))
})
