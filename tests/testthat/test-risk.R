test_that("budget accounting composes correctly", {
  b <- new_synth_budget(epsilon = 5, delta = 1e-6, accounting = "basic")
  b <- spend_synth(b, 3, description = "gmm")
  b <- spend_synth(b, 2, description = "copula")
  expect_equal(b$epsilon_spent, 5)
  expect_warning(spend_synth(b, 1), "exceeded")

  r <- new_synth_budget(epsilon = 10, delta = 1e-6, accounting = "rdp")
  r <- spend_synth(r, 1)
  r <- spend_synth(r, 1)
  # alpha=2: total RDP = 2*(1^2/2)=1, + log(1e6)/1 ~ 14.8 -> exceeded
  expect_gt(r$epsilon_spent, r$epsilon_total)
})

test_that("risk audits return finite summaries", {
  set.seed(31)
  orig <- data.frame(x = rnorm(150), y = rnorm(150))
  synth <- data.frame(x = rnorm(150), y = rnorm(150))
  m <- audit_membership_risk(synth, orig)
  a <- audit_attribute_disclosure(synth, orig)
  l <- audit_linkage_risk(synth, orig)
  expect_true(m$risk_score >= 0 && m$risk_score <= 1)
  expect_true(is.finite(a$mean_disclosure_error))
  expect_true(l$linkage_rate >= 0 && l$linkage_rate <= 1)
})
