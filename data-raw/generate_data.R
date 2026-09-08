# Script to generate example datasets shipped in data/
# Run from the package root with Rscript.

set.seed(20260822)

n_adult <- 500
adult_sample <- data.frame(
  age            = as.integer(pmin(90, pmax(17, round(rlnorm(n_adult, 3.4, 0.25))))),
  workclass      = factor(sample(c("Private", "Self-emp", "Government", "?"),
                                 n_adult, prob = c(.7, .12, .13, .05), replace = TRUE)),
  education_num  = as.integer(pmin(16, pmax(1, round(rnorm(n_adult, 10, 2.6))))),
  marital_status = factor(sample(c("Married", "Never-married", "Divorced",
                                   "Separated", "Widowed"),
                                 n_adult, prob = c(.46, .33, .14, .04, .03),
                                 replace = TRUE)),
  hours_per_week = as.integer(pmin(99, pmax(1, round(rnorm(n_adult, 40.4, 12.3))))),
  sex            = factor(sample(c("Male", "Female"), n_adult, prob = c(.67, .33),
                                 replace = TRUE)),
  capital_gain   = as.integer(ifelse(runif(n_adult) < .09,
                                     sample(1000:40000, n_adult, replace = TRUE), 0L)),
  income         = factor(sample(c("<=50K", ">50K"), n_adult, prob = c(.76, .24),
                                 replace = TRUE))
)

n_acs <- 500
acs_pums_sample <- data.frame(
  age     = as.integer(pmin(95, pmax(18, round(rnorm(n_acs, 45, 17))))),
  income  = round(pmax(0, rlnorm(n_acs, 10.5, 0.6)), 0),
  educ    = factor(sample(c("HS", "SomeCollege", "Bachelors", "Advanced"),
                          n_acs, prob = c(.35, .28, .23, .14), replace = TRUE)),
  married = factor(sample(c("yes", "no"), n_acs, prob = c(.5, .5),
                          replace = TRUE)),
  weeks_worked = as.integer(sample(0:52, n_acs, prob = dbeta(1:53 / 54, 2, 1.2),
                                   replace = TRUE))
)

dir.create("data", showWarnings = FALSE)
save(adult_sample, file = "data/adult_sample.rda", compress = "xz")
save(acs_pums_sample, file = "data/acs_pums_sample.rda", compress = "xz")
cat("datasets written\n")
