# DPSynth

**Differentially Private Synthetic Data with Guaranteed Utility**

R package implementing differentially private (DP) synthetic data generation
for tabular data, with standardized utility evaluation and empirical
disclosure risk auditing.

Author: **Mukul Bijalwan** (<mukulbijalwan555@gmail.com>)

## Features

- **DP-Copula synthesizer** (`dp_copula_synth`) — DP histogram marginals +
  Gaussian-copula dependence with noisy rank correlations projected to the
  positive-definite cone. Handles mixed numeric/categorical data.
- **DP-GMM** (`dp_gmm_synth`) — Gaussian mixture models fit via EM on
  perturbed sufficient statistics (noisy counts, means, variances), DP
  k-means++ initialization through the exponential mechanism.
- **DP marginals** (`dp_marginals_synth`) — fast per-column DP histogram
  baseline.
- **PATE** (`dp_pate_synth`) — Private Aggregation of Teacher Ensembles for
  discrete / mixed-type synthesis.
- **Utility evaluation** (`evaluate_utility`) — univariate (KS, Hellinger),
  multivariate (correlation distance, MI), propensity-score pMSE
  (Woo et al., 2009), downstream TSTR performance.
- **Risk auditing** — membership inference, attribute disclosure, record
  linkage.
- **Budget accounting** (`new_synth_budget`, `spend_synth`) — basic,
  advanced and Rényi-DP composition.

## Installation

```r
# install.packages("remotes")
remotes::install_github("MukulBijalwan/DPSynth")
```

## Quick start

```r
library(DPSynth)

data(adult_sample)
set.seed(1)
res <- dp_synthesize(adult_sample, method = "copula",
                     epsilon = 2.0, delta = 1e-6, n_synth = 1000)
print(res)
head(res$synthetic_data)
```

## Privacy model

A generator G is (ε, δ)-DP if for all neighbouring datasets D ~ D′:

    Pr[G(D) = S] ≤ e^ε · Pr[G(D′) = S] + δ

The entire synthetic dataset is the output; DPSynth privatizes the model
parameters with calibrated noise, after which sampling is post-processing
and consumes no additional budget.

## Note on formal guarantees

Bounds derived from the data itself (e.g., min/max clamping bounds by
default in `dp_gmm_synth`) are not themselves differentially private.
For formal end-to-end guarantees, supply public/domain bounds via the
`bounds` argument.

## License

MIT © 2026 Mukul Bijalwan
