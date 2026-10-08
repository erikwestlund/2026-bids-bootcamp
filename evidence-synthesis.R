# Evidence synthesis demo for the October 9 talk: combine hazard ratios from
# ten simulated sites, the way an OHDSI network study combines its databases.
#
# The slides show sections of this file (each starts with a `## ---- label`
# line) and read `estimation-study/evidence-synthesis.rds`. Run it from the
# project root:
#
#   Rscript evidence-synthesis.R

## ---- es-simulate
library(EvidenceSynthesis)
library(survival)

set.seed(2026)
# Ten sites with the same true hazard ratio (2): five large databases and five
# small ones, where some treatment groups have few or no outcomes
large_sites <- createSimulationSettings(
  nSites = 5,
  n = 20000,
  treatedFraction = 0.2,
  hazardRatio = 2,
  randomEffectSd = 0,
  minBackgroundHazard = 2e-06,
  maxBackgroundHazard = 2e-05
)
## ---- es-simulate-small
small_sites <- createSimulationSettings(
  nSites = 5,
  n = 2000,
  treatedFraction = 0.2,
  hazardRatio = 2,
  randomEffectSd = 0,
  minBackgroundHazard = 2e-06,
  maxBackgroundHazard = 2e-05
)
site_data <- c(simulatePopulations(large_sites), simulatePopulations(small_sites))

## ---- es-fit-sites
# Each site fits its own Cox model and shares only a summary of its likelihood
fit_site <- function(population) {
  cyclops_data <- Cyclops::createCyclopsData(
    Surv(time, y) ~ x + strata(stratumId),
    data = population,
    modelType = "cox"
  )
  fit <- Cyclops::fitCyclopsModel(cyclops_data)
  list(
    normal = approximateLikelihood(fit, parameter = "x", approximation = "normal"),
    grid = approximateLikelihood(fit, parameter = "x", approximation = "adaptive grid")
  )
}
site_fits <- lapply(site_data, fit_site)

site_estimates <- do.call(rbind, lapply(site_fits, `[[`, "normal"))
site_estimates$site <- seq_along(site_data)
site_estimates$patients <- sapply(site_data, nrow)
site_estimates$outcomes <- sapply(site_data, function(population) sum(population$y))

## ---- es-traditional
# Traditional random-effects meta-analysis on each site's log HR and SE.
# Sites whose estimate or SE is undefined have to be dropped.
usable <- is.finite(site_estimates$seLogRr)
traditional <- meta::metagen(
  TE = site_estimates$logRr[usable],
  seTE = site_estimates$seLogRr[usable],
  sm = "HR"
)

## ---- es-likelihood
# OHDSI's approach: combine each site's full likelihood curve, so every site
# contributes, including those with few or no outcomes
site_grids <- lapply(site_fits, `[[`, "grid")
fixed_effect <- computeFixedEffectMetaAnalysis(site_grids)
random_effects <- computeBayesianMetaAnalysis(site_grids)

## ---- es-save
pooled <- data.frame(
  method = c(
    "Traditional random effects",
    "Likelihood-based fixed effect",
    "Likelihood-based random effects (Bayesian)"
  ),
  sites_used = c(sum(usable), length(site_data), length(site_data)),
  hr = c(exp(traditional$TE.random), fixed_effect$rr, exp(random_effects$mu)),
  lower = c(exp(traditional$lower.random), fixed_effect$lb, exp(random_effects$mu95Lb)),
  upper = c(exp(traditional$upper.random), fixed_effect$ub, exp(random_effects$mu95Ub))
)
saveRDS(
  list(
    site_estimates = site_estimates,
    pooled = pooled,
    true_hr = 2,
    site_grids = site_grids,
    random_effects = random_effects
  ),
  "estimation-study/evidence-synthesis.rds"
)
