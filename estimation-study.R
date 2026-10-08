# Estimation study for the October 9 talk: celecoxib vs. diclofenac and GI bleed
# on a simulated copy of the Eunomia GiBleed database (see simulate-eunomia.R),
# run with Strategus.
#
# The slides show sections of this file (each starts with a `## ---- label`
# line) and read the results it writes to `estimation-study/results/`.
# Run it from the project root:
#
#   Rscript estimation-study.R

## ---- packages
library(Strategus)

## ---- cohorts
# Celecoxib (1), diclofenac (2), and GI bleed (3) cohorts, plus 14 negative
# control outcomes. Strategus ships these definitions as test data.
cohort_definition_set <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "testdata/Cohorts.csv",
  jsonFolder = "testdata/cohorts",
  sqlFolder = "testdata/sql",
  packageName = "Strategus"
)
negative_control_set <- CohortGenerator::readCsv(
  system.file("testdata/negative_controls_concept_set.csv", package = "Strategus")
)

cg_module <- CohortGeneratorModule$new()
cohort_shared_resource <- cg_module$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohort_definition_set
)
negative_control_shared_resource <- cg_module$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negative_control_set,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cg_module_specs <- cg_module$createModuleSpecifications(generateStats = TRUE)

## ---- cm-tco
celecoxib_concept_id <- 1118084
diclofenac_concept_id <- 1124300

outcome_of_interest <- CohortMethod::createOutcome(outcomeId = 3, outcomeOfInterest = TRUE)
negative_control_outcomes <- lapply(
  negative_control_set$cohortId,
  function(id) CohortMethod::createOutcome(outcomeId = id, outcomeOfInterest = FALSE, trueEffectSize = 1)
)

tco <- CohortMethod::createTargetComparatorOutcomes(
  targetId = 1,
  comparatorId = 2,
  outcomes = c(list(outcome_of_interest), negative_control_outcomes),
  # The two drugs predict treatment perfectly, so keep them out of the PS model
  excludedCovariateConceptIds = c(celecoxib_concept_id, diclofenac_concept_id)
)

## ---- cm-shared-args
get_data_args <- CohortMethod::createGetDbCohortMethodDataArgs(
  firstExposureOnly = TRUE,
  washoutPeriod = 183,
  removeDuplicateSubjects = "keep first, truncate to second",
  restrictToCommonPeriod = TRUE,
  covariateSettings = list(
    FeatureExtraction::createDefaultCovariateSettings(addDescendantsToExclude = TRUE),
    FeatureExtraction::createCovariateSettings(useConditionGroupEraAnyTimePrior = TRUE)
  )
)

study_pop_args <- CohortMethod::createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 90,
  endAnchor = "cohort start"
)

## ---- cm-ps-args
ps_args <- CohortMethod::createCreatePsArgs(
  # Eunomia is small, so allow the run to continue if the PS model has trouble
  stopOnError = FALSE,
  control = Cyclops::createControl(cvRepetitions = 1, seed = 1)
)
shared_balance_args <- CohortMethod::createComputeCovariateBalanceArgs()
table1_balance_args <- CohortMethod::createComputeCovariateBalanceArgs(
  covariateFilter = CohortMethod::getDefaultCmTable1Specifications()
)

## ---- cm-match
crude <- CohortMethod::createCmAnalysis(
  analysisId = 1,
  description = "Unadjusted",
  getDbCohortMethodDataArgs = get_data_args,
  createStudyPopulationArgs = study_pop_args,
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(modelType = "cox")
)

match_1to1 <- CohortMethod::createCmAnalysis(
  analysisId = 2,
  description = "1:1 matching",
  getDbCohortMethodDataArgs = get_data_args,
  createStudyPopulationArgs = study_pop_args,
  createPsArgs = ps_args,
  matchOnPsArgs = CohortMethod::createMatchOnPsArgs(maxRatio = 1, caliper = 0.2),
  computeSharedCovariateBalanceArgs = shared_balance_args,
  computeCovariateBalanceArgs = table1_balance_args,
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(modelType = "cox", stratified = FALSE)
)

## ---- cm-nto1
# Up to ten diclofenac users per celecoxib user. If celecoxib users were the
# larger group, allowReverseMatch would instead match several of them to each
# diclofenac user.
match_variable <- CohortMethod::createCmAnalysis(
  analysisId = 3,
  description = "Variable-ratio matching (up to 1:10)",
  getDbCohortMethodDataArgs = get_data_args,
  createStudyPopulationArgs = study_pop_args,
  createPsArgs = ps_args,
  matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
    maxRatio = 10,
    allowReverseMatch = TRUE,
    caliper = 0.2
  ),
  computeSharedCovariateBalanceArgs = shared_balance_args,
  computeCovariateBalanceArgs = table1_balance_args,
  # With more than one match per patient, condition on the matched sets
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(modelType = "cox", stratified = TRUE)
)

## ---- cm-stratify
stratify <- CohortMethod::createCmAnalysis(
  analysisId = 4,
  description = "Stratification (10 strata)",
  getDbCohortMethodDataArgs = get_data_args,
  createStudyPopulationArgs = study_pop_args,
  createPsArgs = ps_args,
  stratifyByPsArgs = CohortMethod::createStratifyByPsArgs(numberOfStrata = 10),
  computeSharedCovariateBalanceArgs = shared_balance_args,
  computeCovariateBalanceArgs = table1_balance_args,
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(modelType = "cox", stratified = TRUE)
)

## ---- cm-iptw
iptw <- CohortMethod::createCmAnalysis(
  analysisId = 5,
  description = "Inverse probability weighting",
  getDbCohortMethodDataArgs = get_data_args,
  createStudyPopulationArgs = study_pop_args,
  createPsArgs = ps_args,
  truncateIptwArgs = CohortMethod::createTruncateIptwArgs(maxWeight = 10),
  computeSharedCovariateBalanceArgs = shared_balance_args,
  computeCovariateBalanceArgs = table1_balance_args,
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
    modelType = "cox",
    inversePtWeighting = TRUE,
    # Robust CIs for a weighted Cox model come from the bootstrap
    bootstrapCi = TRUE
  )
)

## ---- cm-dr
match_plus_covariates <- CohortMethod::createCmAnalysis(
  analysisId = 6,
  description = "1:1 matching plus covariates (doubly robust)",
  getDbCohortMethodDataArgs = get_data_args,
  createStudyPopulationArgs = study_pop_args,
  createPsArgs = ps_args,
  matchOnPsArgs = CohortMethod::createMatchOnPsArgs(maxRatio = 1, caliper = 0.2),
  computeSharedCovariateBalanceArgs = shared_balance_args,
  computeCovariateBalanceArgs = table1_balance_args,
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(modelType = "cox", useCovariates = TRUE)
)

## ---- cm-module
cm_specs <- CohortMethod::createCmAnalysesSpecifications(
  cmAnalysisList = list(crude, match_1to1, match_variable, stratify, iptw, match_plus_covariates),
  targetComparatorOutcomesList = list(tco),
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds(
    mdrrThreshold = 10,
    easeThreshold = 0.25,
    sdmThreshold = 0.1,
    sdmAlpha = 0.05,
    equipoiseThreshold = 0.2
  )
)
cm_module_specs <- CohortMethodModule$new()$createModuleSpecifications(
  cmAnalysesSpecifications = cm_specs$toList()
)

## ---- sccs-exposures
# Both drugs enter the model, so the SCCS can compare celecoxib with
# diclofenac, the same comparison as the cohort study
both_drugs <- list(
  SelfControlledCaseSeries::createExposure(exposureId = 1, exposureIdRef = "celecoxib"),
  SelfControlledCaseSeries::createExposure(exposureId = 2, exposureIdRef = "diclofenac")
)
exposure_outcomes <- c(
  list(SelfControlledCaseSeries::createExposuresOutcome(outcomeId = 3, exposures = both_drugs)),
  lapply(negative_control_set$cohortId, function(id) {
    SelfControlledCaseSeries::createExposuresOutcome(
      outcomeId = id,
      exposures = list(
        SelfControlledCaseSeries::createExposure(exposureId = 1, exposureIdRef = "celecoxib", trueEffectSize = 1),
        SelfControlledCaseSeries::createExposure(exposureId = 2, exposureIdRef = "diclofenac")
      )
    )
  })
)

## ---- sccs-windows
# A pre-exposure window detects outcomes that change the chance of treatment
pre_exposure <- SelfControlledCaseSeries::createEraCovariateSettings(
  label = "Pre-exposure",
  includeEraIds = c("celecoxib", "diclofenac"),
  start = -30,
  end = -1,
  endAnchor = "era start"
)
# The 90 days after starting either drug: the diclofenac rate ratio
any_nsaid <- SelfControlledCaseSeries::createEraCovariateSettings(
  label = "Either drug, days 0-90",
  includeEraIds = c("celecoxib", "diclofenac"),
  start = 0,
  end = 90,
  endAnchor = "era start"
)
## ---- sccs-contrast
# An extra term for celecoxib only. Its rate ratio is celecoxib vs. diclofenac.
celecoxib_vs_diclofenac <- SelfControlledCaseSeries::createEraCovariateSettings(
  label = "Celecoxib vs. diclofenac, days 0-90",
  includeEraIds = "celecoxib",
  start = 0,
  end = 90,
  endAnchor = "era start",
  exposureOfInterest = TRUE
)

## ---- sccs-analysis
interval_args <- SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(pre_exposure, any_nsaid, celecoxib_vs_diclofenac),
  ageCovariateSettings = SelfControlledCaseSeries::createAgeCovariateSettings(ageKnots = 5),
  seasonalityCovariateSettings = SelfControlledCaseSeries::createSeasonalityCovariateSettings(seasonKnots = 5)
)

sccs_analysis <- SelfControlledCaseSeries::createSccsAnalysis(
  analysisId = 1,
  description = "SCCS: celecoxib vs. diclofenac",
  getDbSccsDataArgs = SelfControlledCaseSeries::createGetDbSccsDataArgs(
    exposureIds = c("celecoxib", "diclofenac"),
    deleteCovariatesSmallCount = 0
  ),
  # The simulated bleeds recur independently, so every bleed can be used
  createStudyPopulationArgs = SelfControlledCaseSeries::createCreateStudyPopulationArgs(
    firstOutcomeOnly = FALSE,
    naivePeriod = 180
  ),
  createIntervalDataArgs = interval_args,
  fitSccsModelArgs = SelfControlledCaseSeries::createFitSccsModelArgs()
)

## ---- sccs-module
sccs_specs <- SelfControlledCaseSeries::createSccsAnalysesSpecifications(
  sccsAnalysisList = list(sccs_analysis),
  exposuresOutcomeList = exposure_outcomes
)
sccs_module_specs <- SelfControlledCaseSeriesModule$new()$createModuleSpecifications(
  sccsAnalysesSpecifications = sccs_specs$toList()
)

## ---- analysis-spec
analysis_spec <- createEmptyAnalysisSpecifications() |>
  addSharedResources(cohort_shared_resource) |>
  addSharedResources(negative_control_shared_resource) |>
  addModuleSpecifications(cg_module_specs) |>
  addModuleSpecifications(cm_module_specs) |>
  addModuleSpecifications(sccs_module_specs)

## ---- connect
# Strategus wants absolute paths for its output folders
study_folder <- file.path(getwd(), "estimation-study")
dir.create(study_folder, showWarnings = FALSE)

# The simulated copy of Eunomia written by simulate-eunomia.R. DuckDB has
# strict column types, which CohortMethod needs here.
connection_details <- DatabaseConnector::createConnectionDetails(
  dbms = "duckdb",
  server = file.path(study_folder, "eunomia-simulated.duckdb")
)

## ---- execute
execution_settings <- createCdmExecutionSettings(
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames("cohort"),
  workFolder = file.path(study_folder, "work"),
  resultsFolder = file.path(study_folder, "results"),
  minCellCount = 5,
  # Incremental mode passes the negative control IDs to the database as text
  incremental = FALSE,
  # A DuckDB file accepts one connection at a time, so run on a single thread
  maxCores = 1
)

execute(
  analysisSpecifications = analysis_spec,
  executionSettings = execution_settings,
  connectionDetails = connection_details
)
