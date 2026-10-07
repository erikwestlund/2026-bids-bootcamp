# Simulated outcomes for the October 9 talk.
#
# Eunomia's own GI bleeds all happen within 90 days of an NSAID, which makes a
# self-controlled case series impossible to fit. This script makes a copy of
# Eunomia and replaces the drug choice and the outcomes with simulated ones,
# so that a comparative cohort study and an SCCS have the same known answer.
#
# - Each person's NSAID (celecoxib or diclofenac) is reassigned. Older patients
#   and patients with a prior peptic ulcer are more likely to get celecoxib.
# - GI bleeds happen throughout follow-up at a background rate that rises with
#   age and with a prior ulcer. In the 90 days after starting the drug the rate
#   is multiplied by 1.4 (celecoxib) or 2.0 (diclofenac).
# - The true celecoxib vs. diclofenac rate ratio is therefore 1.4 / 2.0 = 0.7.
# - The negative control outcomes follow the same background model, with no
#   drug effect.
#
# The slides show sections of this file (each starts with a `## ---- label`
# line). Run it from the project root:
#
#   Rscript simulate-eunomia.R

## ---- sim-truth
true_effects <- c(celecoxib = 1.4, diclofenac = 2.0)
true_rate_ratio <- true_effects[["celecoxib"]] / true_effects[["diclofenac"]]
risk_window_days <- 90

## ---- sim-setup
library(DBI)
set.seed(2026)

study_folder <- file.path(getwd(), "estimation-study")
dir.create(study_folder, showWarnings = FALSE)
database_file <- file.path(study_folder, "eunomia-simulated.duckdb")
unlink(database_file)
invisible(Eunomia::getEunomiaConnectionDetails(databaseFile = database_file, dbms = "duckdb"))
con <- dbConnect(duckdb::duckdb(), database_file)

celecoxib_concept_id <- 1118084
diclofenac_concept_id <- 1124300
gi_bleed_concept_id <- 192671
peptic_ulcer_concept_id <- 4027663
negative_control_concept_ids <- CohortGenerator::readCsv(
  system.file("testdata/negative_controls_concept_set.csv", package = "Strategus")
)$outcomeConceptId

## ---- sim-patients
# One row per person: the date of their NSAID, their age, and whether a
# peptic ulcer was recorded before it
patients <- dbGetQuery(con, sprintf("
  SELECT de.person_id,
         de.drug_exposure_start_date AS index_date,
         p.birth_datetime::DATE AS birth_date,
         op.observation_period_start_date AS obs_start,
         op.observation_period_end_date AS obs_end,
         EXISTS (
           SELECT 1 FROM condition_occurrence co
           WHERE co.person_id = de.person_id
             AND co.condition_concept_id = %d
             AND co.condition_start_date < de.drug_exposure_start_date
         ) AS prior_ulcer
  FROM drug_exposure de
  JOIN person p ON p.person_id = de.person_id
  JOIN observation_period op ON op.person_id = de.person_id
  WHERE de.drug_concept_id IN (%d, %d)",
  peptic_ulcer_concept_id, celecoxib_concept_id, diclofenac_concept_id
))
patients$age <- as.numeric(patients$index_date - patients$birth_date) / 365.25
# A few NSAIDs fall outside the person's observation period. Those people
# cannot enter either study, so leave them out.
patients <- patients[patients$index_date >= patients$obs_start & patients$index_date <= patients$obs_end, ]

## ---- sim-treatment
# Confounding by indication: celecoxib goes to older patients and to patients
# with a prior ulcer
logit <- -0.5 + 1.5 * patients$prior_ulcer + 0.3 * (patients$age - 40) / 10
patients$celecoxib <- rbinom(nrow(patients), size = 1, prob = plogis(logit))

## ---- sim-follow-up
# Eunomia follows people for their whole lives. Shorten follow-up to one to
# three years on each side of the NSAID, as in a typical claims database.
patients$obs_start <- pmax(patients$obs_start, patients$index_date - round(runif(nrow(patients), 365, 3 * 365)))
patients$obs_end <- pmin(patients$obs_end, patients$index_date + round(runif(nrow(patients), 365, 3 * 365)))

## ---- sim-outcomes
# Draws outcome dates for one person from a Poisson process. The rate changes
# with age, a prior ulcer, and (optionally) the 90 days after the NSAID.
simulate_events <- function(patient, base_rate, ulcer_effect, age_effect, drug_effect) {
  # Split follow-up into pieces of at most 30 days, cut at the start and end
  # of the risk window, so the rate is constant within each piece
  follow_up_end <- patient$obs_end + 1
  cuts <- c(
    seq(patient$obs_start, follow_up_end, by = 30),
    follow_up_end,
    patient$index_date,
    patient$index_date + risk_window_days
  )
  cuts <- sort(unique(cuts[cuts >= patient$obs_start & cuts <= follow_up_end]))
  piece_start <- head(cuts, -1)
  piece_days <- as.numeric(diff(cuts))

  age <- as.numeric(piece_start - patient$birth_date) / 365.25
  in_window <- piece_start >= patient$index_date &
    piece_start < patient$index_date + risk_window_days
  rate_per_year <- base_rate *
    ulcer_effect^patient$prior_ulcer *
    age_effect^((age - 40) / 10) *
    ifelse(in_window, drug_effect, 1)

  counts <- rpois(length(piece_days), rate_per_year * piece_days / 365.25)
  piece <- rep(seq_along(counts), counts)
  piece_start[piece] + floor(runif(length(piece)) * piece_days[piece])
}

simulate_outcome <- function(concept_id, base_rate, ulcer_effect, age_effect, drug_effects) {
  events <- lapply(seq_len(nrow(patients)), function(i) {
    patient <- patients[i, ]
    drug_effect <- if (patient$celecoxib == 1) drug_effects[["celecoxib"]] else drug_effects[["diclofenac"]]
    dates <- simulate_events(patient, base_rate, ulcer_effect, age_effect, drug_effect)
    if (length(dates) == 0) return(NULL)
    data.frame(person_id = patient$person_id, condition_concept_id = concept_id, condition_start_date = dates)
  })
  do.call(rbind, events)
}

## ---- sim-gi-bleeds
gi_bleeds <- simulate_outcome(
  gi_bleed_concept_id,
  base_rate = 0.1,
  ulcer_effect = 3,
  age_effect = 1.3,
  drug_effects = true_effects
)
## ---- sim-negative-controls
# Negative controls share the confounders but are unaffected by either drug
negative_controls <- do.call(rbind, lapply(negative_control_concept_ids, function(concept_id) {
  simulate_outcome(
    concept_id,
    base_rate = 0.08,
    ulcer_effect = 2,
    age_effect = 1.2,
    drug_effects = c(celecoxib = 1, diclofenac = 1)
  )
}))

## ---- sim-write
replace_concepts <- c(gi_bleed_concept_id, negative_control_concept_ids)
replace_ids <- dbGetQuery(con, sprintf(
  "SELECT descendant_concept_id FROM concept_ancestor WHERE ancestor_concept_id IN (%s)",
  paste(replace_concepts, collapse = ", ")
))$descendant_concept_id
replace_list <- paste(unique(c(replace_concepts, replace_ids)), collapse = ", ")

dbExecute(con, sprintf("DELETE FROM condition_occurrence WHERE condition_concept_id IN (%s)", replace_list))
dbExecute(con, sprintf("DELETE FROM condition_era WHERE condition_concept_id IN (%s)", replace_list))

new_conditions <- rbind(gi_bleeds, negative_controls)
first_id <- dbGetQuery(con, "SELECT MAX(condition_occurrence_id) AS id FROM condition_occurrence")$id + 1
new_conditions$condition_occurrence_id <- first_id + seq_len(nrow(new_conditions)) - 1
new_conditions$condition_type_concept_id <- 32020
dbWriteTable(con, "new_conditions", new_conditions, temporary = TRUE)
dbExecute(con, "
  INSERT INTO condition_occurrence (condition_occurrence_id, person_id, condition_concept_id,
                                    condition_start_date, condition_type_concept_id)
  SELECT condition_occurrence_id, person_id, condition_concept_id,
         condition_start_date, condition_type_concept_id
  FROM new_conditions")

# Reassign the drug and shorten follow-up
treatment <- data.frame(
  person_id = patients$person_id,
  drug_concept_id = ifelse(patients$celecoxib == 1, celecoxib_concept_id, diclofenac_concept_id),
  obs_start = patients$obs_start,
  obs_end = patients$obs_end
)
dbWriteTable(con, "treatment", treatment, temporary = TRUE)
dbExecute(con, sprintf("
  UPDATE drug_exposure SET drug_concept_id = t.drug_concept_id
  FROM treatment t
  WHERE drug_exposure.person_id = t.person_id AND drug_exposure.drug_concept_id IN (%d, %d)",
  celecoxib_concept_id, diclofenac_concept_id))
dbExecute(con, sprintf("
  UPDATE drug_era SET drug_concept_id = t.drug_concept_id
  FROM treatment t
  WHERE drug_era.person_id = t.person_id AND drug_era.drug_concept_id IN (%d, %d)",
  celecoxib_concept_id, diclofenac_concept_id))
dbExecute(con, "
  UPDATE observation_period
  SET observation_period_start_date = t.obs_start, observation_period_end_date = t.obs_end
  FROM treatment t
  WHERE observation_period.person_id = t.person_id")

dbDisconnect(con, shutdown = TRUE)

saveRDS(
  list(patients = patients, gi_bleeds = gi_bleeds, true_rate_ratio = true_rate_ratio,
       true_effects = true_effects),
  file.path(study_folder, "simulation.rds")
)
