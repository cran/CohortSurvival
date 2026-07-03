## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, warning = FALSE, message = FALSE,
  out.width = "100%",
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CohortSurvival)
library(dplyr)

## -----------------------------------------------------------------------------
cdm <- CohortSurvival::mockMGUS2cdm()

## -----------------------------------------------------------------------------
cdm$mgus_diagnosis |>
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |>
  dplyr::glimpse()

## -----------------------------------------------------------------------------
cdm$death_cohort |>
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |>
  dplyr::glimpse()

## -----------------------------------------------------------------------------
cdm$progression |>
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |>
  dplyr::glimpse()

## ----eval = FALSE-------------------------------------------------------------
# cdm <- CohortConstructor::deathCohort(
#   cdm = cdm,
#   name = "death_cohort",
#   subsetCohort = "mgus_diagnosis"
# )

## -----------------------------------------------------------------------------
estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort",
  outcomeWashout = 365,
  followUpDays = 365
) |>
  tableSurvival()

## -----------------------------------------------------------------------------
estimateCompetingRiskSurvival(
  cdm = cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "progression",
  competingOutcomeCohortTable = "death_cohort",
  outcomeWashout = 365,
  competingOutcomeWashout = 0,
  followUpDays = 365
) |>
  tableSurvival()

## ----eval = FALSE-------------------------------------------------------------
# cdm$target <- cdm$target |>
#   PatientProfiles::addDemographics(
#     ageGroup = list(c(0, 64), c(65, 74), c(75, Inf)),
#     sex = TRUE,
#     name = "target"
#   )

## -----------------------------------------------------------------------------
cdm$mgus_diagnosis |>
  dplyr::select(subject_id, cohort_start_date, age, age_group, sex) |>
  dplyr::glimpse()

## -----------------------------------------------------------------------------
estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort",
  strata = list("sex", c("age_group", "sex")),
  restrictedMeanFollowUp = 365
) |>
  tableSurvival()

## ----eval = FALSE-------------------------------------------------------------
# cdm <- omopgenerics::bind(
#   cdm$progression,
#   cdm$death_cohort,
#   name = "outcome_cohorts"
# )
# 
# estimateSingleEventSurvival(
#   cdm = cdm,
#   targetCohortTable = "mgus_diagnosis",
#   outcomeCohortTable = "outcome_cohorts"
# )

## -----------------------------------------------------------------------------
cdmDisconnect(cdm)

