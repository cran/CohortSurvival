## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, warning = FALSE, message = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CohortSurvival)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
cdm <- CohortSurvival::mockMGUS2cdm()

## -----------------------------------------------------------------------------
cdm$mgus_diagnosis %>% 
  glimpse()

cdm$death_cohort %>% 
  glimpse()

## -----------------------------------------------------------------------------
MGUS_death <- estimateSingleEventSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort"
)
MGUS_death %>% 
  glimpse()

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death)

## -----------------------------------------------------------------------------
survivalSummary(MGUS_death) %>% 
  tidyr::pivot_wider(names_from = "variable_type", values_from = "estimate") %>%
  dplyr::mutate(
    "Median survival (95% CI)" = paste0(median_survival, 
                                        " (", median_survival_95CI_lower, 
                                        " to ", median_survival_95CI_higher, ")")
  ) %>%
  dplyr::select("Median survival (95% CI)")

## -----------------------------------------------------------------------------
MGUS_death <- estimateSingleEventSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort",
  strata = list(c("age_group"),
                c("sex"),
                c("age_group", "sex"))
)

## ----fig.height=6, fig.width=8------------------------------------------------
plotSurvival(MGUS_death,
             facet = "strata_name",
             colour = "strata_level")

## -----------------------------------------------------------------------------
survivalSummary(MGUS_death) %>%
  tidyr::pivot_wider(names_from = "variable_type", values_from = "estimate") %>%
  dplyr::mutate("Median survival (95% CI)" = paste0(median_survival, " (", median_survival_95CI_lower, " to ", median_survival_95CI_higher, ")")
                ) %>% 
  dplyr::select(strata_name, strata_level,"Median survival (95% CI)")

## -----------------------------------------------------------------------------
MGUS_death <- estimateSingleEventSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort",
  returnParticipants = TRUE
)
survivalParticipants(MGUS_death)

## -----------------------------------------------------------------------------
cdm_disconnect(cdm)

