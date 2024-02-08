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

cdm$progression %>%
  glimpse()

## ----fig.width=5--------------------------------------------------------------
MGUS_death_prog <- estimateCompetingRiskSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "progression",
  competingOutcomeCohortTable = "death_cohort"
)

MGUS_death_prog %>% 
  glimpse()

## -----------------------------------------------------------------------------
plotCumulativeIncidence(MGUS_death_prog, 
                        colour = "variable_level")

## -----------------------------------------------------------------------------
MGUS_death_prog %>% dplyr::filter(estimate_type == "Survival summary") %>%
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

## -----------------------------------------------------------------------------
MGUS_death_prog <-  estimateCompetingRiskSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "progression",
  competingOutcomeCohortTable = "death_cohort",
  strata = list(c("sex"))
)


## ----fig.height=6, fig.width=8------------------------------------------------

plotCumulativeIncidence(MGUS_death_prog  %>%
                          dplyr::filter(strata_name != "Overall"), 
                        facet = "strata_level",
                        colour = "variable_level")

## -----------------------------------------------------------------------------
MGUS_death_prog %>% dplyr::filter(estimate_type == "Survival summary") %>%
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

## -----------------------------------------------------------------------------
MGUS_death_prog <- estimateCompetingRiskSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "progression",
  competingOutcomeCohortTable = "death_cohort",
  returnParticipants = TRUE
)
survivalParticipants(MGUS_death_prog)

## -----------------------------------------------------------------------------
cdm_disconnect(cdm)

