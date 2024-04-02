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
  asSurvivalResult() %>% 
  glimpse()

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death_prog, cumulativeFailure = TRUE,
             colour = "variable_level")

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death_prog, times = c(100,200,300,400)) 

## -----------------------------------------------------------------------------
MGUS_death_prog <-  estimateCompetingRiskSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "progression",
  competingOutcomeCohortTable = "death_cohort",
  strata = list(c("sex"))
)


## ----fig.height=6, fig.width=8------------------------------------------------
plotSurvival(MGUS_death_prog %>% 
               dplyr::filter(strata_name != "Overall"), 
             facet = "strata_level",
             colour = "variable_level",
             cumulativeFailure = TRUE)

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death_prog, times = c(50,150,365))

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

