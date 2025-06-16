## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, warning = FALSE, message = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CohortSurvival)
library(dplyr)
library(cmprsk)
library(survival)

## -----------------------------------------------------------------------------
cdm <- CohortSurvival::mockMGUS2cdm()

## ----fig.width=5--------------------------------------------------------------
input_survival_single <- cdm$mgus_diagnosis %>%
       addCohortSurvival(
       cdm = cdm,
       outcomeCohortTable = "death_cohort",
       outcomeCohortId = 1
       ) 

input_survival_single %>% 
  glimpse()

## -----------------------------------------------------------------------------
cdm$mgus_diagnosis %>%
       addCohortSurvival(
       cdm = cdm,
       outcomeCohortTable = "death_cohort",
       outcomeWashout = 180,
       followUpDays = 365
       ) %>%
  filter(cohort_start_date > "1993-01-01") %>%
  glimpse()
cdm$mgus_diagnosis %>%
       addCohortSurvival(
       cdm = cdm,
       outcomeCohortTable = "death_cohort",
       outcomeDateVariable = "cohort_end_date",
       censorOnDate = as.Date("1994-01-01")
       ) %>%
    filter(cohort_start_date > "1993-01-01") %>%
  glimpse()

## -----------------------------------------------------------------------------
survival::coxph(survival::Surv(time, status) ~ age + sex, data = input_survival_single)
survival::survdiff(survival::Surv(time, status) ~ sex, data = input_survival_single)

## -----------------------------------------------------------------------------

# Add all status and time information for both outcomes
  input_survival_cr <- cdm$mgus_diagnosis %>%
    addCohortSurvival(cdm, "progression") %>%
    dplyr::rename(
      "outcome_time" = "time",
      "outcome_status" = "status"
    ) %>%
     addCohortSurvival(cdm, "death_cohort") %>%
    dplyr::rename(
      "competing_outcome_time" = "time",
      "competing_outcome_status" = "status"
    )
  
  # Collect and 
  input_survival_cr <- input_survival_cr %>%
    dplyr::collect() %>%
    dplyr::mutate(
      time = pmin(outcome_time, competing_outcome_time),
      status = factor(
        dplyr::if_else(competing_outcome_time <= outcome_time, 2 * competing_outcome_status, outcome_status))
    ) %>%
    dplyr::select(-c("outcome_time", "outcome_status", "competing_outcome_time", "competing_outcome_status"))


## ----fig.height=6, fig.width=8------------------------------------------------
input_survival_cr <- input_survival_cr %>%
  dplyr::mutate(sex = dplyr::if_else(sex == "M", 0, 1))

covs <- data.frame(input_survival_cr$age, input_survival_cr$sex)
names(covs) <- c("age", "sex")

summary(cmprsk::crr(ftime = input_survival_cr$time,
            fstatus = input_survival_cr$status,
            cov1 = covs,
            failcode = 1,
            cencode = 0))

## -----------------------------------------------------------------------------
cdmDisconnect(cdm)

