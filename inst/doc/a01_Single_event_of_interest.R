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
library(ggplot2)

## -----------------------------------------------------------------------------
cdm <- CohortSurvival::mockMGUS2cdm()

## -----------------------------------------------------------------------------
cdm$mgus_diagnosis %>% 
  glimpse()

cdm$death_cohort %>% 
  glimpse()

## -----------------------------------------------------------------------------
MGUS_death <- estimateSingleEventSurvival(
  cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort"
)
MGUS_death %>% 
  glimpse()
class(MGUS_death)

## -----------------------------------------------------------------------------
settings(MGUS_death)

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death)

## ----fig.width=9,fig.height=9-------------------------------------------------
plotSurvival(MGUS_death, riskTable = TRUE)

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death, ribbon = FALSE)
plotSurvival(MGUS_death, cumulativeFailure = TRUE)

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death) + theme_bw() + ggtitle("Plot survival") + coord_flip()

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death, timeScale = "years")
plotSurvival(MGUS_death, timeScale = "months")

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death)

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death, times = c(30,90,180))

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death, times = c(30,90,180, 500))

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death, times = c(1,2), timeScale = "years")

## -----------------------------------------------------------------------------
optionsTableSurvival()

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death, .options = list(title = "Survival summary",
                                          decimalMark = ",",
                                          bigMark = "."))

## -----------------------------------------------------------------------------
riskTable(MGUS_death)

## -----------------------------------------------------------------------------
# Transforming the output to a survival result format
MGUS_death_survresult <- MGUS_death %>% 
  asSurvivalResult() 
MGUS_death_survresult %>%
  glimpse()
# Events, attrition and summary are now attributes of the result object
attr(MGUS_death_survresult,"events") %>%
  glimpse()
attr(MGUS_death_survresult,"summary") %>%
  glimpse()
attr(MGUS_death_survresult,"attrition") %>%
  glimpse()

## -----------------------------------------------------------------------------
MGUS_death_survresult %>%
  filter(time %in% c(10:15))

## ----fig.width=13,fig.height=13-----------------------------------------------
MGUS_death_gap7 <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort", eventGap = 7)
plotSurvival(MGUS_death_gap7, riskTable = TRUE, riskInterval = 14)

## -----------------------------------------------------------------------------
MGUS_death_fu <- estimateSingleEventSurvival(
  cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort",
  followUpDays = 100
)

MGUS_death_all <- omopgenerics::bind(
  MGUS_death,
  MGUS_death_fu
)

MGUS_death_all %>%
  asSurvivalResult() %>%
  glimpse()

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death_all, facet = "follow_up_days")

tableSurvival(MGUS_death_all)

## -----------------------------------------------------------------------------
MGUS_death_strata <- estimateSingleEventSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort",
  strata = list(c("age_group"),
                c("sex"),
                c("age_group", "sex"))
) 

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death_strata)

## ----fig.height=6, fig.width=8------------------------------------------------
plotSurvival(MGUS_death_strata,
             facet = "sex",
             colour = "age_group")

## ----fig.width=17,fig.height=18-----------------------------------------------
plotSurvival(MGUS_death_strata,
             colour = c("age_group", "sex"),
             riskTable = TRUE)

## ----fig.width=17,fig.height=18-----------------------------------------------
plotSurvival(MGUS_death_strata, facet = c("age_group", "sex"))

## ----fig.width=17,fig.height=18-----------------------------------------------
plotSurvival(MGUS_death_strata) +
  facet_grid(rows = vars(sex), cols = vars(age_group))
  

## -----------------------------------------------------------------------------
MM_death <- estimateSingleEventSurvival(cdm, "progression", "death_cohort")
MGUS_MM_death <- bind(MGUS_death, MM_death)

## -----------------------------------------------------------------------------
settings(MGUS_MM_death)
tableSurvival(MGUS_MM_death)
plotSurvival(MGUS_MM_death, colour = "target_cohort")

## -----------------------------------------------------------------------------
cdm <- bind(cdm$progression, cdm$death_cohort, name = "outcome_cohorts")

## -----------------------------------------------------------------------------
MGUS_death_prog <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "outcome_cohorts")
tableSurvival(MGUS_death_prog)
plotSurvival(MGUS_death_prog, colour = "outcome")

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death_prog, colour = "outcome", logLog = TRUE)

## -----------------------------------------------------------------------------
x <- tempdir()
exportSummarisedResult(MGUS_death, path = x, fileName = "result.csv")

## -----------------------------------------------------------------------------
MGUS_death_imported <- importSummarisedResult(path = file.path(x, "result.csv"))

## ----fig.width=9,fig.height=9-------------------------------------------------
plotSurvival(MGUS_death_imported, riskTable = TRUE)

## -----------------------------------------------------------------------------
cdmDisconnect(cdm)

