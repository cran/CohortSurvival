% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addCohortSurvival.R
\name{addCompetingRiskCohortSurvival}
\alias{addCompetingRiskCohortSurvival}
\title{Add competing risk survival information to a cohort table}
\usage{
addCompetingRiskCohortSurvival(
  x,
  cdm,
  outcomeCohortTable,
  outcomeCohortId = 1,
  outcomeDateVariable = "cohort_start_date",
  outcomeWashout = Inf,
  outcomeCensorOnCohortExit = FALSE,
  outcomeCensorOnDate = NULL,
  outcomeFollowUpDays = Inf,
  competingOutcomeCohortTable,
  competingOutcomeCohortId = 1,
  competingOutcomeDateVariable = "cohort_start_date",
  competingOutcomeWashout = Inf,
  competingOutcomeCensorOnCohortExit = FALSE,
  competingOutcomeCensorOnDate = NULL,
  competingOutcomeFollowUpDays = Inf
)
}
\arguments{
\item{x}{cohort table to add survival information}

\item{cdm}{CDM reference}

\item{outcomeCohortTable}{The outcome cohort table of interest.}

\item{outcomeCohortId}{ID of event cohorts to include. Only one outcome
(and so one ID) can be considered.}

\item{outcomeDateVariable}{Variable containing date of outcome event}

\item{outcomeWashout}{Washout time in days for the outcome}

\item{outcomeCensorOnCohortExit}{If TRUE, an individual's follow up will be
censored at their cohort exit}

\item{outcomeCensorOnDate}{if not NULL, an individual's follow up will be censored
at the given date}

\item{outcomeFollowUpDays}{Number of days to follow up individuals (lower bound 1,
upper bound Inf)}

\item{competingOutcomeCohortTable}{The outcome cohort table of interest.}

\item{competingOutcomeCohortId}{ID of event cohorts to include. Only one outcome
(and so one ID) can be considered.}

\item{competingOutcomeDateVariable}{Variable containing date of competing outcome event}

\item{competingOutcomeWashout}{Washout time in days for the competing outcome}

\item{competingOutcomeCensorOnCohortExit}{If TRUE, an individual's follow up will be
censored at their cohort exit}

\item{competingOutcomeCensorOnDate}{if not NULL, an individual's follow up will be censored
at the given date}

\item{competingOutcomeFollowUpDays}{Number of days to follow up individuals (lower bound 1,
upper bound Inf)}
}
\value{
Two additional columns will be added to x. The "time" column will
contain number of days to censoring. The "status" column will indicate
whether the patient had the outcome event (value: 1), competing event (value:2)
or did not have the event/is censored (value: 0)
}
\description{
Add competing risk survival information to a cohort table
}
\examples{
\donttest{

cdm <- mockMGUS2cdm()
crsurvivaldata <- cdm$mgus_diagnosis \%>\%
  addCompetingRiskCohortSurvival(
    cdm = cdm,
    outcomeCohortTable = "progression",
    outcomeCohortId = 1,
    competingOutcomeCohortTable = "death_cohort",
    competingOutcomeCohortId = 1
  )
  }

}
