# Copyright 2023 DARWIN EU®
#
# This file is part of CohortSurvival
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

deathDiagnostics <- function(cdm,
                             cohortTable = NULL,
                             cohortId = 1){

  # 0. validate inputs...
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertTable(cdm[["death"]])
  omopgenerics::assertCharacter(cohortTable, length = 1, null = TRUE)
  if(!is.null(cohortTable)) {
    omopgenerics::validateCohortArgument(cdm[[cohortTable]])
    omopgenerics::validateCohortIdArgument(cohortId, cdm[[cohortTable]])
  }

  # 1. start diagnosis whole table
  x <-  cdm$death %>%
    PatientProfiles::addInObservation(indexDate = "death_date") %>%
    PatientProfiles::addFutureObservation(indexDate = "death_date") %>%
    PatientProfiles::addPriorObservation(indexDate = "death_date") %>%
    dplyr::compute()

  output <- omopgenerics::emptySummarisedResult()

  output <- output %>%
    tibble::add_row(
      variable_name = "Number subjects",
      estimate_name = "count",
      estimate_type = "integer",
      estimate_value = as.character(x %>%
                                      dplyr::tally() %>%
                                      dplyr::pull())
    )

  output <- output %>%
    tibble::add_row(
      variable_name = "Not in observation",
      estimate_name = "count",
      estimate_type = "integer",
      estimate_value = as.character(x %>%
                                      dplyr::filter(.data$in_observation == 0) %>%
                                      dplyr::tally() %>%
                                      dplyr::pull())
    )

  output <- output %>%
    tibble::add_row(
      variable_name = c("Future observation",
                        "Future observation",
                        "Future observation",
                        "Prior observation",
                        "Prior observation",
                        "Prior observation"),
      estimate_name = c("min",
                        "max",
                        "mean",
                        "min",
                        "max",
                        "mean"),
      estimate_type = c("numeric",
                        "numeric",
                        "numeric",
                        "numeric",
                        "numeric",
                        "numeric"),
      estimate_value =  x %>%
         dplyr::summarise(
           min_future_obs = min(.data$future_observation, na.rm = TRUE),
           max_future_obs = max(.data$future_observation, na.rm = TRUE),
           mean_future_obs = mean(.data$future_observation, na.rm = TRUE),
           min_prior_obs = min(.data$prior_observation, na.rm = TRUE),
           max_prior_obs = max(.data$prior_observation, na.rm = TRUE),
           mean_prior_obs = mean(.data$prior_observation, na.rm = TRUE)
          ) %>%
           tidyr::pivot_longer(cols = dplyr::everything(),
                               names_to = "estimate_name",
                               values_to = "estimate_value") %>%
        dplyr::pull() %>%
        as.character()
    )

  output <- output %>%
    dplyr::mutate(
      cdm_name = attr(cdm, "cdm_name"),
      result_id = 1,
      result_type = "death_diagnostics",
      package_name = "CohortSurvival",
      package_version = as.character(utils::packageVersion("CohortSurvival")),
      group_name = "cohort_name",
      group_level = "whole_table",
      strata_name = "overall",
      strata_level = "overall",
      additional_name = "overall",
      additional_level = "overall"
    )

  # 2. perform diagnosis for cohorts if required
  if(!is.null(cohortTable)) {
    for(id in cohortId) {
      x_w <-  x %>%
        dplyr::inner_join(cdm[[cohortTable]] %>%
                            dplyr::filter(.data$cohort_definition_id == id) %>%
                            dplyr::select("subject_id"),
                          by = c("person_id" = "subject_id"))

      output_w <- omopgenerics::emptySummarisedResult()

      output_w <- output_w %>%
        tibble::add_row(
          variable_name = "Number subjects",
          estimate_name = "count",
          estimate_type = "integer",
          estimate_value = as.character(x_w %>%
                                          dplyr::tally() %>%
                                          dplyr::pull())
        )

      output_w <- output_w %>%
        tibble::add_row(
          variable_name = "Not in observation",
          estimate_name = "count",
          estimate_type = "integer",
          estimate_value = as.character(x_w %>%
                                          dplyr::filter(.data$in_observation == 0) %>%
                                          dplyr::tally() %>%
                                          dplyr::pull())
        )

      output_w <- output_w %>%
        tibble::add_row(
          variable_name = c("Future observation",
                            "Future observation",
                            "Future observation",
                            "Prior observation",
                            "Prior observation",
                            "Prior observation"),
          estimate_name = c("min",
                            "max",
                            "mean",
                            "min",
                            "max",
                            "mean"),
          estimate_type = c("numeric",
                            "numeric",
                            "numeric",
                            "numeric",
                            "numeric",
                            "numeric"),
          estimate_value =  x_w %>%
            dplyr::summarise(
              min_future_obs = min(.data$future_observation, na.rm = TRUE),
              max_future_obs = max(.data$future_observation, na.rm = TRUE),
              mean_future_obs = mean(.data$future_observation, na.rm = TRUE),
              min_prior_obs = min(.data$prior_observation, na.rm = TRUE),
              max_prior_obs = max(.data$prior_observation, na.rm = TRUE),
              mean_prior_obs = mean(.data$prior_observation, na.rm = TRUE)
            ) %>%
            tidyr::pivot_longer(cols = dplyr::everything(),
                                names_to = "estimate_name",
                                values_to = "estimate_value") %>%
            dplyr::pull() %>%
            as.character()
        )

      output_w <- output_w %>%
        dplyr::mutate(
          cdm_name = attr(cdm, "cdm_name"),
          result_id = 1,
          result_type = "death_diagnostics",
          package_name = "CohortSurvival",
          package_version = as.character(utils::packageVersion("CohortSurvival")),
          group_name = "cohort_name",
          group_level = attr(cdm[[cohortTable]], "cohort_set") %>%
            dplyr::filter(.data$cohort_definition_id == id) %>%
            dplyr::pull("cohort_name"),
          strata_name = "overall",
          strata_level = "overall",
          additional_name = "overall",
          additional_level = "overall"
        )
      output <- dplyr::union_all(output,
                                 output_w)
    }
  }

  expected_zero <- output %>%
    dplyr::filter(!grepl("Prior",.data$variable_name)) %>%
    dplyr::filter(.data$estimate_value != "0")

  if(nrow(expected_zero) != 0) {
    ez <- expected_zero %>%
      dplyr::select("group_level", "variable_name", "estimate_name", "estimate_value") %>%
      dplyr::collect()
    for(i in 1:(nrow(ez))) {
      cli::cli_alert_warning(paste0(ez[i,3]," of ",ez[i,2]," for group ",ez[i,1]," should be 0 but is ",ez[i,4]))
    }
  }

  settings <- output %>%
    dplyr::select("result_id", "result_type", "package_name", "package_version") %>%
    dplyr::distinct()

  output <- output %>%
    dplyr::select(-c("result_type", "package_name", "package_version"))

  output <- omopgenerics::newSummarisedResult(output, settings = settings)

  return(output)
}
