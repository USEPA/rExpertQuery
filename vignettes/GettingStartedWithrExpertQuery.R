## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(dplyr)
library(DT)

## ----install, eval = TRUE, echo = TRUE, results = 'hide', message = FALSE, warning = FALSE----
if (!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}

remotes::install_github("USEPA/rExpertQuery", ref = "develop", dependencies = TRUE, force = TRUE)

library(dplyr)
library(data.table)

## ----testkey, include=FALSE---------------------------------------------------
testkey <- "53BVce47MQ3KXKibjx35g4ojaDQGh8qWfbdO8cE0"

## ----eq.actions.r4, echo = TRUE, results = FALSE, message = FALSE, warning = FALSE----
# query Actions from Region 4 that are included in Measures
R4_actions_in_meas <- rExpertQuery::EQ_Actions(api_key = testkey, region = 4, in_meas = "Yes")

## ----r4.url.prep, include = FALSE---------------------------------------------
# Actions Documents containing "nutrient"
R4_actions_in_meas$planSummaryLink <- paste0('<a href="', R4_actions_in_meas$planSummaryLink, '" target="_blank">', R4_actions_in_meas$planSummaryLink, "</a>")

## ----r4.datatable-------------------------------------------------------------
# create random subset of R4 query results
R4_subset <- R4_actions_in_meas %>%
  dplyr::slice_sample(n = 20)

# create data tab;e
DT::datatable(R4_subset, options = list(pageLength = 2, scrollX = TRUE))

## ----eq.actions.ex2, results = TRUE, message = FALSE, warning = FALSE---------
# query Actions for Missouri Actions with action agency "EPA" and parameter group "PATHOGENS"
MO_epa <- rExpertQuery::EQ_Actions(
  api_key = testkey, statecode = "MO",
  act_agency = "EPA",
  param_group = "PATHOGENS",
  comp_date_start = "2000-01-01",
  comp_date_end = "2020-12-31"
)

## ----mo.url.prep, include = FALSE---------------------------------------------
# Actions Documents containing "nutrient"
MO_epa$planSummaryLink <- paste0('<a href="', MO_epa$planSummaryLink, '" target="_blank">', MO_epa$planSummaryLink, "</a>")

## ----mo.data.table------------------------------------------------------------
# view MO results
DT::datatable(MO_epa, options = list(pageLength = 10, scrollX = TRUE))

## ----nutrients, results = TRUE, message = FALSE, warning = FALSE--------------
# Actions Documents containing "nutrient"
Nutrient_docs <- rExpertQuery::EQ_ActionsDocuments(
  doc_query = "nutrient",
  api_key = testkey
)

## ----nutrients.url.prep, include = FALSE--------------------------------------
# Actions Documents containing "nutrient"
Nutrient_docs$actionDocumentUrl <- paste0('<a href="', Nutrient_docs$actionDocumentUrl, '" target="_blank">', Nutrient_docs$actionDocumentUrl, "</a>")

## ----nutrients.data.table-----------------------------------------------------
Nutrient_docs_subset <- Nutrient_docs %>%
  dplyr::slice_sample(n = 20)

DT::datatable(Nutrient_docs_subset, options = list(pageLength = 5, scrollX = TRUE), escape = FALSE)

## ----pa.4b.ex, results = TRUE, message = FALSE, warning = FALSE---------------
PA_4B <- rExpertQuery::EQ_ActionsDocuments(
  statecode = "PA", act_type = "4B Restoration Approach",
  api = testkey
)

## ----pa.url.prep, include = FALSE---------------------------------------------
# Actions Documents containing "nutrient"
PA_4B$actionDocumentUrl <- paste0('<a href="', PA_4B$actionDocumentUrl, '" target="_blank">', PA_4B$actionDocumentUrl, "</a>")

## ----pa.4b.table--------------------------------------------------------------
PA_4B_subset <- PA_4B %>%
  dplyr::slice_sample(n = 20)

DT::datatable(PA_4B_subset, options = list(pageLength = 5, scrollX = TRUE), escape = FALSE)

## ----ky.assess, results = TRUE, message = FALSE, warning = FALSE--------------
KY_assessments <- rExpertQuery::EQ_Assessments(
  statecode = "KY",
  api_key = testkey
)

## ----R3.ecological, results = TRUE, message = FALSE, warning = FALSE----------
R3_ecological <- rExpertQuery::EQ_Assessments(
  region = 3,
  use_group = "ECOLOGICAL_USE",
  param_status = "Cause",
  api_key = testkey
)

## ----r3.ecological.table------------------------------------------------------
R3_ecological_subset <- R3_ecological %>%
  dplyr::slice_sample(n = 20)

DT::datatable(R3_ecological_subset, options = list(pageLength = 5, scrollX = TRUE), escape = FALSE)

## ----ri.aus, results = TRUE, message = FALSE, warning = FALSE-----------------

RI_aus <- rExpertQuery::EQ_AssessmentUnits(statecode = "RI",
                                           api_key = testkey)


## ----fl.retired, results = TRUE, message = FALSE, warning = FALSE-------------

FL_retired <- rExpertQuery::EQ_AssessmentUnits(statecode = "FL",
                                           au_status = "Retired",
                                           api_key = testkey)


## ----akausmls, results = TRUE, message = FALSE, warning = FALSE---------------
AK_ausmls <- rExpertQuery::EQ_AssessmentUnits(statecode = "AK",
                                              api_key = testkey)

## ----waausmls, results = TRUE, message = FALSE, warning = FALSE---------------
WA_ausmls <- rExpertQuery::EQ_AssessmentUnits(statecode = "WA",
                                              api_key = testkey)

## ----dc.catchcorr, results = TRUE, message = FALSE, warning = FALSE-----------

DC_catch <- rExpertQuery::EQ_CatchCorr(statecode = "DC",
                                       api_key = testkey)


## ----il.au.catch, results = TRUE, message = FALSE, warning = FALSE------------

IL_N99_catch <- rExpertQuery::EQ_CatchCorr(statecode = "IL",
                          auid = "IL_N-99",
                          api_key = testkey)


## ----nat.tmdls, results = TRUE, message = FALSE, warning = FALSE--------------

Nat_tmdls <- rExpertQuery::EQ_NationalExtract("tmdl")


## ----nat.actions, results = TRUE, message = FALSE, warning = FALSE------------

Nat_actions <- rExpertQuery::EQ_NationalExtract("actions")


## ----wi.source.hab, results = TRUE, message = FALSE, warning = FALSE----------
WI_habalt_sources <- rExpertQuery::EQ_Sources(statecode = "WI",
                                 param_group = "HABITAT ALTERATIONS",
                                 api_key = testkey)

## ----source.legacy, results = TRUE, message = FALSE, warning = FALSE----------
legacy_sources <- rExpertQuery::EQ_Sources(source = "LEGACY/HISTORICAL POLLUTANTS",
                             api_key = testkey)

## ----hi.tmdl.both, results = TRUE, message = FALSE, warning = FALSE-----------

HI_both_tmdls <- rExpertQuery::EQ_TMDLs(statecode = "HI",
                               source_type = "Both",
                               api_key = testkey)

