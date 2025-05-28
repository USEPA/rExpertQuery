# Training data

# import natioanl assessments profile
assessments.nat <- rExpertQuery::EQ_NationalExtract("assessments")

# filter for latest assessments
assessments.nat <- assessments.nat %>%
  dplyr::group_by(organizationId) %>%
  dplyr::slice_max(reportingCycle) %>%
  dplyr::select(-objectId) %>%
  dplyr::distinct() %>%
  dplyr::ungroup()

# import national extract for assessment units/monitoring locations
nat.ausmls <- rExpertQuery::EQ_NationalExtract("au_mls")

# set up query to search assessments for KS, primary contact recreation use, and stream, default behavior is to query latest report cycle
ks.imp.streams <- rExpertQuery::EQ_Assessments(statecode = "KS",
                                               use_name = "Primary Contact Recreation",
                                               api_key = testkey,
                                               water_type = "STREAM")

# query MT's latest assessments for zinc as a cause
MT.impair <- rExpertQuery::EQ_Assessments(statecode = "MT",
                                          api_key = testkey,
                                          param_name = "ZINC",
                                          param_status = "Cause"
)

# get cat 5 waters from 2024 assessment cycle
IL.latest <- rExpertQuery::EQ_Assessments(statecode = "IL",
                                          epa_ir_cat = "5",
                                          api_key = testkey)

# get all assessments from previous cycle
IL.previous <- rExpertQuery::EQ_Assessments(statecode = "IL",
                                            report_cycle = 2022,
                                            api_key = testkey)

# search for retired assessment units in Florida
FL.ret.auids <- rExpertQuery::EQ_AssessmentUnits(statecode = "FL",
                                                 au_status = "Retired",
                                                 api_key = testkey)

# query for Hawaii TMDLs
HI.tmdls <- rExpertQuery::EQ_TMDLs(statecode = "HI",
                                   api_key = testkey)

# r10 query for FY 2025 TMDLs
tmdl.proj <- rExpertQuery::EQ_TMDLs(region = 10,
                                    tmdl_date_start = "2024-10-01",
                                    api_key = testkey)

# query Actions from Delaware that are included in Measures
DE.meas <- rExpertQuery::EQ_Actions(statecode = "DE",
                                    in_meas = "Yes",
                                    api_key = testkey)

# query for R3 FY2024 actions
act.fy.24 <- rExpertQuery::EQ_Actions(region = 3,
                                      fisc_year_start = 2024,
                                      fisc_year_end = 2024,
                                      api_key = testkey)


# Actions Documents containing "nutrient"
nutrient.doc <- rExpertQuery::EQ_ActionsDocuments(doc_query = "nutrient",
                                                  api_key = testkey )

# query for assessment units and monitoring locations from Alaska
ak.mls <- rExpertQuery::EQ_AUsMLs(statecode = "AK",
                                  api_key = testkey)

# query for sources in the latest assessment cycle in R6
r6.sources <- rExpertQuery::EQ_Sources(region = 6,
                                       api_key = testkey)


# query for IL_N-99 catchment data
il.n99.catch <- rExpertQuery::EQ_CatchCorr(statecode = "IL",
                                           auid = "IL_N-99",
                                           api_key = testkey)

save(assessments.nat, nat.ausmls, ks.imp.streams, IL.latest, IL.previous, FL.ret.auids,
     HI.tmdls, tmdl.proj, DE.meas, act.fy.24, nutrient.doc, ak.mls, r6.sources, il.n99.catch,
     file = "DemoData.Rds")
