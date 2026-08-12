library(rExpertQuery)

# source helper function
source("tests/testthat/helper-htt2.R")

# get api key once
eq_key <- Sys.getenv("EQ_API_KEY")

# testthat recordings for functions using EQ web services
record_without_capture({
  EQ_ActionsDocuments(
    state = "OR",
    comp_date_start = "01-01-2018",
    comp_date_end = "12-31-2020",
    api_key = eq_key
  )
})

record_to_tests("RIact", {
  EQ_Actions(
    statecode = "RI",
    fisc_year_start = 2014,
    fisc_year_end = 2020,
    api_key = eq_key
  )
})

record_to_tests("ILcat5", {
  EQ_Assessments(
    statecode = "IL",
    epa_ir_cat = 5,
    param_group = "ALGAL GROWTH",
    api_key = eq_key
  )
})

record_to_tests("MOau", {
  EQ_AssessmentUnits(
    statecode = "MO",
    au_name = "Leisure Lake",
    api_key = eq_key
  )
})

record_to_tests("MTml", {
  EQ_AUsMLs(
    org_id = "MTDEQ",
    au_name = "Kleinschmidt Creek",
    api_key = eq_key
  )
})

record_to_tests("ALcc", {
  EQ_CatchCorr(
    auid = "AL03150202-0404-110",
    api_key = eq_key
  )
})

record_to_tests("NATassess", {
  EQ_Assessments(api_key = eq_key)
})

record_to_tests("TXsrc", {
  EQ_Sources(
    report_cycle = 2018,
    statecode = "TX",
    source = "AGRICULTURE",
    api_key = eq_key
  )
})

record_to_tests("FLtmdl", {
  EQ_TMDLs(
    fisc_year_start = 2018,
    fisc_year_end = 2020,
    statecode = "FL",
    api_key = eq_key
  )
})

record_to_tests("NULLdv", {
  EQ_DomainValues(api_key = eq_key)
})

record_to_tests("assessTypesdv", {
  EQ_DomainValues(domain = "assess_types", api_key =eq_key)
})


# record mocks from functions which create csv files
actions.df <- EQ_NationalExtract("actions")

actions.df <- actions.df |>
  dplyr::slice_sample(n = 10)

dir.create("tests/testthat/htt2/NATact", recursive = TRUE, showWarnings = FALSE)
saveRDS(actions.df, "tests/testthat/htt2/NATact/actions.rds")

sources.df <- EQ_NationalExtract("sources")

sources.df <- sources.df |>
  dplyr::slice_sample(n = 10)

dir.create("tests/testthat/htt2/NATsource", recursive = TRUE, showWarnings = FALSE)
saveRDS(sources.df, "tests/testthat/htt2/NATsource/sources.rds")

rm(actions.df, sources.df)

message("All fixtures recorded under: ", normalizePath("tests/testthat/htt2", winslash = "/"))
