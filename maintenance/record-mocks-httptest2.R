# testthat recordings
record_to_tests("ORad", {
  EQ_ActionsDocuments(
    state = "OR",
    comp_date_start = "01-01-2018",
    comp_date_end = "12-31-2020",
    api_key = .setEQKey()
  )
})

record_to_tests("RIact", {
  EQ_Actions(
    statecode = "RI",
    fisc_year_start = 2014,
    fisc_year_end = 2020,
    api_key = .setEQKey()
  )
})

record_to_tests("ILcat5", {
  EQ_Assessments(
    statecode = "IL",
    epa_ir_cat = 5,
    param_group = "ALGAL GROWTH",
    api_key = .setEQKey()
  )
})

record_to_tests("MOau", {
  EQ_AssessmentUnits(
    statecode = "MO",
    au_name = "Leisure Lake",
    api_key = .setEQKey()
  )
})

record_to_tests("MTml", {
  EQ_AUsMLs(
    org_id = "MTDEQ",
    au_name = "Kleinschmidt Creek",
    api_key = .setEQKey()
  )
})

record_to_tests("ALcc", {
  EQ_CatchCorr(
    auid = "AL03150202-0404-110",
    api_key = .setEQKey()
  )
})

record_to_tests("NATact", {
  EQ_NationalExtract("actions", limit = 10)
})

record_to_tests("NATtmdl", {
  EQ_NationalExtract("tmdl", limit = 10)
})

record_to_tests("NATassess", {
  EQ_Assessments(api_key = .setEQKey())
})

record_to_tests("TXsrc", {
  EQ_Sources(
    report_cycle = 2018,
    statecode = "TX",
    source = "AGRICULTURE",
    api_key = .setEQKey()
  )
})

record_to_tests("IAtmdl", {
  EQ_Sources(
    report_cycle = 2018,
    statecode = "IA",
    source = "AGRICULTURE",
    api_key = .setEQKey()
  )
})

record_to_tests("NULLdv", {
  EQ_DomainValues(api_key = .setEQKey())
})

record_to_tests("assessTypesdv", {
  EQ_DomainValues(domain = "assess_types", api_key = .setEQKey())
})


message("All fixtures recorded under: ", normalizePath(fixtures_root, winslash = "/"))
