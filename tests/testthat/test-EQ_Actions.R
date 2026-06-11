testthat::test_that("EQ_Actions returns expected number of row", {
  with_test_mocks("RIact", {
    expect_equal(NROW(EQ_Actions(
      statecode = "RI",
      fisc_year_start = 2014,
      fisc_year_end = 2020,
      api_key = .setEQKey()
    )), 77)
  })
})

testthat::test_that("EQ_Actions returns expected number of columns", {
  with_test_mocks("RIact", {
  expect_equal(NCOL(EQ_Actions(
    statecode = "RI",
    fisc_year_start = 2014,
    fisc_year_end = 2020,
    api_key = .setEQKey()
  )), 23)
})
})

testthat::test_that("EQ_Actions returns expected column names", {
  with_test_mocks("RIact", {
  expected <- c(
    "objectId", "region", "state", "organizationType",
    "organizationId", "organizationName", "waterType",
    "parameterGroup", "parameter", "actionType", "actionId",
    "actionName", "actionAgency", "inIndianCountry",
    "includeInMeasure", "completionDate", "assessmentUnitId",
    "assessmentUnitName", "fiscalYearEstablished",
    "locationDescription", "waterSize", "waterSizeUnits",
    "planSummaryLink"
  )

  actual <- names(EQ_Actions(
    statecode = "RI",
    fisc_year_start = 2014,
    fisc_year_end = 2020,
    api_key = .setEQKey()
  ))

  length.diff <- length(setdiff(expected, actual))

  testthat::expect_equal(length.diff, 0)
})
})

# # need to update function so that this will fail if relevant
# httptest2::with_test_mocks("dir/wrong-statecode-actions-query", {
#   testthat::test_that("EQ_Actions returns error message if statecode is not a real statecode", {
#     expect_error(EQ_Actions(statecode = "NO",
#                             api_key = .setEQKey()))
#   })
# })
