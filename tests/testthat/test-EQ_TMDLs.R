httptest2::with_mock_dir("dir/FLtmdl", {
  testthat::test_that("EQ_TMDLs returns expected number of rows", {
    expect_equal(NROW(EQ_TMDLs(
      fisc_year_start = 2018,
      fisc_year_end = 2020,
      statecode = "FL",
      api_key = .setEQKey()
    )), 333)
  })

  testthat::test_that("EQ_TMDLs returns expected number of columns", {
    expect_equal(NCOL(EQ_TMDLs(
      fisc_year_start = 2018,
      fisc_year_end = 2020,
      statecode = "FL",
      api_key = .setEQKey()
    )), 34)
  })

  testthat::test_that("EQ_TMDLs returns expected column names", {
    expected <- c(
      "objectId", "region", "state", "organizationType",
      "organizationId", "organizationName", "waterType",
      "pollutantGroup", "pollutant", "addressedParameterGroup",
      "addressedParameter", "sourceType", "npdesIdentifier",
      "otherIdentifier", "actionId", "actionName", "actionAgency",
      "inIndianCountry", "explicitMarginOfSafety",
      "implicitMarginOfSafety", "includeInMeasure", "completionDate",
      "tmdlDate", "fiscalYearEstablished", "assessmentUnitId",
      "assessmentUnitName", "loadAllocation", "loadAllocationUnits",
      "locationDescription", "tmdlEndpoint", "waterSize",
      "waterSizeUnits", "wasteLoadAllocation", "planSummaryLink"
    )

    actual <- names(EQ_TMDLs(
      fisc_year_start = 2018,
      fisc_year_end = 2020,
      statecode = "FL",
      api_key = .setEQKey()
    ))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)
  })
})
