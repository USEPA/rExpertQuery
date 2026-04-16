httptest2::with_mock_dir("dir/RI-actions", {
  testthat::test_that("Actions returns expected number of row", {
    expect_equal(NROW(EQ_Actions(statecode = "RI",
                                 api_key = .setEQKey())), 485)
  })

  testthat::test_that("Actions returns expected number of columns", {
    expect_equal(NCOL(EQ_Actions(statecode = "RI",
                                 api_key = .setEQKey())), 23)
  })

  testthat::test_that("Actions returns expected column names", {

    param.cw <- utils::read.csv(system.file("extdata", "EQParamsCrosswalk.csv",
                                            package = "rExpertQuery"
    ))

    expected <- c("objectId", "region", "state", "organizationType",
                  "organizationId", "organizationName", "waterType",
                  "parameterGroup", "parameter", "actionType", "actionId",
                  "actionName", "actionAgency", "inIndianCountry",
                  "includeInMeasure", "completionDate", "assessmentUnitId",
                  "assessmentUnitName", "fiscalYearEstablished",
                  "locationDescription", "waterSize", "waterSizeUnits",
                  "planSummaryLink")

    actual <- names(EQ_Actions(statecode = "RI",
                         api_key = .setEQKey()))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)

  })
})
