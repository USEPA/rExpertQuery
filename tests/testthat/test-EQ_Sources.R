# tests/testthat/test-EQ_Sources.R

testthat::test_that("EQ_Sources returns expected number of rows", {
  with_test_mocks("TXsrc", {
    testthat::expect_equal(NROW(EQ_Sources(
      report_cycle = 2018,
      statecode = "TX",
      source = "AGRICULTURE",
      api_key = .setEQKey()
    )), 305)
  })
})

testthat::test_that("EQ_Sources returns expected number of columns", {
  with_test_mocks("TXsrc", {
    testthat::expect_equal(NCOL(EQ_Sources(
      report_cycle = 2018,
      statecode = "TX",
      source = "AGRICULTURE",
      api_key = .setEQKey()
    )), 21)
  })
})

testthat::test_that("EQ_Sources returns expected column names", {
  with_test_mocks("TXsrc", {
    expected <- c(
      "objectId", "region", "state", "organizationType",
      "organizationId", "organizationName", "waterType",
      "assessmentUnitId", "assessmentUnitName", "reportingCycle",
      "overallStatus", "epaIrCategory", "stateIrCategory",
      "parameterGroup", "causeName", "sourceName", "confirmed",
      "cycleId", "locationDescription", "waterSize", "waterSizeUnits"
    )

    actual <- names(EQ_Sources(
      report_cycle = 2018,
      statecode = "TX",
      source = "AGRICULTURE",
      api_key = .setEQKey()
    ))

    length.diff <- length(setdiff(expected, actual))
    testthat::expect_equal(length.diff, 0)
  })
})
