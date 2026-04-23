httptest2::with_mock_dir("dir/nat-extract-actions", {
  testthat::test_that("EQ_NationalProfile returns expected column names", {
    expect_equal(NROW(EQ_Sources(report_cycle = 2018,
                                 statecode = "TX",
                                 source = "AGRICULTURE",
                                 api_key = .setEQKey())
    ), 305)
  })

  testthat::test_that("EQ_Sources returns results for all expected statecodes", {
    expect_equal(NCOL(EQ_Sources(report_cycle = 2018,
                                 statecode = "TX",
                                 source = "AGRICULTURE",
                                 api_key = .setEQKey())
    ), 21)
  })

  testthat::test_that("EQ_Sources returns expected column names", {

    expected <- c("objectId", "region", "state", "organizationType",
                  "organizationId", "organizationName", "waterType",
                  "assessmentUnitId", "assessmentUnitName", "reportingCycle",
                  "overallStatus", "epaIrCategory", "stateIrCategory",
                  "parameterGroup", "causeName", "sourceName", "confirmed",
                  "cycleId", "locationDescription", "waterSize", "waterSizeUnits")

    actual <- names(EQ_Sources(report_cycle = 2018,
                               statecode = "TX",
                               source = "AGRICULTURE",
                               api_key = .setEQKey())
    )

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)

  })
})
