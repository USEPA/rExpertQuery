httptest2::with_mock_dir("dir/MO-leisurelake-assessmentunits", {
  testthat::test_that("Assessment Units returns expected number of rows", {
    expect_equal(NROW(EQ_AssessmentUnits(statecode = "MO",
                                         au_name = "Leisure Lake",
                                         api_key = .setEQKey())), 2)
  })

  testthat::test_that("Assessment Units returns expected number of columns", {
    expect_equal(NCOL(EQ_AssessmentUnits(statecode = "MO",
                                         au_name = "Leisure Lake",
                                         api_key = .setEQKey())), 20)
  })

  testthat::test_that("Assessment Units returns expected column names", {

    expected <- c("objectId"             "region"               "state"                "organizationType"
                  [5] "organizationId"       "organizationName"     "waterType"            "locationTypeCode"
                  [9] "locationText"         "useClassName"         "assessmentUnitId"     "assessmentUnitName"
                  [13] "assessmentUnitStatus" "reportingCycle"       "cycleId"              "locationDescription"
                  [17] "sizeSource"           "sourceScale"          "waterSize"            "waterSizeUnits" )

    actual <- names(EQ_AssessmentUnits(statecode = "MO",
                                       au_name = "Leisure Lake",
                                       api_key = .setEQKey()))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)

  })
})
