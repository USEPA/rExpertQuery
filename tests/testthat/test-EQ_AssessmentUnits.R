with_test_mocks("MOau", {
  testthat::test_that("Assessment Units returns expected number of rows", {
    expect_equal(NROW(EQ_AssessmentUnits(
      statecode = "MO",
      au_name = "Leisure Lake",
      api_key = .setEQKey()
    )), 2)
  })

  testthat::test_that("Assessment Units returns expected number of columns", {
    expect_equal(NCOL(EQ_AssessmentUnits(
      statecode = "MO",
      au_name = "Leisure Lake",
      api_key = .setEQKey()
    )), 20)
  })

  testthat::test_that("Assessment Units returns expected column names", {
    expected <- c(
      "objectId", "region", "state", "organizationType",
      "organizationId", "organizationName", "waterType",
      "locationTypeCode", "locationText", "useClassName",
      "assessmentUnitId", "assessmentUnitName",
      "assessmentUnitStatus", "reportingCycle", "cycleId",
      "locationDescription", "sizeSource", "sourceScale",
      "waterSize", "waterSizeUnits"
    )

    actual <- names(EQ_AssessmentUnits(
      statecode = "MO",
      au_name = "Leisure Lake",
      api_key = .setEQKey()
    ))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)
  })
})
