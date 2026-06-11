
  testthat::test_that("EQ_CatchCorr returns expected number of rows", {
    with_test_mocks("ALcc", {
    expect_equal(NROW(EQ_CatchCorr(
      auid = "AL03150202-0404-110",
      .setEQKey()
    )), 726)
  })
  })

  testthat::test_that("EQ_CatchCorr returns expected number of columns", {
    with_test_mocks("ALcc", {
    expect_equal(NCOL(EQ_CatchCorr(
      auid = "AL03150202-0404-110",
      .setEQKey()
    )), 11)
  })
    })

  testthat::test_that("EQ_CatchCorr returns expected column names", {
    with_test_mocks("ALcc", {
    expected <- c(
      "objectId", "region", "state", "organizationType",
      "organizationId", "organizationName", "assessmentUnitId",
      "assessmentUnitName", "catchmentNhdPlusId", "reportingCycle",
      "cycleId"
    )

    actual <- names(EQ_CatchCorr(
      auid = "AL03150202-0404-110",
      .setEQKey()
    ))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)
  })
})
