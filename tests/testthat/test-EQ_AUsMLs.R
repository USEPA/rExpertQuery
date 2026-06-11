
  testthat::test_that("EQ_AUsMLs returns expected number of rows", {
    with_test_mocks("MTml", {
    expect_equal(NROW(EQ_AUsMLs(
      org_id = "MTDEQ",
      au_name = "Kleinschmidt Creek",
      api_key = .setEQKey()
    )), 16)
  })
  })

  testthat::test_that("EQ_AUsMLs returns expected number of columns", {
    with_test_mocks("MTml", {
    expect_equal(NCOL(EQ_AUsMLs(
      org_id = "MTDEQ",
      au_name = "Kleinschmidt Creek",
      api_key = .setEQKey()
    )), 21)
  })
  })

  testthat::test_that("EQ_AUsMLs returns expected column names", {
    with_test_mocks("MTml", {
    expected <- c(
      "objectId", "region", "state", "organizationType",
      "organizationId", "organizationName", "waterType",
      "useClassName", "monitoringLocationId",
      "monitoringLocationOrgId", "assessmentUnitId",
      "assessmentUnitName", "assessmentUnitStatus",
      "reportingCycle", "cycleId", "locationDescription",
      "monitoringLocationDataLink", "sizeSource", "sourceScale",
      "waterSize", "waterSizeUnits"
    )

    actual <- names(EQ_AUsMLs(
      org_id = "MTDEQ",
      au_name = "Kleinschmidt Creek",
      api_key = .setEQKey()
    ))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)
  })
})
