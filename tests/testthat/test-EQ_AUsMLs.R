httptest2::with_mock_dir("dir/MT-ausmls-kleinschmidtcreek", {
  testthat::test_that("EQ_AUsMLs returns expected number of rows", {
    expect_equal(NROW(EQ_AUsMLs(org_id = "MTDEQ",
                                au_name = "Kleinschmidt Creek",
                                api_key = .setEQKey())
    ), 16)
  })

  testthat::test_that("EQ_AUsMLs returns expected number of columns", {
    expect_equal(NCOL(EQ_AUsMLs(org_id = "MTDEQ",
                                au_name = "Kleinschmidt Creek",
                                api_key = .setEQKey())
    ), 21)
  })

  testthat::test_that("EQ_AUsMLs returns expected column names", {

    expected <- c("objectId", "region", "state", "organizationType",
                  "organizationId", "organizationName", "waterType",
                  "useClassName", "monitoringLocationId",
                  "monitoringLocationOrgId", "assessmentUnitId",
                  "assessmentUnitName", "assessmentUnitStatus",
                  "reportingCycle", "cycleId", "locationDescription",
                  "monitoringLocationDataLink", "sizeSource", "sourceScale",
                  "waterSize", "waterSizeUnits"  )

    actual <- names(EQ_AUsMLs(org_id = "MTDEQ",
                              au_name = "Kleinschmidt Creek",
                              api_key = .setEQKey())
    )

    length.diff <- length(setdiff(expected,lki actual))

    testthat::expect_equal(length.diff, 0)

  })
})
