httptest2::with_test_mocks("inst/extdata/ORad", {
  testthat::test_that("EQ_ActionsDocuments returns expected number of row", {
    expect_equal(NROW(EQ_ActionsDocuments(
      state = "OR",
      comp_date_start = "01-01-2018",
      comp_date_end = "12-31-2020",
      api_key = .setEQKey()
    )), 48)
  })

  testthat::test_that("EQ_ActionsDocuments returns expected number of columns", {
    expect_equal(NCOL(EQ_ActionsDocuments(
      state = "OR",
      comp_date_start = "01-01-2018",
      comp_date_end = "12-31-2020",
      api_key = .setEQKey()
    )), 18)
  })

  testthat::test_that("EQ_ActionsDocuments returns expected column names", {
    expected <- c(
      "objectId", "actionDocumentUrl", "actionId", "actionName",
      "actionType", "completionDate", "organizationId",
      "organizationName", "organizationType", "region", "state",
      "tmdlDate", "documentDesc", "documentFileName",
      "documentFileTypeName", "documentKey", "documentName",
      "actionDocumentType"
    )

    actual <- names(EQ_ActionsDocuments(
      state = "OR",
      comp_date_start = "01-01-2018",
      comp_date_end = "12-31-2020",
      api_key = .setEQKey()
    ))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)
  })
})
