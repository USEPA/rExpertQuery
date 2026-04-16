httptest2::with_mock_dir("dir/OR-actionsdocuments", {
  testthat::test_that("Actions returns expected number of row", {
    expect_equal(NROW(EQ_ActionsDocuments(state = "OR",
                                          api_key = .setEQKey())), 82)
  })

  testthat::test_that("Actions returns expected number of columns", {
    expect_equal(NCOL(EQ_ActionsDocuments(state = "OR",
                                          api_key = .setEQKey())), 18)
  })

  testthat::test_that("Actions returns expected column names", {

    expected <- c("objectId", "actionDocumentUrl", "actionId", "actionName",
                  "actionType", "completionDate", "organizationId",
                  "organizationName", "organizationType", "region", "state",
                  "tmdlDate", "documentDesc", "documentFileName",
                  "documentFileTypeName", "documentKey", "documentName",
                  "actionDocumentType" )

    actual <- names(EQ_ActionsDocuments(state = "OR",
                                        api_key = .setEQKey()))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)

  })
})
