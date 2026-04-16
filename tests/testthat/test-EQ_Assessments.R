test <- EQ_Assessments(statecode = "IL",
               epa_ir_cat = 3,
               api_key = .setEQKey())

# having trouble with assessments, need to run and update expected cols and values later

httptest2::with_mock_dir("dir/IL-cat3-assessments", {
  testthat::test_that("Assessments returns expected number of row", {
    expect_equal(NROW(EQ_Assessments(statecode = "IL",
                                     epa_ir_cat = 3,
                                     api_key = .setEQKey())), 82)
  })

  testthat::test_that("Assessments returns expected number of columns", {
    expect_equal(NCOL(EQ_Assessments(statecode = "IL",
                                     epa_ir_cat = 3,
                                     api_key = .setEQKey())), 18)
  })

  testthat::test_that("Assessments returns expected column names", {

    expected <- c("objectId", "actionDocumentUrl", "actionId", "actionName",
                  "actionType", "completionDate", "organizationId",
                  "organizationName", "organizationType", "region", "state",
                  "tmdlDate", "documentDesc", "documentFileName",
                  "documentFileTypeName", "documentKey", "documentName",
                  "actionDocumentType" )

    actual <- names(EQ_Assessments(statecode = "IL",
                                   epa_ir_cat = 3,
                                   api_key = .setEQKey()))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)

  })
})

httptest2::with_mock_dir("dir/all-assessments", {
  testthat::test_that("EQ_Assessments returns error message if query results exceed maximum allowed", {
    expect_equal(NROW(EQ_Assessments(api_key = .setEQKey())), 82)
  })
  # need to update this once I can see what result looks like

  testthat::test_that("Assessments returns expected number of columns", {
    expect_equal(NCOL(EQ_Assessments(statecode = "IL",
                                     epa_ir_cat = 3,
                                     api_key = .setEQKey())), 18)
  })

  testthat::test_that("Assessments returns expected column names", {

    expected <- c("objectId", "actionDocumentUrl", "actionId", "actionName",
                  "actionType", "completionDate", "organizationId",
                  "organizationName", "organizationType", "region", "state",
                  "tmdlDate", "documentDesc", "documentFileName",
                  "documentFileTypeName", "documentKey", "documentName",
                  "actionDocumentType" )

    actual <- names(EQ_Assessments(statecode = "IL",
                                   epa_ir_cat = 3,
                                   api_key = .setEQKey()))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)

  })
})

