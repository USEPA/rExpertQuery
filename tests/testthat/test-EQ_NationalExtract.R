testthat::test_that("EQ_NationalExtract returns all expected columns for Actions", {
  testthat::skip_if_not_installed("data.table")

  # load mock actions
  actions_csv <- make_raw_csv_from_rds("actions", c("htt2","NATact","actions.rds"))

  # set up override list
  overrides <- list()

  # stop retries for downloads
  if (exists("nat.extract.retries", envir = asNamespace("rExpertQuery"), inherits = FALSE)) {
    overrides[[length(overrides) + 1L]] <- .local_override(
      "rExpertQuery", "nat.extract.retries",
      function(...) tempfile(fileext = ".zip")  # return a dummy zip path
    )
  }

  overrides[[length(overrides) + 1L]] <- .local_override(
    "utils", "unzip", mock_unzip_returning(actions_csv)
  )
  # restore all overrides after this test
  on.exit(.local_restore(overrides), add = TRUE)

  # run function and test
  expected <- c(
    "objectId", "region", "state", "organizationType", "organizationId",
    "organizationName", "waterType", "pollutantGroup", "pollutant",
    "addressedParameterGroup", "addressedParameter", "sourceType",
    "npdesIdentifier", "otherIdentifier", "actionId", "actionName",
    "actionAgency", "inIndianCountry", "explicitMarginOfSafety",
    "implicitMarginOfSafety", "includeInMeasure", "completionDate",
    "tmdlDate", "fiscalYearEstablished", "assessmentUnitId",
    "assessmentUnitName", "loadAllocation", "loadAllocationUnits",
    "locationDescription", "tmdlEndpoint", "waterSize", "waterSizeUnits",
    "wasteLoadAllocation", "planSummaryLink"
  )

  res <- EQ_NationalExtract("actions")
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(length(setdiff(expected, names(res))), 0)
})

testthat::test_that("EQ_NationalExtract returns all expected columns for TMDLs", {
  testthat::skip_if_not_installed("data.table")

  # load mock actions
  tmdls_csv <- make_raw_csv_from_rds("tmdl", c("htt2","NATtmdl","tmdls.rds"))

  # set up override list
  overrides <- list()

  # stop retries for downloads
  if (exists("nat.extract.retries", envir = asNamespace("rExpertQuery"), inherits = FALSE)) {
    overrides[[length(overrides) + 1L]] <- .local_override(
      "rExpertQuery", "nat.extract.retries",
      function(...) tempfile(fileext = ".zip")  # return a dummy zip path
    )
  }

  overrides[[length(overrides) + 1L]] <- .local_override(
    "utils", "unzip", mock_unzip_returning(tmdls_csv)
  )

  # restore all overrides after this test
  on.exit(.local_restore(overrides), add = TRUE)

  # run function and test
  expected <- c(
    "objectId", "region", "state", "organizationType",
    "organizationId", "organizationName", "waterType",
    "pollutantGroup", "pollutant", "addressedParameterGroup",
    "addressedParameter", "sourceType", "npdesIdentifier",
    "otherIdentifier", "actionId", "actionName",
    "actionAgency", "inIndianCountry",
    "explicitMarginOfSafety", "implicitMarginOfSafety",
    "includeInMeasure", "completionDate", "tmdlDate",
    "fiscalYearEstablished", "assessmentUnitId",
    "assessmentUnitName", "loadAllocation",
    "loadAllocationUnits", "locationDescription",
    "tmdlEndpoint", "waterSize", "waterSizeUnits",
    "wasteLoadAllocation", "planSummaryLink"
  )

  res <- EQ_NationalExtract("tmdl")
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(length(setdiff(expected, names(res))), 0)
})
