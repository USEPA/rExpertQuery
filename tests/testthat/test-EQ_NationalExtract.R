testthat::test_that("EQ_NationalExtract returns all expected columns for Actions (CSV from RDS)", {
  testthat::skip_if_not_installed("data.table")

  # load mock actions
  actions_df_raw <- readRDS(testthat::test_path("htt2", "NATact", "actions.rds"))

  # write to temp csv
  tmp_dir <- tempfile("ne_actions_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  actions_csv <- file.path(tmp_dir, "actions.csv")
  data.table::fwrite(actions_df_raw, actions_csv)

  # set up override list
  overrides <- list()

  # stop retries for downloads
  if (exists("nat.extract.retries", envir = asNamespace("rExpertQuery"), inherits = FALSE)) {
    overrides[[length(overrides) + 1L]] <- .local_override(
      "rExpertQuery", "nat.extract.retries",
      function(...) tempfile(fileext = ".zip")  # return a dummy zip path
    )
  }
  # make unzip return the temp csv path
  overrides[[length(overrides) + 1L]] <- .local_override(
    "utils", "unzip",
    function(zipfile, exdir = tempdir(), ...) {
      dest <- file.path(exdir, basename(actions_csv))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(actions_csv, dest, overwrite = TRUE)
      dest
    }
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
  tmdls_df_raw <- readRDS(testthat::test_path("htt2", "NATtmdl", "tmdls.rds"))

  # write to temp csv
  tmp_dir <- tempfile("ne_tmdls_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmdls_csv <- file.path(tmp_dir, "tmdls.csv")
  data.table::fwrite(tmdls_df_raw, tmdls_csv)

  # set up override list
  overrides <- list()

  # stop retries for downloads
  if (exists("nat.extract.retries", envir = asNamespace("rExpertQuery"), inherits = FALSE)) {
    overrides[[length(overrides) + 1L]] <- .local_override(
      "rExpertQuery", "nat.extract.retries",
      function(...) tempfile(fileext = ".zip")  # return a dummy zip path
    )
  }
  # make unzip return the temp csv path
  overrides[[length(overrides) + 1L]] <- .local_override(
    "utils", "unzip",
    function(zipfile, exdir = tempdir(), ...) {
      dest <- file.path(exdir, basename(tmdls_csv))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(tmdls_csv, dest, overwrite = TRUE)
      dest
    }
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

  res <- EQ_NationalExtract("tmdls")
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(length(setdiff(expected, names(res))), 0)
})
