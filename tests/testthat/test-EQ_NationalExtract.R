testthat::test_that("EQ_NationalExtract returns all expected columns for Actions", {
  testthat::skip_if_not_installed("data.table")

  expected <- c(
    "objectId", "region", "state", "organizationType", "organizationId",
    "organizationName", "waterType", "parameterGroup", "parameter", "actionType",
    "actionId", "actionName", "actionAgency", "inIndianCountry", "includeInMeasure",
    "completionDate", "assessmentUnitId", "assessmentUnitName", "fiscalYearEstablished",
    "locationDescription", "waterSize", "waterSizeUnits", "planSummaryLink"
  )

  actions_df_final <- readRDS(testthat::test_path("htt2", "NATact", "actions.rds"))

  miss <- setdiff(expected, names(actions_df_final))
  if (length(miss)) {
    stop("Fixture actions.rds is missing expected columns: ", paste(miss, collapse = ", "))
  }

  tmp_dir <- tempfile("ne_actions_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  actions_csv <- file.path(tmp_dir, "actions.csv")
  data.table::fwrite(actions_df_final, actions_csv)

  overrides <- list()

  overrides[[length(overrides) + 1L]] <- .local_override(
    "rExpertQuery", ".setEQKey",
    function(...) "DUMMY_KEY"
  )

  overrides[[length(overrides) + 1L]] <- .local_override(
    "rExpertQuery", ".download_nat_extract",
    function(url, max_retries) {
      data.table::fread(actions_csv, check.names = FALSE)
    }
  )

  real_fread <- get("fread", envir = asNamespace("data.table"))
  map_path <- system.file("extdata", "EQColumnsForPOST.csv",
                          package = "rExpertQuery", mustWork = TRUE)

  overrides[[length(overrides) + 1L]] <- .local_override(
    "data.table", "fread",
    function(input, ...) {
      path <- tryCatch(as.character(input), error = function(e) "")
      if (length(path) == 1L &&
          tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) "") ==
          tryCatch(normalizePath(map_path, winslash = "/", mustWork = FALSE), error = function(e) "")) {
        data.table::data.table(
          col.name = expected,
          nat_extract = expected,
          position = seq_along(expected),
          actions = seq_along(expected)
        )
      } else {
        real_fread(input, ...)
      }
    }
  )

  on.exit(.local_restore(overrides), add = TRUE)

  res <- EQ_NationalExtract("actions")
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(length(setdiff(expected, names(res))), 0)
})

testthat::test_that("EQ_NationalExtract returns all expected columns for Sources", {
  testthat::skip_if_not_installed("data.table")

  # final columns
  expected <- c(
    "objectId", "region", "state", "organizationType", "organizationId",
    "organizationName", "waterType", "assessmentUnitId", "assessmentUnitName",
    "reportingCycle", "overallStatus", "epaIrCategory", "stateIrCategory",
    "parameterGroup", "causeName", "sourceName", "confirmed", "cycleId",
    "locationDescription", "waterSize", "waterSizeUnits"
  )

  # load mock
  sources_df_final <- readRDS(testthat::test_path("htt2", "NATsource", "sources.rds"))

  # check cols
  miss <- setdiff(expected, names(sources_df_final))
  if (length(miss)) {
    stop("Fixture sources.rds is missing expected columns: ", paste(miss, collapse = ", "))
  }

  tmp_dir <- tempfile("ne_sources_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  sources_csv <- file.path(tmp_dir, "sources.csv")
  data.table::fwrite(sources_df_final, sources_csv)

  overrides <- list()

  # provide dummy key
  overrides[[length(overrides) + 1L]] <- .local_override(
    "rExpertQuery", ".setEQKey",
    function(...) "DUMMY_KEY"
  )

  # mock the download helper
  overrides[[length(overrides) + 1L]] <- .local_override(
    "rExpertQuery", ".download_nat_extract",
    function(url, max_retries) {
      data.table::fread(sources_csv, check.names = FALSE)
    }
  )

  # return identity mapping for cols
  real_fread <- get("fread", envir = asNamespace("data.table"))
  map_path <- system.file("extdata", "EQColumnsForPOST.csv",
                          package = "rExpertQuery", mustWork = TRUE)

  overrides[[length(overrides) + 1L]] <- .local_override(
    "data.table", "fread",
    function(input, ...) {
      path <- tryCatch(as.character(input), error = function(e) "")
      if (length(path) == 1L &&
          tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) "") ==
          tryCatch(normalizePath(map_path, winslash = "/", mustWork = FALSE), error = function(e) "")) {
        data.table::data.table(
          col.name    = expected,
          nat_extract = expected,
          position    = seq_along(expected),
          sources     = seq_along(expected) # non-NA marks inclusion for this profile
        )
      } else {
        real_fread(input, ...)
      }
    }
  )

  on.exit(.local_restore(overrides), add = TRUE)

  # run and test
  res <- EQ_NationalExtract("sources")
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(length(setdiff(expected, names(res))), 0)
})
