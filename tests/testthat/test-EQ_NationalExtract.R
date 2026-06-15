testthat::test_that("EQ_NationalExtract returns all expected columns for Actions", {
  # Load the pre-saved tiny fixture
  actions_df <- readRDS(testthat::test_path("htt2", "NATact", "actions.rds"))

  testthat::with_mocked_bindings(
    EQ_NationalExtract = function(profile, limit = NULL, api_key = NULL, ...) actions_df,
    .env = asNamespace("rExpertQuery"),  # replace with your package name if different
    {
      expected <- c(
        "objectId", "region", "state", "organizationType",
        "organizationId", "organizationName", "waterType",
        "parameterGroup", "parameter", "actionType", "actionId",
        "actionName", "actionAgency", "inIndianCountry",
        "includeInMeasure", "completionDate", "assessmentUnitId",
        "assessmentUnitName", "fiscalYearEstablished",
        "locationDescription", "waterSize", "waterSizeUnits",
        "planSummaryLink"
      )

      actual <- names(EQ_NationalExtract("actions", limit = 10))
      testthat::expect_equal(setdiff(expected, actual), character())
    }
  )
})

testthat::test_that("EQ_NationalExtract returns all expected columns for TMDLs",{
  # Load the pre-saved tiny fixture
  tmdls_df <- readRDS(testthat::test_path("htt2", "NATtmdl", "tmdls.rds"))

  testthat::with_mocked_bindings(
    EQ_NationalExtract = function(profile, limit = NULL, api_key = NULL, ...) tmdls_df,
    .env = asNamespace("rExpertQuery"),  # replace with your package name if different
    {
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

    actual <- names(EQ_NationalExtract("tmdl",
      limit = 10
    ))


    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)
  })
})
