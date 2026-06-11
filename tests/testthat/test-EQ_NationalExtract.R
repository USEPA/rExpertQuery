testthat::test_that("EQ_NationalExtract returns all expected columns for Actions",{
  with_test_mocks("NATact", {
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

    actual <- names(EQ_NationalExtract("actions",
      limit = 10
    ))


    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)
  })
})

testthat::test_that("EQ_NationalExtract returns all expected columns for Actions",{
  with_test_mocks("NATtmdl", {
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
