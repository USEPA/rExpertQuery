httptest2::with_mock_dir("dir/ILcat5", {
  testthat::test_that("EQ_Assessments returns expected number of row", {
    expect_equal(NROW(EQ_Assessments(statecode = "IL",
                                     epa_ir_cat = 5,
                                     param_group = "ALGAL GROWTH",
                                     api_key = .setEQKey())), 234)
  })

  testthat::test_that("EQ_Assessments returns expected number of columns", {
    expect_equal(NCOL(EQ_Assessments(statecode = "IL",
                                     epa_ir_cat = 5,
                                     param_group = "ALGAL GROWTH",
                                     api_key = .setEQKey())), 56)
  })

  testthat::test_that("EQ_Assessments returns expected column names", {

    expected <- c("objectId", "region", "state", "organizationType",
                  "organizationId", "organizationName", "waterType",
                  "reportingCycle", "cycleLastAssessed", "assessmentUnitId",
                  "assessmentUnitName", "assessmentUnitStatus", "overallStatus",
                  "epaIrCategory", "stateIrCategory", "useGroup", "useName",
                  "useClassName", "useSupport", "useIrCategory",
                  "useStateIrCategory", "monitoringStartDate",
                  "monitoringEndDate", "assessmentDate", "assessmentTypes",
                  "assessmentMethods", "assessmentBasis", "parameterGroup",
                  "parameterName", "parameterStatus", "parameterAttainment",
                  "parameterIrCategory", "parameterStateIrCategory", "delisted",
                  "delistedReason", "pollutantIndicator", "cycleFirstListed",
                  "alternateListingIdentifier", "vision303dPriority",
                  "cwa303dPriorityRanking", "cycleScheduledForTmdl",
                  "cycleExpectedToAttain", "consentDecreeCycle", "cycleId",
                  "seasonStartDate", "seasonEndDate", "associatedActionId",
                  "associatedActionName", "associatedActionType",
                  "associatedActionStatus", "associatedActionAgency",
                  "locationDescription", "sizeSource", "sourceScale",
                  "waterSize", "waterSizeUnits")

    actual <- names(EQ_Assessments(statecode = "IL",
                                   epa_ir_cat = 5,
                                   param_group = "ALGAL GROWTH",
                                   api_key = .setEQKey()))

    length.diff <- length(setdiff(expected, actual))

    testthat::expect_equal(length.diff, 0)

  })
})

httptest2::with_mock_dir("dir/NATassess", {
  testthat::test_that("EQ_Assessments returns error message if query results exceed maximum allowed", {
    expect_error(EQ_Assessments(api_key = .setEQKey()))
  })
})

