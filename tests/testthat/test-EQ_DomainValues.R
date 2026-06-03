testthat::test_that("EQ_DomainValues returns expected columns when domain = NULL", {
  expected <- c("eq_param", "attains_ws_name", "attains_ws_field")

  actual <- names(EQ_DomainValues(api_key = .setEQKey()))

  length.diff <- length(setdiff(expected, actual))

  testthat::expect_equal(length.diff, 0)
})

testthat::test_that("EQ_DomainValues returns expected values when domain = NULL", {
  expected <- c(
    "act_agency", "act_agency", "act_status", "act_type",
    "ad_param", "ad_param_group", "assess_basis", "assess_methods",
    "assess_types", "au_status", "cause", "delist_reason",
    "doc_type", "file_type", "loc_type", "org_id", "org_name",
    "param_attain", "param_group", "param_name",
    "param_state_ir_cat", "param_status", "source_scale",
    "source_type", "statecode", "use_name", "use_support",
    "water_type"
  )

  actual <- EQ_DomainValues(api_key = .setEQKey()) |>
    dplyr::pull(eq_param)

  length.diff <- length(setdiff(expected, actual))

  testthat::expect_equal(length.diff, 0)
})


testthat::test_that("EQ_DomainValues returns expected columns when domain = 'assess_types'", {
  expected <- c(
    "OTHER",
    "PATHOGEN INDICATORS",
    "PHYSICAL/CHEMICAL",
    "HABITAT",
    "TOXICOLOGICAL",
    "OTHER PUBLIC HEALTH INDICATORS",
    "OTHER AQUATIC LIFE INDICATORS",
    "BIOLOGICAL"
  )

  actual <- EQ_DomainValues(domain = "assess_types", api_key = .setEQKey()) |>
    dplyr::pull(name)

  length.diff <- length(setdiff(expected, actual))

  testthat::expect_equal(length.diff, 0)
})
