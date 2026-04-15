httptest2::with_mock_dir("dir/RI-actions", {
  testthat::test_that("Actions returns expected number of row", {
    expect_equal(NROW(EQ_Actions(statecode = "RI", api_key = EPATADA:::.setEQKey())), 485)
  })

  testthat::test_that("Actions returns expected number of columns")
})
