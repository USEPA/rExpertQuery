library(httptest2)

# Run this from the project root so the absolute path below is correct
httptest2::with_mock_dir("dir/ORad", {
  httptest2::capture_requests({
    EQ_ActionsDocuments(
      state = "OR",
      comp_date_start = "01-01-2018",
      comp_date_end = "12-31-2020",
      api_key = .setEQKey()
    )
  })
})

httptest2::with_mock_dir("dir/ORad", {
  httptest2::capture_requests({
    EQ_Actions(statecode = "RI",
               fisc_year_start = 2014,
               fisc_year_end = 2020,
               api_key = .setEQKey())
  })
})
