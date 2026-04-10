# load required packages
library(testthat)
library(httptest2)

# fixtures directory
fixtures_dir <- function(...) testthat::test_path("fixtures", ...)

# toggle recording mode by environmental variable
is_recording <- function() identical(Sys.getenv("REXPQ_RECORD"), "1")

# redact sensitive headers when recording
if (is_recording()) {
  httptest2::redact_headers("Authorization", "X-API-Key", "X-Auth-Token")
}

# wrapper to record or replay a given fixture set
with_fixtures <- function(name, code) {
  code <- substitute(code)
  dir <- fixtures_dir(name)
  if (is_recording()) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    httptest2::capture_requests(
      eval(code, parent.frame()),
      dir = dir
    )
  } else {
    httptest2::with_mock_dir(
      dir,
      eval(code, parent.frame())
    )
  }
}
