# tests/testthat/helper-mocks.R
# Resolve the fixtures root whether running from source or installed package
pkg <- "rExpertQuery"

fixtures_root <- system.file("extdata", "htt2", package = pkg)
if (identical(fixtures_root, "")) {
  # Running from source tree
  fixtures_root <- normalizePath(file.path("inst", "extdata", "htt2"), mustWork = FALSE)
}

# Helper to run code inside a specific subdirectory of the fixtures
with_pkg_mocks <- function(subdir, code) {
  dir <- if (missing(subdir) || is.null(subdir)) fixtures_root else file.path(fixtures_root, subdir)
  if (!dir.exists(dir)) stop("Mock directory not found: ", dir)
  httptest2::with_mock_dir(dir, eval.parent(substitute(code)))
}
