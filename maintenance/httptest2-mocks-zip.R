build_mocks_zip <- function(src = "tests/testthat/dir",
                            zip = "inst/extdata/httptest2-mocks.zip") {
  stopifnot(dir.exists(src))
  dir.create(dirname(zip), recursive = TRUE, showWarnings = FALSE)

  oldwd <- setwd(src)
  on.exit(setwd(oldwd), add = TRUE)

  files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  # keep only files; zip can be confused by directories
  files <- files[!dir.exists(files)]

  zip_abs <- normalizePath(file.path(oldwd, zip), mustWork = FALSE)
  if (file.exists(zip_abs)) file.remove(zip_abs)

  utils::zip(zipfile = zip_abs, files = files)
  message("Wrote: ", zip_abs)
  invisible(zip_abs)
}

build_mocks_zip()
