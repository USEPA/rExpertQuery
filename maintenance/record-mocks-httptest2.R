# Run this script manually; do NOT run it during tests/CI
# install.packages(c("httptest2","withr","rprojroot","devtools")) if needed

library(httptest2)
library(withr)

# Find your package root
pkg_root <- if (requireNamespace("rprojroot", quietly = TRUE)) {
  rprojroot::find_package_root_file()
} else {
  getwd()  # run from package root if rprojroot not available
}

# Choose a very short scratch directory for recording
scratch <- if (dir.exists("C:/m")) "C:/m/htt2" else tempfile("htt2")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)

# Load your package code (adjust package name if using library())
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(pkg_root, quiet = TRUE)
} else {
  # If your package is installed, you can do: library(yourpkg)
  message("Consider installing devtools for load_all(); falling back to installed package if available.")
}

# Record outside the package dir so httptest2 doesn't prepend tests/testthat
withr::with_dir(tempdir(), {
  # Optional verbose capture
  old <- options(httptest2.verbose = TRUE)
  on.exit(options(old), add = TRUE)

  record <- function(name, code) {
    target <- file.path(scratch, name)
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    message("Recording into: ", normalizePath(target, winslash = "/"))
    httptest2::with_mock_dir(target, {
      httptest2::capture_requests({
        eval(substitute(code), envir = parent.frame())
      })
    })
  }

  # Minimal sanity check to ensure capture works in this session
  tmp_check <- file.path(scratch, "_check")
  dir.create(tmp_check, showWarnings = FALSE)
  httptest2::with_mock_dir(tmp_check, {
    httptest2::capture_requests({
      httr2::request("https://httpbin.org/get") |> httr2::req_perform()
    })
  })

  # Your recordings
  record("ORad", {
    EQ_ActionsDocuments(
      state = "OR",
      comp_date_start = "01-01-2018",
      comp_date_end = "12-31-2020",
      api_key = .setEQKey()
    )
  })

  record("RIact", {
    EQ_Actions(
      statecode = "RI",
      fisc_year_start = 2014,
      fisc_year_end = 2020,
      api_key = .setEQKey()
    )
  })

  record("ILcat5", {
    EQ_Assessments(
      statecode = "IL",
      epa_ir_cat = 5,
      param_group = "ALGAL GROWTH",
      api_key = .setEQKey()
    )
  })

  record("MOau", {
    EQ_AssessmentUnits(
      statecode = "MO",
      au_name = "Leisure Lake",
      api_key = .setEQKey()
    )
  })

  record("MTml", {
    EQ_AUsMLs(
      org_id = "MTDEQ",
      au_name = "Kleinschmidt Creek",
      api_key = .setEQKey()
    )
  })

  record("ALcc", {
    EQ_CatchCorr(
      auid = "AL03150202-0404-110",
      api_key = .setEQKey()
    )
  })

  record("NATact", {
    EQ_NationalExtract("actions", limit = 10)
  })

  record("NATtmdl", {
    EQ_NationalExtract("actions", limit = 10)
  })

  record("TXsrc", {
    EQ_Sources(
      report_cycle = 2018,
      statecode = "TX",
      source = "AGRICULTURE",
      api_key = .setEQKey()
    )
  })

  record("FLtmdl", {
    EQ_Sources(
      report_cycle = 2018,
      statecode = "TX",
      source = "AGRICULTURE",
      api_key = .setEQKey()
    )
  })
})

# Copy the recordings into a short-named fixtures dir under inst/extdata
fixtures_root <- file.path(pkg_root, "inst", "extdata", "htt2")
dir.create(fixtures_root, recursive = TRUE, showWarnings = FALSE)

copy_dir <- function(src, dst) {
  files <- list.files(src, all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  for (f in files) {
    rel <- sub(paste0("^", normalizePath(src, winslash = "/")), "", normalizePath(f, winslash = "/"))
    rel <- sub("^/", "", rel)
    out <- file.path(dst, rel)
    if (dir.exists(f)) {
      dir.create(out, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      file.copy(f, out, overwrite = TRUE)
    }
  }
}

# Copy each recorded subdir (skip the _check dir)
for (d in list.dirs(scratch, full.names = TRUE, recursive = FALSE)) {
  nm <- basename(d)
  if (nm == "_check") next
  copy_dir(d, file.path(fixtures_root, nm))
}

message("Fixtures copied to: ", normalizePath(fixtures_root, winslash = "/"))
