library(httptest2)
library(withr)

# Find package root
pkg_root <- if (requireNamespace("rprojroot", quietly = TRUE)) {
  rprojroot::find_package_root_file()
} else {
  getwd()
}

fixtures_root <- file.path(pkg_root, "inst", "extdata", "htt2")
dir.create(fixtures_root, recursive = TRUE, showWarnings = FALSE)

record_to_pkg <- function(subdir, code) {
  target <- file.path(fixtures_root, subdir)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  message("Recording into: ", normalizePath(target, winslash = "/"))

  # Record by working from the target directory
  withr::with_dir(target, {
    options(httptest2.verbose = TRUE)
    httptest2::capture_requests({
      eval(substitute(code), envir = parent.frame())
    })
  })
  invisible(target)
}

  # Your recordings
  record_to_pkg("ORad", {
    EQ_ActionsDocuments(
      state = "OR",
      comp_date_start = "01-01-2018",
      comp_date_end = "12-31-2020",
      api_key = .setEQKey()
    )
  })

  record_to_pkg("RIact", {
    EQ_Actions(
      statecode = "RI",
      fisc_year_start = 2014,
      fisc_year_end = 2020,
      api_key = .setEQKey()
    )
  })

  record_to_pkg("ILcat5", {
    EQ_Assessments(
      statecode = "IL",
      epa_ir_cat = 5,
      param_group = "ALGAL GROWTH",
      api_key = .setEQKey()
    )
  })

  record_to_pkg("MOau", {
    EQ_AssessmentUnits(
      statecode = "MO",
      au_name = "Leisure Lake",
      api_key = .setEQKey()
    )
  })

  record_to_pkg("MTml", {
    EQ_AUsMLs(
      org_id = "MTDEQ",
      au_name = "Kleinschmidt Creek",
      api_key = .setEQKey()
    )
  })

  record_to_pkg("ALcc", {
    EQ_CatchCorr(
      auid = "AL03150202-0404-110",
      api_key = .setEQKey()
    )
  })

  record_to_pkg("NATact", {
    EQ_NationalExtract("actions", limit = 10)
  })

  record_to_pkg("NATtmdl", {
    EQ_NationalExtract("actions", limit = 10)
  })

  record_to_pkg("TXsrc", {
    EQ_Sources(
      report_cycle = 2018,
      statecode = "TX",
      source = "AGRICULTURE",
      api_key = .setEQKey()
    )
  })

  record_to_pkg("FLtmdl", {
    EQ_Sources(
      report_cycle = 2018,
      statecode = "TX",
      source = "AGRICULTURE",
      api_key = .setEQKey()
    )
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
