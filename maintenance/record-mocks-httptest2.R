# Run manually, not in CI/tests

suppressPackageStartupMessages({
  library(httptest2)
  library(withr)
})

# Load your package code so EQ_* are available
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  library(rExpertQuery)  # if installed
}

# Find package root
pkg_root <- if (requireNamespace("rprojroot", quietly = TRUE)) {
  rprojroot::find_package_root_file()
} else {
  getwd()
}

# Fixtures dir inside the package
fixtures_root <- file.path(pkg_root, "inst", "extdata", "htt2")
dir.create(fixtures_root, recursive = TRUE, showWarnings = FALSE)

# Recorder that writes directly into inst/extdata/htt2/<subdir>
record_to_pkg <- function(subdir, code) {
  target <- file.path(fixtures_root, subdir)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  message("Recording into: ", normalizePath(target, winslash = "/"))

  # Work from target so files are written there
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
  EQ_NationalExtract("tmdl", limit = 10)  # was "actions" in your snippet
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
    statecode = "FL",   # your snippet had TX; adjust if needed
    source = "AGRICULTURE",
    api_key = .setEQKey()
  )
})

record_to_pkg("NULLdv", {
  EQ_DomainValues(api_key = .setEQKey())
})

record_to_pkg("assessTypesdv", {
  EQ_DomainValues(domain = "assess_types", api_key = .setEQKey())
})


message("All fixtures recorded under: ", normalizePath(fixtures_root, winslash = "/"))
