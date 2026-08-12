httptest2::set_redactor(function(x) {
  # Request-side redaction (affects fixture filenames)
  if (inherits(x, "httr2_request")) {
    s3_pat <- "^cg-[0-9a-f\\-]+\\.s3-us-gov-west-1\\.amazonaws\\.com$"

    # Structured URL
    if (is.list(x$url)) {
      # Host/hostname → short forms
      if (!is.null(x$url$host) && is.character(x$url$host)) {
        if (x$url$host == "api.epa.gov") x$url$host <- "epa"
        if (grepl(s3_pat, x$url$host))   x$url$host <- "s3"
      }
      if (!is.null(x$url$hostname) && is.character(x$url$hostname)) {
        if (x$url$hostname == "api.epa.gov") x$url$hostname <- "epa"
        if (grepl(s3_pat, x$url$hostname))   x$url$hostname <- "s3"
      }
      # Path: strip EPA prefix; collapse national-downloads/<digits>/ → national/
      if (!is.null(x$url$path) && is.character(x$url$path) && nzchar(x$url$path)) {
        p <- x$url$path
        p <- sub("^/?expertquery/api/attains/?", "", p)
        p <- sub("^/?national-downloads/[0-9]+/", "national/", p)
        x$url$path <- p
      }
      return(x)
    }

    # Character URL: parse/edit/rebuild
    if (is.character(x$url) && length(x$url) == 1L && nzchar(x$url)) {
      if (requireNamespace("httr2", quietly = TRUE)) {
        u <- httr2::url_parse(x$url)
        if (!is.null(u$host) && is.character(u$host)) {
          if (u$host == "api.epa.gov") u$host <- "epa"
          if (grepl(s3_pat, u$host))   u$host <- "s3"
        }
        if (!is.null(u$hostname) && is.character(u$hostname)) {
          if (u$hostname == "api.epa.gov") u$hostname <- "epa"
          if (grepl(s3_pat, u$hostname))   u$hostname <- "s3"
        }
        if (!is.null(u$path) && is.character(u$path) && nzchar(u$path)) {
          p <- u$path
          p <- sub("^/?expertquery/api/attains/?", "", p)
          p <- sub("^/?national-downloads/[0-9]+/", "national/", p)
          u$path <- p
        }
        x$url <- httr2::url_build(u)
      }
      return(x)
    }

    return(x)
  }

  # Response-side redaction (body content)
  if (inherits(x, "httr2_response")) {
    # Only redact for text/JSON; skip binary like application/zip, octet-stream
    ct <- tolower(if (!is.null(x$headers[["content-type"]])) x$headers[["content-type"]] else "")
    if (grepl("application/zip|application/octet-stream", ct, perl = TRUE)) {
      return(x)
    }

    # gsubs for text/JSON
    x <- httptest2::gsub_response(
      x,
      "https?://cg-[0-9a-f\\-]+\\.s3-us-gov-west-1\\.amazonaws\\.com",
      "s3"
    )
    x <- httptest2::gsub_response(
      x,
      "/national-downloads/[0-9]+/",
      "/national/"
    )
    x <- httptest2::gsub_response(
      x,
      "https://api\\.epa\\.gov/expertquery/api/attains/",
      "epa/"
    )
    return(x)
  }

  x
})

record_to_tests <- function(subdir, code) {
  stopifnot(requireNamespace("testthat", quietly = TRUE))

  base_tt <- normalizePath(testthat::test_path("."), winslash = "/", mustWork = TRUE)
  proj    <- normalizePath(file.path(base_tt, "..", ".."), winslash = "/", mustWork = TRUE)
  target  <- file.path(base_tt, "htt2", subdir)

  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  message("Recording into: ", normalizePath(target, winslash = "/"))

  expr <- substitute(code)
  caller <- parent.frame()

  withr::with_dir(proj, {
    op <- options(httptest2.verbose = TRUE)
    on.exit(options(op), add = TRUE)
    httptest2::capture_requests(eval(expr, envir = caller))
  })

  src_roots <- unique(c(
    file.path(proj, "tests", "testthat"),
    base_tt,
    file.path(base_tt, "tests", "testthat")
  ))

  hosts <- c("api.epa.gov", "s3", "epa")

  moved_any <- FALSE
  for (root in src_roots) {
    if (!dir.exists(root)) next
    for (h in hosts) {
      src <- file.path(root, h)
      if (!dir.exists(src)) next
      dst <- file.path(target, h)
      dir.create(dst, recursive = TRUE, showWarnings = FALSE)

      files <- list.files(src, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
      if (length(files)) moved_any <- TRUE
      for (f in files) {
        rel <- sub(
          paste0("^", gsub("\\\\", "/", normalizePath(src, winslash = "/", mustWork = TRUE)), "/?"),
          "",
          gsub("\\\\", "/", normalizePath(f, winslash = "/", mustWork = TRUE))
        )
        out <- file.path(dst, rel)
        dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
        file.copy(f, out, overwrite = TRUE)
      }
      unlink(src, recursive = TRUE, force = TRUE)
    }
  }

  if (!moved_any) warning("No captured files found under any candidate roots.")
  message("Recorded fixtures moved to: ", normalizePath(target, winslash = "/"))
  invisible(target)
}

record_without_capture <- function(code) {
  expr <- substitute(code)
  eval(expr, envir = parent.frame())
}

with_test_mocks <- function(dir, code) {
  # Ensure recording is off, and restore on exit
  old_opts <- options("httptest2.capture" = FALSE)
  on.exit(options(old_opts), add = TRUE)

  # Absolute path to the mock directory
  path <- testthat::test_path("htt2", dir)
  if (!dir.exists(path)) {
    stop("Mock dir not found: ", path)
  }

  message("with_test_mocks using: ", normalizePath(path, winslash = "/"))
  httptest2::with_mock_dir(path, code)
}

# Temporarily override a function in a package namespace
.local_override <- function(pkg, name, replacement) {
  ns <- asNamespace(pkg)
  if (!exists(name, envir = ns, inherits = FALSE)) {
    stop("No binding '", name, "' in namespace '", pkg, "'.")
  }
  was_locked <- bindingIsLocked(name, ns)
  if (was_locked) unlockBinding(name, ns)
  old <- get(name, envir = ns)
  assign(name, replacement, envir = ns)
  if (was_locked) lockBinding(name, ns)
  list(pkg = pkg, name = name, old = old, was_locked = was_locked)
}

# Restore previously overridden bindings
.local_restore <- function(overrides) {
  for (ov in rev(overrides)) {
    ns <- asNamespace(ov$pkg)
    was_locked <- bindingIsLocked(ov$name, ns)
    if (was_locked) unlockBinding(ov$name, ns)
    assign(ov$name, ov$old, envir = ns)
    if (was_locked) lockBinding(ov$name, ns)
  }
}

# Build a temp CSV for a profile from an RDS that has FINAL columns:
# - Rename final -> raw (nat_extract) using EQColumnsForPOST.csv
# - Add NA placeholders for any raw columns required by the profile but missing
# Returns: absolute path to the CSV.
make_raw_csv_from_rds <- function(profile, rds_rel) {
  stopifnot(is.character(profile), length(profile) == 1L)
  # Resolve the RDS path under tests/testthat
  rds_path <- do.call(testthat::test_path, as.list(rds_rel))
  if (!file.exists(rds_path)) stop("RDS not found: ", rds_path)
  df_final <- readRDS(rds_path)

  # Mapping CSV shipped with the package
  map_path <- system.file("extdata", "EQColumnsForPOST.csv",
                          package = "rExpertQuery", mustWork = TRUE)
  map_df <- data.table::fread(map_path, check.names = TRUE)

  # Find the profile column in the mapping, case-insensitive
  prof_idx <- which(tolower(names(map_df)) == tolower(profile))
  if (!length(prof_idx)) {
    stop("Profile column '", profile, "' not found in mapping. Available: ",
         paste(names(map_df), collapse = ", "))
  }
  prof_col <- names(map_df)[prof_idx[1]]

  # Only rows used by this profile; order if 'position' exists
  map_subset <- map_df[!is.na(map_df[[prof_col]]), ]
  if ("position" %in% names(map_subset)) data.table::setorderv(map_subset, "position")

  if (!all(c("col.name", "nat_extract") %in% names(map_subset))) {
    stop("Mapping CSV missing 'col.name' and/or 'nat_extract'.")
  }

  # Final -> raw mapping
  raw_by_final <- setNames(as.character(map_subset[["nat_extract"]]),
                           as.character(map_subset[["col.name"]]))

  # Rename fixture columns back to raw if we have a mapping
  finals_in_fixture <- intersect(names(df_final), names(raw_by_final))
  df_raw <- df_final
  if (length(finals_in_fixture)) {
    names(df_raw)[match(finals_in_fixture, names(df_raw))] <- raw_by_final[finals_in_fixture]
  }

  # Ensure all raw columns for this profile exist; add NA if missing
  required_raw <- unique(na.omit(as.character(map_subset[["nat_extract"]])))
  missing_raw <- setdiff(required_raw, names(df_raw))
  if (length(missing_raw)) {
    # Fill as NA_character_; downstream code can coerce as needed
    for (nm in missing_raw) df_raw[[nm]] <- NA_character_
  }

  # Write to a temp CSV
  tmp_dir <- tempfile(sprintf("ne_%s_", tolower(profile)))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  csv_path <- file.path(tmp_dir, sprintf("%s.csv", tolower(profile)))
  data.table::fwrite(df_raw, csv_path)
  csv_path
}

# Unzip override that supports list=TRUE and returns our CSV
mock_unzip_returning <- function(csv_path) {
  function(zipfile, files = NULL, list = FALSE, exdir = tempdir(), ...) {
    if (isTRUE(list)) return(basename(csv_path))
    target_name <- if (is.null(files) || length(files) == 0L) basename(csv_path) else files[[1L]]
    dest <- file.path(exdir, target_name)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(csv_path, dest, overwrite = TRUE)
    dest
  }
}
