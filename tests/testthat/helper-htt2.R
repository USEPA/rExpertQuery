httptest2::set_redactor(function(x) {
  # Request-side redaction (affects fixture filenames)
  if (inherits(x, "httr2_request")) {
    s3_pat <- "^cg-[0-9a-f\\-]+\\.s3-us-gov-west-1\\.amazonaws\\.com$"

    # Structured URL (typical for httr2)
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
      return(x)  # do NOT modify binary bodies
    }

    # Your existing body gsubs for text/JSON
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

  base_tt <- normalizePath(testthat::test_path("."), winslash = "/", mustWork = TRUE)  # .../tests/testthat
  proj    <- normalizePath(file.path(base_tt, "..", ".."), winslash = "/", mustWork = TRUE)
  target  <- file.path(base_tt, "htt2", subdir)

  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  message("Recording into: ", normalizePath(target, winslash = "/"))

  expr <- substitute(code)

  # Record from project root so httptest2 writes under proj/tests/testthat
  withr::with_dir(proj, {
    op <- options(httptest2.verbose = TRUE); on.exit(options(op), add = TRUE)
    httptest2::capture_requests(eval(expr, envir = parent.frame()))
  })

  # Possible roots where httptest2 may have written host dirs
  src_roots <- unique(c(
    file.path(proj, "tests", "testthat"),
    base_tt,
    file.path(base_tt, "tests", "testthat")
  ))

  # Known/likely host folders; include both redacted and unredacted names
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
        rel <- sub(paste0("^", gsub("\\\\", "/", normalizePath(src, winslash = "/", mustWork = TRUE)), "/?"),
                   "", gsub("\\\\", "/", normalizePath(f, winslash = "/", mustWork = TRUE)))
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

with_test_mocks <- function(subdir, code) {
  stopifnot(requireNamespace("testthat", quietly = TRUE))
  dir_rel <- file.path("htt2", subdir)
  if (!dir.exists(testthat::test_path(dir_rel))) {
    stop("Mock directory not found: ", testthat::test_path(dir_rel))
  }
  httptest2::with_test_mocks(dir_rel, code)  # pass 'code' unevaluated
}
