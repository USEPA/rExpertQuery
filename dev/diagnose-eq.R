library(rExpertQuery)

eq_key <- Sys.getenv("EQ_API_KEY")
cat("EQ_API_KEY present: ", nzchar(eq_key), "\n", sep = "")
stopifnot(nzchar(eq_key))

test_call <- function(label, expr) {
  cat("\n--- Testing ", label, " ---\n", sep = "")
  out <- tryCatch(expr, error = function(e) e)
  if (inherits(out, "error")) {
    cat("FAILED: ", conditionMessage(out), "\n", sep = "")
  } else {
    cat("SUCCESS\n")
    cat("Class: ", paste(class(out), collapse = ", "), "\n", sep = "")
    if (!is.null(nrow(out))) cat("Rows: ", nrow(out), "\n", sep = "")
  }
  invisible(out)
}

test_call("EQ_Actions", EQ_Actions(
  statecode = "RI",
  fisc_year_start = 2014,
  fisc_year_end = 2020,
  api_key = eq_key
))

test_call("EQ_ActionsDocuments", EQ_ActionsDocuments(
  state = "OR",
  comp_date_start = "01-01-2018",
  comp_date_end = "12-31-2020",
  api_key = eq_key
))
