library(rExpertQuery)

eq_key <- Sys.getenv("EQ_API_KEY")

cat("EQ_API_KEY present: ", nzchar(eq_key), "\n", sep = "")

if (!nzchar(eq_key)) {
  stop("EQ_API_KEY is missing or empty")
}

# Try a very simple request first
cat("Testing a simple API call...\n")

result <- tryCatch(
  {
    out <- EQ_DomainValues(
      api_key = eq_key
    )
    cat("Success: request completed\n")
    cat("Class of result: ", paste(class(out), collapse = ", "), "\n", sep = "")
    cat("Rows: ", if (!is.null(nrow(out))) nrow(out) else "unknown", "\n", sep = "")
    out
  },
  error = function(e) {
    cat("Request failed:\n")
    cat(conditionMessage(e), "\n")
    NULL
  }
)

if (is.null(result)) {
  stop("Diagnostic request failed")
}
