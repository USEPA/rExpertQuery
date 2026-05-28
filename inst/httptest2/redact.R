# inst/httptest2/redact.R
# Always use fully qualified calls to httptest2 functions here.
httptest2::set_redactor(function(x) {
  x |>
    # Example replacements; adjust to your API/paths
    httptest2::gsub_response("https://api.epa.gov/expertquery/api/attains/", "epa/") |>
    httptest2::gsub_response("https://api.epa.gov/expertquery/api/", "epa/") |>
    # Redact Authorization headers, query params, etc., as needed
    httptest2::redact_authorization()
})
