# set redactor to shortern paths for mocks

httptest2::set_redactor(function(x) {
  x |>
    # shorten S3 host
    httptest2::gsub_response("https?://cg-[0-9a-f\\-]+\\.s3-us-gov-west-1\\.amazonaws\\.com", "s3") |>

    # collapse versioned dir
    httptest2::gsub_response("/national-downloads/[0-9]+/", "/national/") |>

    # shorten API base to keep other paths short
    httptest2::gsub_response("https://api\\.epa\\.gov/expertquery/api/attains/", "epa/")
})
