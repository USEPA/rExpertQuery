httptest2::set_redactor(function(x) {
  x |>
    # 1) Shorten S3 host: cg-<uuid>.s3-us-gov-west-1.amazonaws.com -> s3
    httptest2::gsub_response("https?://cg-[0-9a-f\\-]+\\.s3-us-gov-west-1\\.amazonaws\\.com", "s3") |>

    # 2) Collapse versioned dir: /national-downloads/<digits>/ -> /national/
    httptest2::gsub_response("/national-downloads/[0-9]+/", "/national/") |>

    # 3) Also shorten your API base if desired to keep other paths short
    httptest2::gsub_response("https://api\\.epa\\.gov/expertquery/api/attains/", "epa/")
})
