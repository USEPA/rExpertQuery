set_redactor(function (x) {
  gsub_response(x, "https://api.epa.gov/expertquery/api/attains/", "epa/")
})

