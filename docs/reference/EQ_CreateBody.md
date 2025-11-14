# Create character strings to use as the body for POST requests to return counts and retrieve data in rExportQuery functions (internal function)

Create character strings to use as the body for POST requests to return
counts and retrieve data in rExportQuery functions (internal function)

## Usage

``` r
EQ_CreateBody(comp.params, crosswalk, extract)
```

## Arguments

- comp.params:

  A data frame of the EQ_CompareParams output for the query.

- crosswalk:

  The crosswalk between param names and Expert Query field names for the
  POST request. This is imported from an internal rExpertQuery reference
  file.

- extract:

  The Expert Query Data extract type.

## Value

A list containing two character strings. The first character string is
for the body of the count POST request. The second character string is
for the body of the data POST request.
