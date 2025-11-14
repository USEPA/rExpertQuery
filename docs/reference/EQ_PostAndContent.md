# Create POST request and GET content from Expert Query via Expert Query web services (internal function)

Create POST request and GET content from Expert Query via Expert Query
web services (internal function)

## Usage

``` r
EQ_PostAndContent(headers, body.list, extract, max_retries = 3)
```

## Arguments

- headers:

  Character string. Header for POST request created in EQ_CreateHeader.

- body.list:

  List of character strings for count and query POSTs created in
  EQ_CreateBody.

- extract:

  Character string. The Expert Query Data profile type.

- max_retries:

  Integer. The number of retry attempts.

## Value

A data frame of the query result or a printed message if the query rows
exceed one million.
