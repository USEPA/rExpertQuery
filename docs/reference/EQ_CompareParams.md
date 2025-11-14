# Compare user-supplied and default params in rExportQuery functions to create data frame of all params for building the filters section of the POST request body (internal function)

Compare user-supplied and default params in rExportQuery functions to
create data frame of all params for building the filters section of the
POST request body (internal function)

## Usage

``` r
EQ_CompareParams(default, user)
```

## Arguments

- default:

  The data frame of default params and their values. All values must be
  character strings.

- user:

  The data frame of user-supplied params and their values. All values
  must be character strings.

## Value

A data frame of all params and values that should be used as filters in
the body of the POST request.
