# Format user-supplied or default params from rExpertQuery functions to transform all param values to character strings (internal function)

Format user-supplied or default params from rExpertQuery functions to
transform all param values to character strings (internal function)

## Usage

``` r
EQ_FormatParams(.data)
```

## Arguments

- .data:

  The data frame of params and their values. The value column may
  contain character, numeric, or language values.

## Value

A data frame of the params and their values. All values are character
strings.
