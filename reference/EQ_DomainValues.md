# Provides allowable values for a param by leveraging ATTAINS web services.

Provides allowable values for a param by leveraging ATTAINS web
services.

## Usage

``` r
EQ_DomainValues(domain = NULL, api_key = NULL)
```

## Arguments

- domain:

  Character string. Running this function without entering a value for
  domain will return a list of all allowable domain values. Not all
  params in rExpertQuery are limited to a set list of allowable domain
  values (ex: date params).

  The rExpertQuery params that will return a list of ATTAINS allowable
  domain values are: act_agency (Action Agency), act_status (Action
  Status), act_type (Action Type), ad_param (Addressed Parameter),
  addressed parameter group (Addressed Parameter Group), assess_basis
  (Assessment Basis), assess_methods (Assessment Method), assess_types
  (Assessment Type), au_status (Assessment Unit Status Indicator), cause
  (Cause Name), delist_reason (Delisting Reason), doc_type, (Action
  Document Type), file_type (Action Document File Type), locat_type
  (Location Type), org_id (Organization Identifier), org_name
  (Organization Name), param_attain (Parameter Attainment), param_group
  (Parameter Group), param_name (Parameter Name), param_state_ir_code
  (Parameter State Integrated Report Code), param_status (Parameter
  Status), source_scale (Source Scale), source_type (Source Type),
  statecode (State), use_name (Use Name), use_class (Use Class),
  use_support (Use Support), and water_type (Water Type).

  Default is NULL. When param = NULL, the df returned will list all
  domains for which an allowable list of values can be returned from
  ATTAINS web services. The "eq_param" column returns the names of the
  params as used in rExpertQuery functions. Any of these can be used as
  values in the "domain" param in EQ_DomainValues.

- api_key:

  Character string. Users must supply their unique api key to access
  Expert Query/ATTAINS web services. To obtain an api, submit the form
  at: https://owapps.epa.gov/expertquery/api-key-signup

## Value

If a domain value is provided, a df of allowable values for the selected
domain from the ATTAINS web services is returned. A printed message
describes which column contains the values which should be used in
rExpertQuery functions (typically "name" or "code"). If no domain is
provided, the function returns a df displaying the rExpertQuery params
and the corresponding name (attains_ws_name) and field
(attains_ws_field) in the ATTAINS web services.

## Examples

``` r
 if (FALSE) { # \dontrun{

# Get all rExpertQuery params that can be used as inputs for EQ_DomainValues
all_rEQ_params <- EQ_DomainValues()

# Get all allowable values for the rExpertQuery param "water_type"
all_water_types <- EQ_DomainValues("water_type")

# Get all allowable values for the rExpertQuery param "use_name" and filter
# for those used by Oregon DEQ
ORDEQ_use_names <- EQ_DomainValues("use_name") |>
dplyr::filter(context == "OREGONDEQ")
              } # }
```
