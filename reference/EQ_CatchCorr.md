# Query and return ATTAINS Catchment Correspondence data via Expert Query web services

\#' \* If using VPN, suggest signing out of VPN before running this
function as it can cause 403 error and prevent download of catchment
correspondence data.

## Usage

``` r
EQ_CatchCorr(
  api_key = NULL,
  au_name = NULL,
  auid = NULL,
  org_id = NULL,
  org_name = NULL,
  region = NULL,
  report_cycle = "latest",
  statecode = NULL
)
```

## Arguments

- api_key:

  Character string. Users must supply their unique api key to access
  Expert Query web services. To obtain an api, submit the form at:
  https://owapps.epa.gov/expertquery/api-key-signup

- au_name:

  Character string. The name assigned to an Assessment Unit by the
  Organization. Default = NULL.

- auid:

  Character string. A unique identifier assigned to an Assessment Unit
  by the Organization. Default = NULL.

- org_id:

  Character string. A unique identifier assigned to the Organization.
  Options can be viewed with EQ_DomainValues("org_id"). Default = NULL.

- org_name:

  Character string. A unique name assigned to the Organization. Options
  can be viewed with EQ_DomainValues("org_name"). Default = NULL.

- region:

  Numeric (integer). Integer from 1 to 10 to identify the EPA region of
  interest. See
  https://www.epa.gov/aboutepa/regional-and-geographic-offices for
  options. Default = NULL.

- report_cycle:

  Character string. The Integrated Reporting cycle of the data. Format
  is "YYYY" or "latest", which will select the most recent available
  cycle. Default = "latest".

- statecode:

  Character string. FIPS state alpha code that identifies a state (e.g.
  statecode = "DE" for Delaware). See
  https://www.waterqualitydata.us/Codes/statecode for options. Default =
  NULL.

## Value

A data frame of ATTAINS Catchment Correspondence with the columns
"objectId", "region", "state", "organizationType", "organizationId",
"organizationName", "assesssmentUnitId", "assessmentUnitName",
"catchmentNhdPlusId", "reportingCycle", and "cycleId".

## Details

\*\* Catchment Correspondence queries can generate large files. You must
have enough memory available for these functions to import them into R
successfully. Best practice is to make your initial query as specific as
possible to reduce size of file returned.

## Examples

``` r
 if (FALSE) { # \dontrun{

# to run examples add your api key below
# to get an API key: https://owapps.epa.gov/expertquery/api-key-signup
testkey <- "YOURAPIKEY"

# Catchment Correspondence for Illinois' Big Muddy River (Assessment Unit IL_N-99)
IL_ILN99 <- EQ_CatchCorr(statecode = "IL",
                         auid = "IL_N-99",
                         api_key = testkey)

# Catchment Correspondence for Washington, DC
DC_catchcorr <- EQ_AUsMLs(statecode = "DC",
                            api_key = testkey)
              } # }
```
