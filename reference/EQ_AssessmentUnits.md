# Query and return ATTAINS Assessment Units data via Expert Query web services

Query and return ATTAINS Assessment Units data via Expert Query web
services

## Usage

``` r
EQ_AssessmentUnits(
  api_key = NULL,
  au_name = NULL,
  au_status = "Active",
  auid = NULL,
  region = NULL,
  report_cycle = NULL,
  statecode = NULL,
  use_class = NULL,
  water_type = NULL
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

- au_status:

  Character string. The current condition or status of an Assessment
  Unit. Options are "Active", "Historical" or "Retired". Default =
  "Active".

- auid:

  Character string. A unique identifier assigned to an Assessment Unit
  by the Organization. Default = NULL.

- region:

  Numeric (integer). Integer from 1 to 10 to identify the EPA region of
  interest. See
  https://www.epa.gov/aboutepa/regional-and-geographic-offices for
  options.

- report_cycle:

  Character string. The Integrated Reporting cycle of the data. Format
  is "YYYY" or "latest", which will select the most recent available
  cycle. Default = "latest".

- statecode:

  Character string. FIPS state alpha code that identifies a state (e.g.
  statecode = "DE" for Delaware). See
  https://www.waterqualitydata.us/Codes/statecode for options.

- use_class:

  Character string. The Use Class assigned to an Assessment Unit.
  Options are "CULTURAL_USE", "DRINKING_WATER_USE", "ECOLOGICAL USE",
  "FISHCONSUMPTION_USE", "OTHER_USE", and "RECREATION_USE". Default =
  NULL.

- water_type:

  Character string. An Assessment Unit must have at least one water
  type, and it may have multiple water types. Options can be viewed with
  EQ_DomainValues("water_type"). Default = NULL.

## Value

A data frame of ATTAINS Assessment Units with the columns "region",
"state", "organizationType", "organizationId", "organizationName",
"waterType", "locationTypeCode", "locationText", "useClassName",
"assessmentUnitId", "assessmentUnitName", "assessmentUnitStatus",
"reportingCycle", "cycleId", "locationDescription", "sizeSource",
"sourceScale", "waterSize", and "waterSizeUnits".

## Examples

``` r
 if (FALSE) { # \dontrun{

# to run examples add your api key below
# to get an API key: https://owapps.epa.gov/expertquery/api-key-signup
testkey <- "YOURAPIKEY"

# Active estaury Assessment Units from Maryland
MD_estuary <- EQ_AssessmentUnits(statecode = "MD",
                                water_type = "ESTUARY",
                                api_key = testkey)

# Active Assessment Units from Rhode Island from the 2016 report cycle
RI_aus_2016 <- EQ_Assessments(statecode = "RI",
                          report_cycle = 2016,
                          api_key = testkey)
              } # }
```
