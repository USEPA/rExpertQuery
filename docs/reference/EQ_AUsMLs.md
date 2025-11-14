# Query and return ATTAINS Assessment Units with Monitoring Locations data via Expert Query web services

Query and return ATTAINS Assessment Units with Monitoring Locations data
via Expert Query web services

## Usage

``` r
EQ_AUsMLs(
  api_key = NULL,
  au_name = NULL,
  au_status = "Active",
  auid = NULL,
  mon_loc_id = NULL,
  mon_loc_org = NULL,
  org_id = NULL,
  org_name = NULL,
  region = NULL,
  report_cycle = "latest",
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
  Organization.

- au_status:

  Character string. The current condition or status of an Assessment
  Unit. Options are "Active", "Historical" or "Retired". Default =
  "Active".

- auid:

  Character string. A unique identifier assigned to an Assessment Unit
  by the Organization. Default = NULL.

- mon_loc_id:

  Character string. Unique identifier for the monitoring location.

- mon_loc_org:

  Character string. The unique identifier assigned to the Organization
  that conducted the monitoring. For all possible options see the WQP
  domain list for Organizations here:
  https://cdx.epa.gov/wqx/download/DomainValues/Organization_CSV.zip.

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

A data frame of ATTAINS Assessment Units with Monitoring Locations with
the columns "objectId", "region", "state", "organizationType",
"organizationId", "organizationName", "waterType", "useClassName",
"monitoringLocationId", "monitoringLocationOrgId", "assessmentUnitId",
"assessmentUnitName", "assessmentUnitStatus", "reportingCycle",
"locationDescrption", "monitoringLocationDataLink", "sizeSource",
"sourceScale", "waterSize", and "waterSizeUnits".

## Examples

``` r
 if (FALSE) { # \dontrun{

# to run examples add your api key below
# to get an API key: https://owapps.epa.gov/expertquery/api-key-signup
testkey <- "YOURAPIKEY"

# Alaska Assessment Units with Monitoring Locations
AK_aus_mls <- EQ_AUsMLs(statecode = "AK",
                        api_key = testkey)

# Red Lake Band of Chippewa Indians (Minnesota) Assessment Units with Monitoring Locations
RedLake_aus_mls <- EQ_AUsMLs(org_id = "REDLAKE",
                            api_key = testkey)
              } # }
```
