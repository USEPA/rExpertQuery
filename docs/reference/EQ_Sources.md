# Query and return ATTAINS Sources data via Expert Query web services

Query and return ATTAINS Sources data via Expert Query web services

## Usage

``` r
EQ_Sources(
  api_key = NULL,
  au_name = NULL,
  auid = NULL,
  cause = NULL,
  confirmed = NULL,
  epa_ir_cat = NULL,
  org_id = NULL,
  org_name = NULL,
  overall_status = NULL,
  param_group = NULL,
  region = NULL,
  report_cycle = "latest",
  source = NULL,
  state_ir_cat = NULL,
  statecode = NULL,
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

- auid:

  Character string. A unique identifier assigned to an Assessment Unit
  by the Organization. Default = NULL.

- cause:

  Character string. A cause of impairment associated with a Probable
  Source. Options can be viewed with EQ_DomainValue("cause"). Default =
  NULL.

- confirmed:

  Character string. Indicator of whether the source has been confirmed.
  Options are "yes" or "no". Default = NULL.

- epa_ir_cat:

  The overall EPA Integrated Report Category for the Assessment Unit ID,
  calculated by ATTAINS. Options are "1", "2", "3", "4A", "4B", "4C",
  "5", "5A", and "5R". Default = NULL. For more information on how the
  categories are defined and calculated within ATTAINS see:
  https://www.epa.gov/sites/default/files/2018-09/documents/attains_calculations_of_epa_ir_categories_2018-08-31.pdf

- org_id:

  Character string. A unique identifier assigned to the Organization.
  Options can be viewed with EQ_DomainValues("org_id"). Default = NULL.

- org_name:

  Character string. A unique name assigned to the Organization. Options
  can be viewed with EQ_DomainValues("org_name"). Default = NULL.

- overall_status:

  Character string.The overall support status for the Assessment Unit
  ID, calculated by ATTAINS. Options are "Fully Supporting", "Not
  Supporting", "Not Assessed". Default = NULL.

- param_group:

  A collection of related Parameters. Options can be viewed with
  EQ_DomainValues("param_attain"). Default = NULL.

- region:

  Numeric (integer). Integer from 1 to 10 to identify the EPA region of
  interest. See
  https://www.epa.gov/aboutepa/regional-and-geographic-offices for
  options. Default = NULL.

- report_cycle:

  Character string. The Integrated Reporting cycle of the data. Format
  is "YYYY" or "latest", which will select the most recent available
  cycle. Default = "latest".

- source:

  Character string. Indicates the specific Source types associated with
  the Action or Assessment. Options can be viewed with
  EQ_DomainValue("source"). Default = NULL.

- state_ir_cat:

  Character string. Label of Organization-specific Integrated Reporting
  categories as defined by the Organization's Domain Administrator.
  Options can be viewed with EQ_DomainValues("state_ir_cat). Default =
  NULL.

- statecode:

  Character string. FIPS state alpha code that identifies a state (e.g.
  statecode = "DE" for Delaware). See
  https://www.waterqualitydata.us/Codes/statecode for options. Default =
  NULL.

- water_type:

  type Character string. An Assessment Unit must have at least one water
  type, and it may have multiple water types. Options can be viewed with
  EQ_DomainValues("water_type"). Default = NULL.

## Value

A data frame of ATTAINS Sources with the columns "objectId", "region",
"state", "organizationType", "organizationId", "organizationName",
"waterType", "assessmentUnitId", "assessmentUnitName", "reportingCycle",
"overallStatus", "epaIrCategory", "stateIrCategory", "parameterGroup",
"causeName", "sourceName", "confirmed", "cycleId",
"locationDescription", "waterSize", and "waterSizeUnits".

## Examples

``` r
 if (FALSE) { # \dontrun{

# to run examples add your api key below
# to get an API key: https://owapps.epa.gov/expertquery/api-key-signup
testkey <- "YOURAPIKEY"

# Mercury TMDLS from Louisiana
 LA_mercury_tmdls <- EQ_TMDLs(statecode = "LA",
                              pollutant = "MERCURY",
                              api_key = testkey)

# Query for sources records associated with parameter group "NUISANCER EXOTIC SPECIES"
Sources_exoticspecies_paramgroup <- EQ_Sources(param_group = "NUISANCE EXOTIC SPECIES",
                                               api_key = testkey)
              } # }
```
