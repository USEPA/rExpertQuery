# Query and return ATTAINS Actions Documents data via Expert Query web services

Query and return ATTAINS Actions Documents data via Expert Query web
services

## Usage

``` r
EQ_ActionsDocuments(
  api_key = NULL,
  act_id = NULL,
  act_name = NULL,
  act_type = NULL,
  comp_date_end = NULL,
  comp_date_start = NULL,
  doc_file_name = NULL,
  doc_name = NULL,
  doc_type = NULL,
  doc_query = NULL,
  org_id = NULL,
  org_name = NULL,
  region = NULL,
  statecode = NULL,
  tmdl_date_end = NULL,
  tmdl_date_start = NULL
)
```

## Arguments

- api_key:

  Character string. Users must supply their unique api key to access
  Expert Query web services. To obtain an api, submit the form at:
  https://owapps.epa.gov/expertquery/api-key-signup

- act_id:

  Character string. Unique Identifier for the Action associated with an
  Assessment that will be used to track the Action entered (such as the
  corresponding information and associated documents) in ATTAINS, and
  its associated name. Default = NULL

- act_name:

  Character string. The name associated with the action.Default = NULL.

- act_type:

  Character string. Identifies the type of Action associated with an
  Action. Options can be viewed with EQ_DomainValues(act_type).

- comp_date_end:

  Character string. The ending date of the date range during which the
  Action is planned to be completed. Usually this refers to the date
  that the TMDL Action date is initially submitted to EPA. Format is
  "YYYY-MM-DD". Default = NULL.

- comp_date_start:

  Character string. The starting date of the date range during which the
  Action is planned to be completed. Usually this refers to the date
  that the TMDL Action date is initially submitted to EPA. Format is
  "YYYY-MM-DD". Default = NULL.

- doc_file_name:

  Character string. The document file name of the Actions Document.
  Default = NULL.

- doc_name:

  Character string. The document name of the Actions Document. Default =
  NULL.

- doc_type:

  Character string. The document type of the Actions Document. Default =
  NULL.

- doc_query:

  Character string. Document text search terms.

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

- statecode:

  Character string. FIPS state alpha code that identifies a state (e.g.
  statecode = "DE" for Delaware). See
  https://www.waterqualitydata.us/Codes/statecode for options.

- tmdl_date_end:

  Character string. This should correspond to the ending date of the
  date range that EPA approved the official final TMDL submitted (such
  as the date on the approval letter). Format is "YYYY-MM-DD". Default =
  NULL.

- tmdl_date_start:

  Character string. This should correspond to the starting date of the
  date range that EPA approved the official final TMDL submitted (such
  as the date on the approval letter). Format is "YYYY-MM-DD". Default =
  NULL.

## Value

A data frame of ATTAINS Actions Documents with the columns "objectId",
"actionDocumentUrl", "actionId", "actionName", "actionType",
"completionDate", "organizationId", "organizationName",
"organizationType", "region", "state", "tmdlDate", "documentDesc",
"documentFileName", "documentFileTypeName", "documentKey",
"documentName", and " actionDocumentType".

## Examples

``` r
if (FALSE) { # \dontrun{

# to run examples add your api key below
# to get an API key: https://owapps.epa.gov/expertquery/api-key-signup
testkey <- "YOURAPIKEY"

# Actions Documents from California established between fiscal years 2018 and 2020
CA_actions_docs <- EQ_ActionsDocuments(
  statecode = "CA",
  comp_date_start = "2000-01-01",
  comp_date_end = "2010-12-31",
  api_key = testkey
)

# Actions Documents from Minnesota and Wisconsin containing "nutrient"
R4_nutrient_actions_docs <- EQ_ActionsDocuments(
  region = 4,
  doc_query = "nutrient",
  api_key = testkey
)
} # }
```
