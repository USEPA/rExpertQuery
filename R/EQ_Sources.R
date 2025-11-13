#' Query and return ATTAINS Sources data via Expert Query web services
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param au_name Character string. The name assigned to an Assessment Unit by the Organization.
#' Default = NULL.
#' @param auid Character string. A unique identifier assigned to an Assessment Unit by the
#' Organization. Default = NULL.
#' @param cause Character string. A cause of impairment associated with a Probable Source. Options
#' can be viewed with EQ_DomainValue("cause"). Default = NULL.
#' @param confirmed Character string. Indicator of whether the source has been confirmed. Options
#' are "yes" or "no". Default = NULL.
#' @param epa_ir_cat The overall EPA Integrated Report Category for the Assessment Unit ID,
#' calculated by ATTAINS. Options are "1", "2", "3", "4A", "4B", "4C", "5", "5A", and "5R". Default
#' = NULL. For more information on how the categories are defined and calculated within ATTAINS
#' see:
#' https://www.epa.gov/sites/default/files/2018-09/documents/attains_calculations_of_epa_ir_categories_2018-08-31.pdf
#' @param org_id Character string. A unique identifier assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_id"). Default = NULL.
#' @param org_name Character string. A unique name assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_name"). Default = NULL.
#' @param overall_status Character string.The overall support status for the Assessment Unit ID,
#' calculated by ATTAINS. Options are "Fully Supporting", "Not Supporting", "Not Assessed". Default
#' = NULL.
#' @param param_group A collection of related Parameters. Options can be viewed with
#' EQ_DomainValues("param_attain"). Default = NULL.
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options. Default = NULL.
#' @param report_cycle Character string. The Integrated Reporting cycle of the data. Format is
#' "YYYY" or "latest", which will select the most recent available cycle. Default = "latest".
#' @param source Character string. Indicates the specific Source types associated with the Action
#' or Assessment. Options can be viewed with EQ_DomainValue("source"). Default = NULL.
#' @param state_ir_cat Character string. Label of Organization-specific Integrated Reporting
#' categories as defined by the Organization's Domain Administrator. Options can be viewed with
#' EQ_DomainValues("state_ir_cat). Default = NULL.
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' Default = NULL.
#' @param water_type type Character string. An Assessment Unit must have at least one water type, and it
#' may have multiple water types. Options can be viewed with EQ_DomainValues("water_type"). Default
#' = NULL.
#'
#' @return A data frame of ATTAINS Sources with the columns "objectId", "region", "state",
#' "organizationType", "organizationId", "organizationName", "waterType", "assessmentUnitId",
#' "assessmentUnitName", "reportingCycle", "overallStatus", "epaIrCategory", "stateIrCategory",
#' "parameterGroup", "causeName", "sourceName", "confirmed", "cycleId", "locationDescription",
#' "waterSize", and "waterSizeUnits".
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#' # to run examples add your api key below
#' # to get an API key: https://owapps.epa.gov/expertquery/api-key-signup
#' testkey <- "YOURAPIKEY"
#'
#' # Mercury TMDLS from Louisiana
#'  LA_mercury_tmdls <- EQ_TMDLs(statecode = "LA",
#'                               pollutant = "MERCURY",
#'                               api_key = testkey)
#'
#' # Query for sources records associated with parameter group "NUISANCER EXOTIC SPECIES"
#' Sources_exoticspecies_paramgroup <- EQ_Sources(param_group = "NUISANCE EXOTIC SPECIES",
#'                                                api_key = testkey)
#'               }
#'
EQ_Sources <- function(api_key = NULL, au_name = NULL, auid = NULL, cause = NULL,
                       confirmed = NULL, epa_ir_cat = NULL,
                       org_id = NULL, org_name = NULL,
                       overall_status = NULL, param_group = NULL, region = NULL,
                       report_cycle = "latest", source = NULL, state_ir_cat = NULL,
                       statecode = NULL, water_type = NULL) {
  # check for api key
  if (is.null(api_key)) {
    stop("EQ_Sources: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "sources")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_Sources) %>%
    # format for building body
    EQ_FormatParams()

  #create df of user entered params
  get.user <- as.list(match.call()[-1])

  # evaluate user params
  user.evals <- lapply(get.user, eval, envir = parent.frame())

  # create df and format params
  user.params <- as.data.frame(t(user.evals), stringsAsFactors = FALSE) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "param") %>%
    EQ_FormatParams()

  # remove intermediate objects
  rm(get.user, user.evals)

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "sources")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(
    headers = post.headers,
    body.list = post.bodies,
    extract = "sources"
  )
  # should rows where ml is NA be filtered out?

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}
