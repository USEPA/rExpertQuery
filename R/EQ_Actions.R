#' Expert Query Actions
#'
#' Query ATTAINS Actions data via Expert Query web services and return as data frame.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param act_agency Character string. Denotes the agency that is establishing/issuing the action
#' associated with an Assessment. Options are "State", "Tribe", or "EPA". Default = NULL.
#' @param act_id Character string. Unique Identifier for the Action associated with an Assessment
#' that will be used to track the Action entered (such as the corresponding information and
#' associated documents) in ATTAINS, and its associated name. Default = NULL
#' @param act_name Character string. The name associated with the action (ex: name of TMDL REport,
#' 4B Report, Alternative Report, etc).
#' @param act_type Character string. Identifies the type of Action associated with an Action.
#' Options can be viewed with EQ_DomainValues(act_type).Default = NULL
#' @param au_name Character string. The name assigned to an Assessment Unit by the Organization.
#' Default = NULL.
#' @param auid Character string. A unique identifier assigned to an Assessment Unit by the
#' Organization. Default = NULL.
#' @param comp_date_end Character string. The ending date of the date range during which the
#' Action is planned to be completed. Usually this refers to the date that the TMDL Action date is
#' initially submitted to EPA. Format is "YYYY-MM-DD". Default = NULL.
#' @param comp_date_start Character string. The starting date of the date range during which the
#' Action is planned to be completed. Usually this refers to the date that the TMDL Action date is
#' initially submitted to EPA. Format is "YYYY-MM-DD". Default = NULL.
#' @param fisc_year_end Character string. The ending year for the date range of fiscal years in
#' which a particular action, program, or project was initiated or established. Format is "YYYY".
#' Default = NULL.
#' @param fisc_year_start  Character string. The starting year for the date range of fiscal years
#' in which a particular action, program, or project was initiated or established. Format is "YYYY".
#' Default = NULL.
#' @param in_meas Character string. EPA can determine whether the Action should count towards 303(d)
#' measures. Draft Actions get partial credit and finalized Actions get full cred. By default, all
#' measures count towards the measures unless EPA changes this flag for a specific action. Options
#' are "Yes" or "No". Default = NULL.
#' @param indian_country Character string. Indicates if the water is either wholly or partially in
#' Indian country. Options are "Yes" or "No". Default = NULL.
#' @param org_id Character string. A unique identifier assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_id"). Default = NULL.
#' @param org_name Character string. A unique name assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_name"). Default = NULL.
#' @param param_name The name of the characteristic being  monitored and assessed. Options can be
#' viewed with EQ_DomainValues("param_name"). Default = NULL.
#' @param param_group Character string. A collection of related Parameters. Options can be viewed
#' with EQ_DomainValues("param_attain"). Default = NULL.
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options. Default = NULL.
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param water_type Character string. An Assessment Unit must have at least one water type, and it
#' may have multiple water types. Options can be viewed with EQ_DomainValues("water_type"). Default
#' = NULL.
#'
#' @return A data frame of ATTAINS actions with the columns "objectId", "region", "state",
#' "organizationType", "organizationId", "organizationName", "waterType", "parameterGroup",
#' "parameter", "actionType", "actionId", "actionName", "actionAgency", "inIndianCountry",
#' "includeInMeasure", "completionDate", "assessmentUnitId", "assessmentUnitName",
#' "fiscalYearEstablished", "locationDescription", "waterSize", "waterSizeUnits", and
#' "planSummaryLink".
#'
#' @export
#'
EQ_Actions <- function(api_key = NULL, act_agency = NULL, act_id = NULL, act_name = NULL,
                       act_type = NULL, au_name = NULL, auid = NULL, comp_date_end = NULL,
                       comp_date_start = NULL, fisc_year_end = NULL, fisc_year_start = NULL,
                       in_meas = NULL, indian_country = NULL, org_id = NULL,
                       org_name = NULL, param_name = NULL, param_group = NULL,
                       region = NULL, statecode = NULL, water_type = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Actions: An api key is required to access EQ web services.")
  }

  # get param and filter crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "actions")

  # get default params from EQ_Actions
  default.params <- EQ_DefaultParams(EQ_Actions) %>%
    # format for building body
    EQ_FormatParams()

  # create data frame of user supplied params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build data frame of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "actions")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop and print message if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "actions")

  # remove intermediate objects
  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}
