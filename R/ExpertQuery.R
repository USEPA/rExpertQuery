#' Expert Query Actions
#'
#' Return actions data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param act_agency Character string. Denotes the agency that is establishing/issuing the action
#' associated with an Assessment. Options are "State", "Tribe", or "EPA". Default = NULL.
#' @param act_id
#' @param act_name
#' @param act_type Character string. Identifies the type of Action associated with an Action.
#' Options can be viewed with EQ_DomainValues(act_type).Default = NULL
#' @param au_name
#' @param auid
#' @param comp_date_end
#' @param comp_date_start
#' @param fisc_year_end
#' @param fisc_year_start
#' @param in_meas
#' @param indian_country
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param param_group
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param water_type
#'
#' @return A data frame of ATTAINS actions served via Expert Query webservices including
#' the columns "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "assessmentUnitId", "assessmentUnitName", "parameterGroup",
#' "locationDescription", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_Actions <- function(api_key = NULL, act_agency = NULL, act_id = NULL, act_name = NULL,
                       act_type = NULL, au_name = NULL, auid = NULL, comp_date_end = NULL,
                       comp_date_start = NULL, fisc_year_end = NULL, fisc_year_start = NULL,
                       in_meas = NULL, indian_country = NULL, obj_id = NULL, org_id = NULL,
                       org_name = NULL, org_type = NULL, param = NULL, param_group = NULL,
                       region = NULL, statecode = NULL, water_type = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Actions: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "actions")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_Actions) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "actions")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "actions")

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}

#' Expert Query Actions Documents
#'
#' Return actions documents data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param act_id
#' @param act_name
#' @param act_type Character string. Identifies the type of Action associated with an Action.
#' Options can be viewed with EQ_DomainValues(act_type).
#' @param comp_date_end
#' @param comp_date_start
#' @param doc_file_name
#' @param doc_key
#' @param doc_name
#' @param doc_query
#' @param doc_type
#' @param doc_url
#' @param file_type
#' @param fisc_year_start
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param tmdl_date_end
#' @param tmdl_date_start
#'
#' @return A data frame of ATTAINS actions documents served via Expert Query webservices including
#' the columns "objectId", "organizationName", "organizationType", "region", "state", "tmdlDate",'
#' "documentDesc", "documentFileName", "documentFileTypeName", "documentKey", "documentName",
#' and "actionDocumentType".
#'
#' @export
#'
EQ_ActionsDocuments <- function(api_key = NULL, act_id = NULL, act_name = NULL, act_type = NULL,
                                comp_date_end = NULL, comp_date_start = NULL, doc_file_name = NULL,
                                doc_key = NULL, doc_name = NULL, doc_query = NULL, doc_type = NULL,
                                doc_url = NULL, file_type = NULL, fisc_year_start = NULL,
                                obj_id = NULL, org_id = NULL, org_name = NULL, region = NULL,
                                statecode = NULL, tmdl_date_end = NULL, tmdl_date_start = NULL) {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_ActionsDocuments: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "act_docs")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_ActionsDocuments) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "act_docs")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "act_docs")

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}

#' Expert Query Assessments
#'
#' Return assessments data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param act_agency Character string. Denotes the agency that is establishing/issuing the action
#' associated with an Assessment. Options are "State", "Tribe", or "EPA". Default = NULL.
#' @param act_status Character string Status of the Action associated with an assessment. Options
#' are "Draft", "Submitted", or "Final". Default = NULL.
#' @param act_type Character string. Identifies the type of Action associated with an Action.
#' Options can be viewed with EQ_DomainValues("act_type"). Default = NULL.
#' @param act_id Character string. Unique Identifier for the Action associated with an Assessment
#' that will be used to track the Action entered (such as the corresponding information and
#' associated documents) in ATTAINS, and its associated name. Default = NULL.
#' @param alt_list_id Character string. Unique identifier for a list water, if different from the
#' Assessment Unit ID. Default = NULL.
#' @param assess_basis Character string. Code representing the basis for an Assessment; is it based
#' on monitored data, extrapolate data, or both. Options can be viewed with
#' EQ_DomainValues(assess_basis). Default = NULL.
#' @param assess_date_end Character string. Ending date for range of when Assessment was completed.
#' Format is "YYYY-MM-DD". Default = NULL.
#' @param assess_date_start Character string. Starting date for range of when Assessment was
#' completed. Format is "YYYY-MM-DD". Default = NULL.
#' @param assess_methods Character string. The name of the Assessment Method being used. Options
#' can be viewed with EQ_DomainValues"(assess_methods"). Default = NULL.
#' @param assess_types Character string. Code representing the type of Assessment that was
#' performed. Options can be viewe with EQ_DomainValues("assess_types")
#' @param au_name Character string. The name assigned to an Assessment Unit by the Organization.
#' Default = NULL.
#' @param au_status Character string. The current condition or status of an Assessment Unit.
#' Options are "Active", "Historical" or "Retired". Default = "Active".
#' @param auid Character string. A unique identifier assigned to an Assessment Unit by the
#' Organization. Default = NULL.
#' @param cd_cycle_end Character string. Ending year for cycle cor which Consent Decree actions are
#' due. Format is "YYYY". Default = NULL.
#' @param cd_cycle_start Character string. Starting year for cycle cor which Consent Decree actions
#' are due. Format is "YYYY". Default = NULL.
#' @param cwa Character string. CWA 303(d) priority for developing a TMDL. Options are "High",
#' "Medium", or "Low". Default = NULL.
#' @param cycle_first_end Character string. Ending date for the range of cycles where the Assessment
#' Unit and Cause were first included on the 303(d) list. Format is "YYYY".
#' @param cycle_first_start Character string. Starting date for the range of cycles where the
#' Assessment Unit and Cause were first included on the 303(d) list. Format is "YYYY".
#' @param cycle_first_end Character string. Ending date for the range of cycles where the Assessment
#' Unit and Cause were first included on the 303(d) list. Format is "YYYY".
#' @param cycle_first_start Character string. Starting date for the range of cycles where the
#' Assessment Unit and Cause were first included on the 303(d) list. Format is "YYYY".
#' @param delist_reason Character string. The specific reason or explanation for removing a
#' waterbody or segment from a list of impaired waters or areas of concern. Options can be viewed
#' with EQ_DomainValues("delist_reason"). Default = NULL.
#' @param delisted Character string. Indicates whether a waterbody or segment has been removed from
#' a list of impaired waters or areas of concern due to meeting the required water quality standards
#' or improvement targets. Options are "No" or "Yes". Default = NULL.
#' @param epa_ir_cat The overall EPA Integrated Report Category for the Assessment Unit ID,
#' calculated by ATTAINS. Options are "1", "2", "3", "4A", "4B", "4C", "5", "5A", and "5R". Default
#' = NULL.
#' @param expect_attain_cycle_hi Character string. Upper end of  range of Cycles (years) by which
#' the Assessment Unit is expected to attain its standards (use to indicate whether or not this
#' cause should be considered towards category 4B). Format is YYYY (ex: "2028"). Default = NULL.
#' @param expect_attain_cycle_lo Character string. Lower end of  range of Cycles (years) by which
#' the Assessment Unit is expected to attain its standards (use to indicate whether or not this
#' cause should be considered towards category 4B). Format is YYYY (ex: "2028"). Default = NULL.
#' @param last_cycle_end Character string. Ending year for the cycle the Assessment Unit was last
#' assessed, which can include any conclusions related to the Assessment Unit and can include
#' delisting decisions. This does not need to match the current Assessment Cycle. Format is
#' "YYYY". Default = NULL.
#' @param last_cycle_start Character string. Starting year for the cycle the Assessment Unit was
#' last assessed, which can include any conclusions related to the Assessment Unit and can include
#' delisting decisions. This does not need to match the current Assessment Cycle. Format is
#' "YYYY". Default = NULL.
#' @param mon_end_date_hi Character string. Ending date for the range of dates on which monitoring
#' ended. Format is "YYYY-MM-DD". Default = NULL.
#' @param mon_end_date_lo Character string. Starting date for the range of dates on which monitoring
#' ended. Format is "YYYY-MM-DD". Default = NULL.
#' @param mon_start_date_hi Character string. Ending date for the range of dates on which
#' monitoring began. Format is "YYYY-MM-DD". Default = NULL.
#' @param mon_start_date_lo Character string. Starting date for the range of dates on which
#' monitoring began. Format is "YYYY-MM-DD". Default = NULL.
#' @param org_id Character string. A unique identifier assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_id"). Default = NULL.
#' @param org_name Character string. A unique name assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_name"). Default = NULL.
#' @param overall_status Character string.The overall support status for the Assessment Unit ID,
#' calculated by ATTAINs. Options are "Fully Supporting", "Not Supporting", "Not Assessed". Default
#' = NULL.
#' @param param_attain The attainment status for the Parameter for a specific Use. Options can be
#' viewed with EQ_DomainValues("param_attain"). Default = NULL.
#' @param param_group A collection of related Parameters. Options can be viewed with
#' EQ_DomainValues("param_attain"). Default = NULL.
#' @param param_ir_cat The EPA Integrated Report Category for the Assessment Unit ID/Parameter
#' combination, calculated by ATTAINS. Options are "1", "2", "3", "4A", "4B", "4C", "5", "5A", and
#' "5R". Default = NULL.
#' @param param_name The name of the characteristic being  monitored and assessed. Options can be
#' viewed with EQ_DomainValues("param_name"). Default = NULL.
#' @param param_state_ir_cat Label of Organization-specific Integrated Reporting categories for the
#' Assessment Unit/Parameter combination as defined by the Organization's Domain Administrator.
#' Options can be viewed with EQ_DomainValues("param_state_ir_cat", org_id). Default = NULL
#' @param param_status Status for a Parameter, indicating whether this Parameter is a Cause,
#' Observed Effect, or provided for informational purposes as Meeting Criteria or Meeting Threshold
#' Parameter. Options can be viewed with EQ_DomainValues("param_status"). Default = NULL.
#' @param pollut_ind Flag indicating whether or not the Cause of an impairment is Pollutant. Options
#' are "Yes" or "No". Default = NULL.
#' @param region Character string. Value from 1 to 10 as character string to identify the EPA
#' region of interest. Default = NULL.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param report_cycle Character string. The Integrated Reporting cycle of the data. Format is
#' "YYYY" or "latest", which will select the most recent available cycle. Default = "latest".
#' @param seas_end_date_hi The ending date for the range of end dates that applies to a Waste
#' Load Allocation. Format is "YYYY-MM-DD". Default = NULL.
#' @param seas_end_date_lo The starting date for the range of end dates that applies to a Waste
#' Load Allocation. Format is "YYYY-MM-DD". Default = NULL.
#' @param seas_start_date_hi The ending date for the range of start dates that applies to a Waste
#' Load Allocation. Format is "YYYY-MM-DD". Default = NULL.
#' @param seas_start_date_lo The starting date for the range of start dates that applies to a Waste
#' Load Allocation. Format is "YYYY-MM-DD". Default = NULL.
#' @param state_ir_cat
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param tmdl_cycle_hi Character string. The ending date for the range of dates for cycle when
#' the jurisdiction anticipates submitting the TMDL for EPA approval. Format is "YYYY". Default =
#' NULL.
#' @param tmdl_cycle_hi Character string. The starting date for the range of dates for cycle when
#' the jurisdiction anticipates submitting the TMDL for EPA approval. Format is "YYYY". Default =
#' NULL.
#' @param use_class Character string. The Use Class assigned to an Assessment Unit. Options can be
#' viewed with EQ_DomainValues("use_class"). Default = NULL.
#' @param use_group This represents a collection of related Uses
#' @param use_ir_cat
#' @param use_name
#' @param use_state_ir_cat
#' @param use_suppor"
#' @param vis
#' @param water_size
#' @param water_type
#'
#' @return A data frame of ATTAINS assessments served via Expert Query webservices including the
#' columns "objectId", "region", "state", "organizationType", "organizationId", "organizationName",
#' "waterType", "reportingCycle", "cycleLastAssessed", "assessmentUnitId", "assessmentUnitName",
#' "assessmentUnitStatus", "overallStatus", "epaIrCategory", "stateIrCategory", "useGroup",
#' "useName", "useClassName", "useSupport", "useIrCategory", "useStateIrCategory",
#' "monitoringStartDate", "monitoringEndDate", "assessmentDate", "assessmentTypes",
#' "assessmentMethods", "assessmentBasis", "parameterGroup", "parameterName", "parameterStatus",
#' "parameterAttainment", "parameterIrCategory", "parameterStateIrCategory", "delisted",
#' "delistedReason", "pollutantIndicator", "cycleFirstListed", "alternateListingIdentifier",
#' "vision303dPriority", "cwa303dPriorityRanking", "cycleScheduledForTmdl", "cycleExpectedToAttain",
#' "consentDecreeCycle", "cycleId", "seasonStartDate", "seasonEndDate", "associatedActionId",
#' "associatedActionName", "associatedActionType", "associatedActionStatus",
#' "associatedActionAgency", "locationDescription", "sizeSource", "sourceScale", "waterSize", and
#' "waterSizeUnits".
#'
#' @export
#'
EQ_Assessments <- function(api_key = NULL, act_agency = NULL, act_status = NULL, act_type = NULL,
                           act_id = NULL, alt_list_id = NULL, assess_basis = NULL,
                           assess_date_end = NULL, assess_date_start = NULL, assess_methods = NULL,
                           assess_types = NULL, au_name = NULL, au_status = "Active", auid = NULL,
                           cd_cycle_end = NULL, cd_cycle_start = NULL, cwa = NULL,
                           cycle_first_end = NULL, cycle_first_start = NULL, cycle_last_end = NULL,
                           cycle_last_start = NULL, delist_reason = NULL, delisted = NULL,
                           epa_ir_cat = NULL, expect_attain_cycle_hi = NULL,
                           expect_attain_cycle_lo = NULL, last_cycle_end = NULL,
                           last_cycle_start = NULL, mon_end_date_hi = NULL,
                           mon_end_date_lo = NULL, mon_start_date_hi = NULL,
                           mon_start_date_lo = NULL, org_id = NULL, org_name = NULL,
                           org_type = NULL, overall_status = NULL, param_attain = NULL,
                           param_group = NULL, param_ir_cat = NULL, param_name = NULL,
                           param_state_ir_cat = NULL, param_status = NULL,
                           pollut_ind = NULL, region = NULL, report_cycle = "latest",
                           seas_end_date_hi = NULL, seas_end_date_lo = NULL,
                           seas_start_date_hi = NULL, seas_start_date_lo = NULL,
                           state_ir_cat = NULL,
                           statecode = NULL, tmdl_cycle_hi = NULL, tmdl_cycle_lo = NULL,
                           use_class = NULL, use_group = NULL,
                           use_ir_cat = NULL, use_name = NULL, use_state_ir_cat = NULL,
                           use_support = NULL, vis = NULL, water_size = NULL, water_type = NULL) {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Assessments: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "assessments")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_Assessments) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "assessments")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "assessments")

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}

#' Expert Query Assessment Units
#'
#' Return assessment units data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param au_name
#' @param au_status
#' @param au_id
#' @param cycle_id
#' @param loc_txt
#' @param loc_type
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param report_cyle
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param use_class
#' @param water_type
#'
#' @return A data frame of ATTAINS assessment units served via Expert Query webservices including
#' the columns "region", "state", "organizationType", "organizationId", "organizationName",
#' "waterType", "locationTypeCode", "locationText", "useClassName", "assessmentUnitId",
#' "assessmentUnitName", "assessmentUnitStatus", "reportingCycle", "cycleId",
#' "locationDescription", "sizeSource", "sourceScale", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_AssessmentUnits <- function(api_key = NULL, au_name = NULL, au_status = "A", auid = NULL,
                               cycle_id = NULL, loc_txt = NULL, loc_type = NULL, region = NULL,
                               report_cycle = NULL, statecode = NULL, use_class = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_AssessmentUnits: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "aus")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_AssessmentUnits) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "aus")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "aus")

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}

#' Expert Query Assessment Units with Monitoring Locations
#'
#' Return assessment units with monitoring locations data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param au_name
#' @param au_status
#' @param auid
#' @param cycle_id
#' @param mon_loc_id
#' @param mon_loc_org
#' @param nhd_id
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param report_cycle
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param use_class
#' @param water_type
#'
#' @return A data frame of ATTAINS assessment units with monitoring locations served via Expert
#' Query webservices including the columns "objectId", "region", "state", "organizationType",
#' "organizationId", "organizationName", "waterType", "useClassName", "monitoringLocationId",
#' "monitoringLocationOrgId", "assessmentUnitId", "assessmentUnitName", "assessmentUnitStatus",
#' "reportingCycle", "locationDescription", "monitoringLocationDataLink", "sizeSource",
#' "sourceScale", "waterSize", an "waterSizeUnits".
#'
#' @export
#'
EQ_AUsMLs <- function(api_key = NULL, au_name = NULL, au_status = "A", auid = NULL,
                      cycle_id = NULL, mon_loc_id = NULL, mon_loc_org = NULL,  nhd_id = NULL,
                      obj_id = NULL, org_id = NULL, org_name = NULL, org_type = NULL,
                      region = NULL, report_cycle = "latest", statecode = NULL, use_class = NULL,
                      water_type = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_AUsMLs: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "au_mls")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_AUsMLs) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "au_mls")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "au_mls")
  # should rows where ml is NA be filtered out?

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}

#' Expert Query Catchment Correspondence
#'
#' Return catchment correspondence data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param au_name
#' @param auid
#' @param cycle_id
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param report_cycle
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#'
#' @return A data frame of ATTAINS catchment correspondence served via Expert Query webservices
#' including the columns "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "locationTypeCode", "locationText", "useClassName",
#' "assessmentUnitId", "assessmentUnitName", "assessmentUnitStatus", "reportingCycle", "cycleId",
#' "locationDescription", "sizeSource", "sourceScale", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_CatchCorr <- function(api_key = NULL, au_name = NULL, auid = NULL, cycle_id = NULL,
                         obj_id = NULL, org_id = NULL, org_name = NULL, org_type = NULL,
                         region = NULL, report_cycle = "latest", statecode = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_CatchCorr: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "catch_corr")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_CatchCorr) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "catch_corr")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "catch_corr")
  # should rows where ml is NA be filtered out?

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}

#' Expert Query Sources
#'
#' Return sources data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param au_name
#' @param auid
#' @param cause
#' @param confirmed Character string. Indicator of whether the source has been confirmed. Options
#' are "yes" or "no". Default = NULL.
#' @param cycle_id
#' @param epa_ir_cat
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param overall_status
#' @param param_group
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param report_cycle
#' @param source
#' @param state_ir_cat
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param water_type
#'
#' @return A data frame of ATTAINS sources served via Expert Query webservices including
#' the columns "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "assessmentUnitId", "assessmentUnitName", "reportingCycle",
#' "overallStatus", "epaIrCategory", "stateIrCategory", "parameterGroup", "causeName",
#' "sourceName", "confirmed", "cycleId", "locationDescription", "waterSize", and "waterSizeUnits".
#'
#' @export
#'
EQ_Sources <- function(api_key = NULL, au_name = NULL, auid = NULL, cause = NULL,
                       confirmed = NULL, cycle_id = NULL, epa_ir_cat = NULL, obj_id = NULL,
                       org_id = NULL, org_name = NULL, org_type = NULL,
                       overall_status = NULL, param_group = NULL, region = NULL,
                       report_cycle = "latest", source = NULL, state_ir_cat = NULL,
                       statecode = NULL, water_type = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_Sources: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "sources")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_Sources) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "sources")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "sources")
  # should rows where ml is NA be filtered out?

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}

#' Expert Query TMDLs
#'
#' Return tmdl data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param act_agency Character string. Denotes the agency that is establishing/issuing the action
#' associated with an Assessment. Options are "state", "tribe", or "epa". Default = NULL.
#' @param act_id
#' @param act_name
#' @param au_name
#' @param auid
#' @param comp_date_end
#' @param comp_date_start
#' @param fisc_year_end
#' @param fisc_year_start
#' @param in_meas
#' @param indian_country
#' @param obj_id
#' @param org_id
#' @param org_name
#' @param org_type
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options.
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param tmdl_date_start
#' @param water_type
#' @param ad_param
#' @param ad_param_group
#' @param mos_exp
#' @param mos_imp
#' @param npdes_id
#' @param other_id
#' @param pollutant
#' @param poll_group
#' @param source_type
#'
#' @return A data frame of ATTAINS tmdls served via Expert Query webservices including
#' the columns "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "pollutantGroup", "pollutant", "addressedParameterGroup",
#' "addressedParameter", "sourceType", "npdesIdentifier", "otherIdentifier", "actionId",
#' "actionName", "actionAgency", "inIndianCountry", "explicitMarginOfSafety",
#' "implicitMarginOfSafety", "includeInMeasure", "completionDate", "tmdlDate",
#' "fiscalYearEstablished", "assessmentUnitId", "assessmentUnitName", "loadAllocation",
#' "loadAllocationUnits", "locationDescription", "tmdlEndpoint", "waterSize", "waterSizeUnits",
#' "wasteLoadAllocation", and "planSummaryLink".
#'
#' @export
#'
EQ_TMDLs <- function(api_key = NULL, act_agency = NULL, act_id = NULL, act_name = NULL,
                     au_name = NULL, auid = NULL, comp_date_end = NULL, comp_date_start = NULL,
                     fisc_year_end = NULL, fisc_year_start = NULL, in_meas = NULL,
                     indian_country = NULL,  obj_id = NULL, org_id = NULL, org_name = NULL,
                     org_type = NULL, region = NULL, statecode = NULL, tmdl_date_end = NULL,
                     tmdl_date_start = NULL, water_type = NULL, ad_param = NULL,
                     ad_param_group = NULL, mos_exp = NULL, mos_imp = NULL, npdes_id = NULL,
                     other_id = NULL, pollutant = NULL, poll_group = NULL, source_type = NULL)  {

  # check for api key
  if(is.null(api_key)) {
    stop("EQ_TMDLs: An api key is required to access EQ web services.")
  }

  # get param crosswalk for building query
  params.cw <- EQ_ExtractParams(extract = "tmdl")

  # get default params from EQ_Assessments
  default.params <- EQ_DefaultParams(EQ_TMDLs) %>%
    # format for building body
    EQ_FormatParams()

  # create df of user entered params
  user.params <- as.list(match.call()[-1]) %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame() %>%
    # format for building body
    EQ_FormatParams()

  # compare default and user params to build df of all params and values for body
  params.df <- EQ_CompareParams(default = default.params, user = user.params)

  # remove intermediate objects
  rm(user.params, default.params)

  # create post bodies
  post.bodies <- EQ_CreateBody(comp.params = params.df, crosswalk = params.cw, extract = "tmdl")

  # create post headers
  post.headers <- EQ_CreateHeader(key = api_key)

  # query EQ (check number of rows before download, stop if it exceeds max rows)
  query.df <- EQ_PostAndContent(headers = post.headers,
                                body.list = post.bodies,
                                extract = "tmdl")
  # should rows where ml is NA be filtered out?

  rm(params.cw, params.df, post.bodies, post.headers)

  return(query.df)
}
