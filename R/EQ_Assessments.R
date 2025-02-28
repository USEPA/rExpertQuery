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
#' are "Draft", ""EPA Final Action", "EPA Review", "Modify", "Public Review", "State Final Action",
#' or "Withdrawn". Default = NULL.
#' @param act_type Character string. Identifies the type of Action associated with an Action.
#' Options are FILL IN FROM EQ.
#' Default = NULL.
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
#' @param cycle_last_end Character string. Ending year for the cycle the Assessment Unit was last
#' assessed, which can include any conclusions related to the Assessment Unit and can include
#' delisting decisions. This does not need to match the current Assessment Cycle. Format is
#' "YYYY". Default = NULL.
#' @param cycle_last_start Character string. Starting year for the cycle the Assessment Unit was
#' last assessed, which can include any conclusions related to the Assessment Unit and can include
#' delisting decisions. This does not need to match the current Assessment Cycle. Format is
#' "YYYY". Default = NULL.
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
#' @param state_ir_cat Character string. Label of Organization-specific Integrated Reporting
#' categories as defined by the Organization's Domain Administrator. Options can be viewed with
#' EQ_DomainValues("state_ir_cat). Default = NULL.
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param tmdl_cycle_hi Character string. The ending date for the range of dates for cycle when
#' the jurisdiction anticipates submitting the TMDL for EPA approval. Format is "YYYY". Default =
#' NULL.
#' @param tmdl_cycle_lo Character string. The starting date for the range of dates for cycle when
#' the jurisdiction anticipates submitting the TMDL for EPA approval. Format is "YYYY". Default =
#' NULL.
#' @param use_class Character string. The Use Class assigned to an Assessment Unit. Options can be
#' viewed with EQ_DomainValues("use_class"). Default = NULL.
#' @param use_group Character string. This represents a collection of related Uses. Options are
#' "DRINKINGWATER_USE", "ECOLOGICAL_USE", "FISHCONSUMPTION_USE", "RECREATION_USE", and
#' "OTHER_USE". Default = NULL.
#' @param use_ir_cat Character string. The EPA Integrated Report Category for the Assessment Unit
#' ID/Use combination, calculated by ATTAINS. Options are "1", "2", "3", "4A", "4B", "4C", "5",
#' "5A", and "5R". Default = NULL.
#' @param use_name Character string. Name of the designated Use. Options can be viewed with
#' EQ_DomainValues("use_name"). Default = NULL.
#' @param use_state_ir_cat Character string. Label of Organization-specific Integrated Reporting
#' categories for the Assessment Unit/Use combination as defined by the Organization's Domain
#' Administrator.
#' @param use_support Character string. The decision as to whether the Use Name (designated use) is
#' Fully Supporting, Not Supporting, Insufficient Information, or Not Assessed. Options are
#' "Fully Supporting","Not Supporting", "Insufficent Information" or "Not Assessed."
#' @param vis Character string. Indicates if the water has been identified as a priority water by
#' the state under the 303(d) Vision. Options are "Yes" or "No". Default = NULL.
#' @param water_type Character string. An Assessment Unit must have at least one water type, and it
#' may have multiple water types. Options can be viewed with EQ_DomainValues("water_type"). Default
#' = NULL.
#'
#' @return A data frame of ATTAINS Assessments with the columns "objectId", "region", "state",
#' "organizationType", "organizationId", "organizationName", "waterType", "reportingCycle",
#' "cycleLastAssessed", "assessmentUnitId", "assessmentUnitName", "assessmentUnitStatus",
#' "overallStatus", "epaIrCategory", "stateIrCategory", "useGroup", "useName", "useClassName",
#' "useSupport", "useIrCategory", "useStateIrCategory", "monitoringStartDate", "monitoringEndDate",
#' "assessmentDate", "assessmentTypes", "assessmentMethods", "assessmentBasis", "parameterGroup",
#' "parameterName", "parameterStatus", "parameterAttainment", "parameterIrCategory",
#' "parameterStateIrCategory", "delisted", "delistedReason", "pollutantIndicator",
#' "cycleFirstListed", "alternateListingIdentifier", "vision303dPriority", "cwa303dPriorityRanking",
#' "cycleScheduledForTmdl", "cycleExpectedToAttain", "consentDecreeCycle", "cycleId",
#' "seasonStartDate", "seasonEndDate", "associatedActionId", "associatedActionName",
#' "associatedActionType", "associatedActionStatus", "associatedActionAgency",
#' "locationDescription", "sizeSource", "sourceScale", "waterSize", and "waterSizeUnits".
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
                           expect_attain_cycle_lo = NULL, mon_end_date_hi = NULL,
                           mon_end_date_lo = NULL, mon_start_date_hi = NULL,
                           mon_start_date_lo = NULL, org_id = NULL, org_name = NULL,
                           overall_status = NULL, param_attain = NULL, param_group = NULL,
                           param_ir_cat = NULL, param_name = NULL, param_state_ir_cat = NULL,
                           param_status = NULL, pollut_ind = NULL, region = NULL,
                           report_cycle = "latest", seas_end_date_hi = NULL,
                           seas_end_date_lo = NULL, seas_start_date_hi = NULL,
                           seas_start_date_lo = NULL, state_ir_cat = NULL, statecode = NULL,
                           tmdl_cycle_hi = NULL, tmdl_cycle_lo = NULL, use_class = NULL,
                           use_group = NULL, use_ir_cat = NULL, use_name = NULL,
                           use_state_ir_cat = NULL, use_support = NULL, vis = NULL,
                           water_type = NULL) {

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
