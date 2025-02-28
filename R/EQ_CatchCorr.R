#' Expert Query Catchment Correspondence
#'
#' Return catchment correspondence data from Expert Query.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#' @param au_name Character string. The name assigned to an Assessment Unit by the Organization.
#' Default = NULL.
#' @param auid Character string. A unique identifier assigned to an Assessment Unit by the
#' Organization. Default = NULL.
#' @param org_id Character string. A unique identifier assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_id"). Default = NULL.
#' @param org_name Character string. A unique name assigned to the Organization. Options can
#' be viewed with EQ_DomainValues("org_name"). Default = NULL.
#' @param region Numeric (integer). Integer from 1 to 10 to identify the EPA region of interest.
#' See https://www.epa.gov/aboutepa/regional-and-geographic-offices for options. Default = NULL.
#' @param report_cycle Character string. The Integrated Reporting cycle of the data. Format is
#' "YYYY" or "latest", which will select the most recent available cycle. Default = "latest".
#' @param statecode Character string. FIPS state alpha code that identifies a state (e.g.
#' statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' Default = NULL.
#'
#' @return A data frame of ATTAINS Catchment Correspondence with the columns "objectId", "region",
#' "state", "organizationType", "organizationId", "organizationName", "assesssmentUnitId",
#' "assessmentUnitName", "catchmentNhdPlusId", "reportingCycle", and "cycleId".
#'
#' @export
#'
EQ_CatchCorr <- function(api_key = NULL, au_name = NULL, auid = NULL,
                         org_id = NULL, org_name = NULL, region = NULL,
                         report_cycle = "latest", statecode = NULL)  {

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
