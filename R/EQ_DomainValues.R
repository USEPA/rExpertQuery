#' Provides allowable values for a param by leveraging ATTAINS web services.
#'
#' @param domain Character string. Running this function without entering a value
#' for domain will return a list of all allowable domain values. Not all params
#' in rExpertQuery are limited to a set list of allowable domain values (ex: date
#' params).
#'
#' The rExpertQuery params that will return a list of ATTAINS allowable
#' domain values are: act_agency (Action Agency), act_status (Action Status),
#' act_type (Action Type), ad_param (Addressed Parameter), addressed parameter
#' group (Addressed Parameter Group), assess_basis (Assessment Basis), assess_methods
#' (Assessment Method), assess_types (Assessment Type), au_status (Assessment Unit
#' Status Indicator), cause (Cause Name), delist_reason (Delisting Reason), doc_type,
#' (Action Document Type), file_type (Action Document File Type), locat_type
#' (Location Type), org_id (Organization Identifier), org_name (Organization Name),
#' param_attain (Parameter Attainment), param_group (Parameter Group), param_name
#' (Parameter Name), param_state_ir_code (Parameter State Integrated Report Code),
#' param_status (Parameter Status), source_scale (Source Scale), source_type (Source
#' Type), statecode (State), use_name (Use Name), use_support (Use Support), and
#' water_type (Water Type).
#'
#' Default is NULL. When param = NULL, the df returned will list all domains for
#' which an allowable list of values can be returned from ATTAINS web services.
#' The "eq_param" column returns the names of the params as used in rExpertQuery
#' functions. Any of these can be used as values in the "domain" param in
#' EQ_DomainValues.
#'
#' @return If a domain value is provided, a df of allowable values for the
#' selected domain from the ATTAINS web services is returned. A printed message
#' describes which column contains the values which should be used in rExpertQuery
#' functions (typically "name" or "code"). If no domain is provided, the function
#' returns a df displaying the rExpertQuery params and the corresponding name
#' (attains_ws_name) and field (attains_ws_field) in the ATTAINS web services.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#' # Get all rExpertQuery params that can be used as inputs for EQ_DomainValues
#' all_rEQ_params <- EQ_DomainValues()
#'
#' # Get all allowable values for the rExpertQuery param "water_type"
#  # Allowable values are contained in the "name" column of the output df
#' all_water_types <- EQ_DomainValues("water_type")
#'
#' # Get all allowable values for the rExpertQuery param "use_name" and filter
#' # for those used by Oregon DEQ
#' ORDEQ_use_names <- EQ_DomainValues("use_name") |>
#' dplyr::filter(context == "OREGONDEQ")
#'               }
#'
#' @importFrom rlang .data
#'
EQ_DomainValues <- function(domain = NULL) {
  # base URL to query ATTAINS web services
  base.url <- "https://attains.epa.gov/attains-public/api/domains"

  # read in parameter crosswalk
  param.cw <- utils::read.csv(system.file("extdata", "EQParamsCrosswalk.csv",
    package = "rExpertQuery"
  ))

  # return list of all allowable domain values if no domain value is supplied
  if (is.null(domain)) {
    print(paste0(
      "EQ_DomainValues: getting list of available domain names. Values in the eq_param column can be used as inputs in EQ_DomainValues."
    ))

    raw.data <- jsonlite::fromJSON(base.url) |>
      dplyr::select(domain) |>
      dplyr::rename(attains_ws_name = domain)

    eq.params <- raw.data |>
      dplyr::left_join(param.cw, by = dplyr::join_by('attains_ws_name')) |>
      dplyr::filter(!is.na(.data[['eq_name']])) |>
      dplyr::select(param, attains_ws_name, attains_ws_field) |>
      dplyr::rename(eq_param = param) |>
      dplyr::arrange(eq_param)

    rm(base.url, raw.data)

    return(eq.params)
  }

  if (!is.null(domain)) {
    # check to make sure user supplied domain value is valid
    if (!domain %in% param.cw[['param']]) {
      stop("EQ_DomainValues: User supplied domain value is not valid. Check spelling and review
           function documentation to ensure the domain value entered is correct.")
    }

    # check to make sure user supplied domain value is valid
    if (domain %in% param.cw[['param']]) {
      # filter for domains which have values in web service
      param.ws <- param.cw |>
        dplyr::filter(.data[['attains_ws_name']] != "")

      # check to see if user supplied domain has values in web service
      if (!domain %in% param.ws[['param']]) {
        stop("EQ_DomainValues: User supplied domain value valid, but no list of allowable values is
              available. Review function documentation for more information on allowable values.")
      }

      # remove intermediate object
      rm(param.ws)

      # filter crosswalk by user supplied domain value
      param.filter <- param.cw |>
        dplyr::filter(.data[['param']] %in% domain) |>
        dplyr::select("param", "attains_ws_name", "attains_ws_field") |>
        dplyr::distinct()

      raw.data <- jsonlite::fromJSON(paste0(base.url, "?domainName=", param.filter[['attains_ws_name']]))

      print(paste0(
        "EQ_DomainValues: For ", domain, " the values in the '",
        param.filter[['attains_ws_field']], "' column of the function output are the ",
        "allowable values for rExpert Query functions."
      ))

      rm(param.filter, base.url, param.cw)

      return(raw.data)
    }
  }
}
