#' Provides allowable values for a param by leveraging ATTAINS web services.
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query/ATTAINS web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
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
EQ_DomainValues <- function(domain = NULL, api_key = NULL) {

  # local copy to avoid any name collision in pipelines
  dom <- domain

  # Load parameter crosswalk (fail fast if missing)
  cw_path <- system.file(
    "extdata", "EQParamsCrosswalk.csv",
    package = "rExpertQuery",
    mustWork = TRUE
  )
  param.cw <- utils::read.csv(cw_path, stringsAsFactors = FALSE)

  # base URL for ATTAINS (no trailing '?')
  base.url <- "https://api.epa.gov/attains/domains"

  # helper: perform request safely; return NULL on any HTTP error or exception
  safe_req_json <- function(req) {
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(NULL)
    status <- httr2::resp_status(resp)
    if (status < 200 || status >= 300) return(NULL)
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  }

  if (is.null(dom)) {
    # domain = NULL: list all domain names
    message("EQ_DomainValues: getting list of available domain names. Values in eq_param can be used as inputs in EQ_DomainValues.")

    # if no api_key, skip network and use internal fallback
    raw.data <- NULL
    if (!is.null(api_key)) {
      req <- httr2::request(base.url) |>
        httr2::req_url_query(api_key = api_key)
      raw.data <- safe_req_json(req)
    }

    # Normalize payload shape to a data.frame (the endpoint can return a list/vector)
    df <- NULL
    if (is.data.frame(raw.data)) {
      df <- raw.data
    } else if (is.list(raw.data) && "domain" %in% names(raw.data) && is.character(raw.data$domain)) {
      df <- tibble::tibble(domain = raw.data$domain)
    } else if (is.character(raw.data)) {
      df <- tibble::tibble(domain = raw.data)
    }

    if (!is.null(df) && "domain" %in% names(df) && nrow(df) > 0) {
      eq.params <- df |>
        dplyr::select(domain) |>
        dplyr::rename(attains_ws_name = domain) |>
        dplyr::left_join(param.cw, by = "attains_ws_name") |>
        dplyr::filter(!is.na(eq_name)) |>
        dplyr::transmute(
          eq_param = param,
          attains_ws_name = attains_ws_name,
          attains_ws_field = attains_ws_field
        ) |>
        dplyr::arrange(eq_param)

      rm(df)

      message("EQ_DomainValues: domain list retrieved from ATTAINS web services.")
      return(eq.params)
    } else {
      message("EQ_DomainValues: ATTAINS domain list unavailable; returning internal list (may be out of date).")
      eq_domain_values_null <- utils::getFromNamespace("eq_domain_values_null", "rExpertQuery")
      return(eq_domain_values_null)
    }

  } else {
    # domain != NULL: fetch specific domain values

    # validate user input against crosswalk
    if (!dom %in% param.cw[["param"]]) {
      stop("EQ_DomainValues: User supplied domain value is not valid. Check spelling and review function documentation to ensure the domain value entered is correct.")
    }

    # get the ATTAINS domain name to query (first match, single value)
    param.ws <- param.cw |>
      dplyr::filter(param == dom) |>
      dplyr::pull(attains_ws_name)
    param.ws <- param.ws[1]

    retain.cols <- c(
      "attains_ws_name",
      "name",
      "code",
      "context",
      "context2",
      "dateModified",
      "attains_ws_field",
      "eq_name",
      "eq_param"
    )

    # If no api_key, we can still try; but on any error, fall back
    req <- httr2::request(base.url) |>
      httr2::req_url_query(domainName = param.ws)
    if (!is.null(api_key)) req <- httr2::req_url_query(req, api_key = api_key)

    raw.data <- safe_req_json(req)

    # Proceed only if the payload has actual domain values (name/code present)
    got_values <- is.data.frame(raw.data) &&
      "domain" %in% names(raw.data) &&
      nrow(raw.data) > 0 &&
      any(c("name", "code") %in% names(raw.data))

    if (got_values) {
      eq.params <- raw.data |>
        dplyr::rename(attains_ws_name = domain) |>
        dplyr::left_join(param.cw, by = "attains_ws_name") |>
        dplyr::filter(param == dom) |>
        dplyr::rename(eq_param = param) |>
        dplyr::select(dplyr::all_of(retain.cols)) |>
        dplyr::arrange(eq_param) |>
        dplyr::distinct()

      message(paste0(
        "EQ_DomainValues: For ", dom, " the values in the '",
        eq.params[["attains_ws_field"]][1], "' column of the function output are the allowable values for rExpertQuery functions."
      ))
      message("EQ_DomainValues: domain list retrieved from ATTAINS web services.")
      return(eq.params)

    } else {
      # fallback to internal crosswalk-derived values (NOT the NULL-domain list)
      message("EQ_DomainValues: ATTAINS domain values unavailable; returning internal list (may be out of date).")

      eq_domain_values <- utils::getFromNamespace("eq_domain_values", "rExpertQuery")

      eq.params <- eq_domain_values |>
        dplyr::left_join(param.cw, by = c("attains_ws_name", "attains_ws_field"),
                         relationship = "many-to-many") |>
        dplyr::filter(param == dom) |>
        dplyr::rename(eq_param = param) |>
        dplyr::select(dplyr::all_of(retain.cols)) |>
        dplyr::arrange(eq_param) |>
        dplyr::distinct()

      message(paste0(
        "EQ_DomainValues: For ", dom, " the values in the '",
        eq.params[["attains_ws_field"]][1], "' column of the function output are the allowable values for rExpertQuery functions."
      ))
      return(eq.params)
    }
  }
}

#' Downloads/updates an internal copy of allowable domain values for EQ_DomainValues
#'
#' @param api_key Character string. Users must supply their unique api key to access Expert
#' Query/ATTAINS web services. To obtain an api, submit the form at:
#' https://owapps.epa.gov/expertquery/api-key-signup
#'
#' @return Returns a data frame of the allowable domain values for the "domain"
#' param of EQ_DomainValues.
#'
# base URL to query ATTAINS web services
EQ_UpdateInternalDomainValues <- function(api_key = NULL){

  # check for api key
  if (is.null(api_key)) {
    stop("EQ_DomainValues: An api key is required to access EQ/ATTAINS web services.")
  }

base.url <-  "https://api.epa.gov/attains/domains?"

# read in parameter crosswalk
param.cw <- utils::read.csv(system.file("extdata", "EQParamsCrosswalk.csv",
                                        package = "rExpertQuery"
))

  raw.data <- tryCatch(
    jsonlite::fromJSON(paste0(base.url, "&api_key=", api_key)),
    error = function(e) NULL)

  # update for domain = NULL
  eq_domain_values_null <- raw.data |>
      dplyr::select(domain) |>
      dplyr::rename(attains_ws_name = domain) |>
      dplyr::left_join(param.cw, by = "attains_ws_name") |>
      dplyr::filter(!is.na(eq_name)) |>
      dplyr::transmute(
        eq_param = param,
        attains_ws_name = attains_ws_name,
        attains_ws_field = attains_ws_field
      ) |>
      dplyr::arrange(eq_param)

  # update for other domains
  param.cw <- utils::read.csv(
    system.file("extdata", "EQParamsCrosswalk.csv", package = "rExpertQuery"),
    stringsAsFactors = FALSE
  )

  param.cw <- param.cw |>
    dplyr::select(attains_ws_name,
                  attains_ws_field) |>
    dplyr::distinct() |>
    dplyr::filter(attains_ws_name != "",
                  !is.na(attains_ws_name))

  attains_ws_name <- param.cw |>
    dplyr::select(attains_ws_name) |>
    dplyr::filter(attains_ws_name != "" &
                    !is.na(attains_ws_name)) |>
    dplyr::distinct() |>
    dplyr::pull()

  fetch_one <- function(param.ws) {
    url <- paste0(base.url, "domainName=", param.ws, "&api_key=", api_key)

    raw.data <- tryCatch(
      jsonlite::fromJSON(url),
      error = function(e) NULL)

    raw.data
  }

  eq_domain_values <- purrr::map_dfr(.x = attains_ws_name, .f = fetch_one)

  eq_domain_values <- eq_domain_values |>
    dplyr::left_join(param.cw, by = c("domain" = "attains_ws_name"),
                     relationship = "many-to-many") |>
    dplyr::rename(attains_ws_name = domain)

  usethis::use_data(
    eq_domain_values,
    eq_domain_values_null,
    internal = TRUE,
    overwrite = TRUE
  )
}
