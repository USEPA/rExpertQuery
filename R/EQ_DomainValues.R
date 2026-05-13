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

    raw.data <- suppressMessages(suppressWarnings(tryCatch(
      jsonlite::fromJSON(base.url),
      error = function(e) NULL)))

      if (!is.null(raw.data) && "domain" %in% names(raw.data) && nrow(raw.data) > 0) {
        # remote path (as you requested)
        eq.params <- raw.data |>
          dplyr::select(domain) |>
          dplyr::rename(attains_ws_name = .data$domain) |>
          dplyr::left_join(param.cw, by = dplyr::join_by(attains_ws_name)) |>
          dplyr::filter(!is.na(.data$eq_name)) |>
          dplyr::transmute(
            eq_param = .data$param,
            attains_ws_name = .data$attains_ws_name,
            attains_ws_field = .data$attains_ws_field
          ) |>
          dplyr::arrange(.data$eq_param)

        message("EQ_DomainValues: domain list retrieved from ATTAINS web services.")
        return(eq.params)
      } else {
        # fallback to packaged crosswalk
        message("EQ_DomainValues: ATTAINS domain list unavailable; returning internal list (may be out of date).")

        e <- new.env(parent = emptyenv())

        dv_filepath <- system.file("extdata", "DomainValuesNull.rda",
                                   package = "rExpertQuery", mustWork = TRUE)

        load(dv_filepath, envir = e)
        eq.params <- e[["domain_values"]]
        return(eq.params)
      }
  }

  if (!is.null(domain)) {
    # check to make sure user supplied domain value is valid
    if (!domain %in% param.cw[['param']]) {
      stop("EQ_DomainValues: User supplied domain value is not valid. Check spelling and review
           function documentation to ensure the domain value entered is correct.")
    }

    # check to make sure user supplied domain value is valid
    if (domain %in% param.cw[['param']]) {
      # check to see if user supplied domain has values in web service

      # get param name for web services
      param.ws <- param.cw |>
        dplyr::filter(.data$param == .env$domain) |>
        dplyr::pull(.data$attains_ws_name)

      # cols to retain
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

      # filter for domains which have values in web service
      raw.data <- suppressMessages(suppressWarnings(tryCatch(
        jsonlite::fromJSON(paste0(base.url, "?domainName=", param.ws)),
        error = function(e) NULL)))

      if (!is.null(raw.data) && "domain" %in% names(raw.data) && nrow(raw.data) > 0) {

        # remote path
        eq.params <- raw.data |>
          dplyr::rename(attains_ws_name = .data$domain) |>
          dplyr::left_join(param.cw, by = "attains_ws_name",
                           relationship = "many-to-many") |>
          dplyr::filter(.data$param == .env$domain) |>
          dplyr::rename(eq_param = .data$param) |>
          dplyr::select(dplyr::all_of(retain.cols)) |>
          dplyr::arrange(.data$eq_param) |>
          dplyr::distinct()

        print(paste0(
          "EQ_DomainValues: For ", domain, " the values in the '",
          eq.params[['attains_ws_field']][1], "' column of the function output are the ",
          "allowable values for rExpert Query functions."
        ))

        message("EQ_DomainValues: domain list retrieved from ATTAINS web services.")
        return(eq.params)
      } else {
        # fallback to packaged crosswalk
        message("EQ_DomainValues: ATTAINS domain list unavailable; returning internal list (may be out of date).")

        e <- new.env(parent = emptyenv())

        dv_filepath <- system.file("extdata", "DomainValues.rda",
                                   package = "rExpertQuery", mustWork = TRUE)

        load(dv_filepath, envir = e)
        domain_values <- e[["domain_values"]]

      # filter crosswalk by user supplied domain value
      eq.params <- domain_values |>
                  dplyr::left_join(param.cw, by = c("attains_ws_name",
                                                    "attains_ws_field"),
                           relationship = "many-to-many") |>
        dplyr::filter(.data$param == .env$domain) |>
        dplyr::rename(eq_param = .data$param) |>
        dplyr::select(dplyr::all_of(retain.cols)) |>
        dplyr::arrange(.data$eq_param) |>
        dplyr::distinct()

      print(paste0(
        "EQ_DomainValues: For ", domain, " the values in the '",
        eq.params[['attains_ws_field']][1], "' column of the function output are the ",
        "allowable values for rExpert Query functions."
      ))

      # remove intermediate objects
      objs <- c("param.filter", "base.url", "param.cw")
      rm(
        list = objs[sapply(objs, exists, envir = .GlobalEnv, inherits = FALSE)],
        envir = .GlobalEnv
      )
    }

      return(eq.params)
    }
  }
}

#' Downloads/updates an internal copy of allowable domain values for EQ_DomainValues when domain = NULL
#'
#' @return Returns a data frame of the allowable domain values for the "domain"
#' param of EQ_DomainValues.
#'
# base URL to query ATTAINS web services
EQ_UpdateInternalDomainValuesNull <- function(){
base.url <- "https://attains.epa.gov/attains-public/api/domains"

# read in parameter crosswalk
param.cw <- utils::read.csv(system.file("extdata", "EQParamsCrosswalk.csv",
                                        package = "rExpertQuery"
))

  raw.data <- tryCatch(
    jsonlite::fromJSON(base.url),
    error = function(e) NULL)

    eq.params <- raw.data |>
      dplyr::select(domain) |>
      dplyr::rename(attains_ws_name = .data$domain) |>
      dplyr::left_join(param.cw, by = "attains_ws_name") |>
      dplyr::filter(!is.na(.data$eq_name)) |>
      dplyr::transmute(
        eq_param = .data$param,
        attains_ws_name = .data$attains_ws_name,
        attains_ws_field = .data$attains_ws_field
      ) |>
      dplyr::arrange(.data$eq_param)

    save(eq.params, file = file.path("inst", "extdata", "DomainValuesNull.rda"), compress = "xz")
}

#' Downloads/updates an internal copy of allowable domain values for EQ_DomainValues when domain != NULL
#'
#' @return Returns a data frame of the allowable domain values all allowable values of domain excluding NULL.
#' param of EQ_DomainValues.
#'
# base URL to query ATTAINS web services
EQ_UpdateInternalDomainValues<- function() {
  base.url <- "https://attains.epa.gov/attains-public/api/domains"

  param.cw <- utils::read.csv(
    system.file("extdata", "EQParamsCrosswalk.csv", package = "rExpertQuery"),
    stringsAsFactors = FALSE
  )

  param.cw <- param.cw |>
    dplyr::select(attains_ws_name,
                  attains_ws_field) |>
    dplyr::distinct()

  attains_ws_name <- param.cw |>
    dplyr::select(.data$attains_ws_name) |>
    dplyr::filter(.data$attains_ws_name != "" &
                    !is.na(.data$attains_ws_name)) |>
    dplyr::distinct() |>
    dplyr::pull()

  fetch_one <- function(param.ws) {
    url <- paste0(base.url, "?domainName=", param.ws)

    raw.data <- tryCatch(
      jsonlite::fromJSON(url),
      error = function(e) NULL)

    raw.data
  }

  domain_values <- purrr::map_dfr(.x = attains_ws_name, .f = fetch_one)

  domain_values <- domain_values |>
    dplyr::left_join(param.cw, by = c("domain" = "attains_ws_name"),
                     relationship = "many-to-many") |>
    dplyr::rename(attains_ws_name = domain)

  save(domain_values, file = file.path("inst", "extdata", "DomainValues.rda"), compress = "xz")
}
