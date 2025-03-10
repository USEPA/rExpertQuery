#' Expert Query Domain Values
#'
#' Provides information on allowable values for a param by leveraging ATTAINS web services.
#'
#' HRM Note 3/10/25 - Crosswalk between ATTAINS and EQ domains not yet complete. This function
#' does not yet work for all EQ params as a result.
#'
#' @param domain Character string. Running this function without entering a value for domain
#' will return a list of all allowable domain values.
#'
#' @return A df allowable values for the selected query_param.
#'
#'@importFrom rlang .data
#'
EQ_DomainValues <- function(domain = NULL) {
  # base URL to query ATTAINS web services
  base.url <- "https://attains.epa.gov/attains-public/api/domains"

  # return list of all allowable domain values if no domain value is supplied
  if (is.null(domain)) {
    print(paste0(
      "EQ_DomainValues: getting list of available domain names."
    ))

    raw.data <- jsonlite::fromJSON(base.url) %>%
      dplyr::select(domain)

    rm(base.url)

    return(raw.data)
  }

  if (!is.null(domain)) {
    # get file path for parameter crosswalk
    param.cw <- system.file("extdata", "EQParamsCrosswalk.csv", package = "rExpertQuery")

    # read in parameter crosswalk
    param.cw <- utils::read.csv(param.cw)

    # check to make sure user supplied domain value is valid
    if (!domain %in% param.cw$param) {
      stop("EQ_DomainValues: User supplied domain value is not valid. Check spelling and review
           function documentation to ensure the domain value entered is correct.")
    }

    # check to make sure user supplied domain value is valid
    if (domain %in% param.cw$param) {
      # filter for domains which have values in web service
      param.ws <- param.cw %>%
        dplyr::filter(.data$attains_ws_name != "")

      # check to see if user supplied domain has values in web service
      if (!domain %in% param.ws$param) {
        stop("EQ_DomainValues: User supplied domain value valid, but no list of allowable values is
              available. Review function documentation for more information on allowable values.")
      }

      # remove intermediate object
      rm(param.ws)

      # filter crosswalk by user supplied domain value
      param.filter <- param.cw %>%
        dplyr::filter(.data$param %in% domain) %>%
        dplyr::select('param', 'attains_ws_name', 'attains_ws_field') %>%
        dplyr::distinct()

      raw.data <- jsonlite::fromJSON(paste0(base.url, "?domainName=", param.filter$attains_ws_name)) %>%
        dplyr::select(dplyr::all_of(param.cw2$attains_ws_field)) %>%
        dplyr::rename(domainValue = 1)

      rm(param.filter, base.url, param.cw)

      return(raw.data)
    }
  }
}
