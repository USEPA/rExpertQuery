#' Provides allowable values for a param by leveraging ATTAINS web services.
#'
#' @param domain Character string. Running this function without entering a value for domain
#' will return a list of all allowable domain values.
#'
#' @return A df allowable values for the selected domain if a domain is provided. If no
#' domain is provided, the function returns a list of domains.
#'
#' @export
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
      "EQ_DomainValues: getting list of available domain names."
    ))

    raw.data <- jsonlite::fromJSON(base.url) %>%
      dplyr::select(domain) %>%
      dplyr::rename(attains_ws_name = domain)

    eq.params <- raw.data %>%
      dplyr::left_join(param.cw, by = dplyr::join_by(attains_ws_name)) %>%
      dplyr::filter(!is.na(eq_name)) %>%
      dplyr::select(param) %>%
      dplyr::rename(domain = param) %>%
      dplyr::arrange()

    rm(base.url, raw.data)

    return(eq.params)
  }

  if (!is.null(domain)) {
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
        dplyr::select("param", "attains_ws_name", "attains_ws_field") %>%
        dplyr::distinct()

      raw.data <- jsonlite::fromJSON(paste0(base.url, "?domainName=", param.filter$attains_ws_name))

      print(paste0(
        "EQ_DomainValues: For ", domain, " the values in the ",
        param.filter$attains_ws_field, " column of the function output are the ",
        "allowable values for rExpert Query functions."
      ))

      rm(param.filter, base.url, param.cw)

      return(raw.data)
    }
  }
}
