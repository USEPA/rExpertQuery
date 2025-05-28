#' Extract crosswalk for params and filter names for POST request for the specified query
#' (internal function)
#'
#' @param extract enter extract type. Options are: "actions", "act_docs",
#' "assessments", "aus", "au_mls", "catch_corr", "sources", and "tmdl".
#'
#' @return A df of the params for the selected extract.
#'
#' @keywords internal
#'
EQ_ExtractParams <- function(extract = NULL) {
  # select filter column
  extract.filter <- dplyr::case_when(
    extract == "actions" ~ extract,
    extract == "act_docs" ~ "action_documents",
    extract == "assessments" ~ extract,
    extract == "aus" ~ "assessment_units",
    extract == "au_mls" ~ "au_mls",
    extract == "catch_corr" ~ "catchment_correspondence",
    extract == "sources" ~ extract,
    extract == "tmdl" ~ extract
  )


  # import crosswalk ref file
  params.cw <- readr::read_csv(system.file("extdata", "EQParamsCrosswalk.csv",
                                           package = "rExpertQuery"
  ), show_col_types = FALSE) %>%
    dplyr::filter(.data[[extract.filter]] == "yes") %>%
    dplyr::select("param", "eq_name")

  # return the crosswalk
  return(params.cw)
}

#' Get default params from specified rExpertQuery function (internal function)
#'
#' @param func The Expert Query exported function to call parameters from
#'
#' @return A data frame of the default params for the selected function.
#'
#' @keywords internal
#'

EQ_DefaultParams <- function(func) {
  # create df of function formals
  params.df <- formals(func) %>%
    as.list() %>%
    tibble::enframe(name = "param", value = "value") %>%
    as.data.frame()

  return(params.df)
}

#' Format user-supplied or default params from rExpertQuery functions to transform all
#' param values to character strings (internal function)
#'
#' @param .data The data frame of params and their values. The value column may contain character,
#' numeric, or language values.
#'
#' @return A data frame of the params and their values. All values are character strings.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
#'

EQ_FormatParams <- function(.data) {
  # change language or numeric to character in value column
  params.df <- .data

  params.df$value <- sapply(params.df$value, function(x) {
    if (is.language(x)) {
      deparse(x)
    } else if (is.logical(x) || is.numeric(x)) {
      as.character(x)
    } else {
      x
    }
  })

  params.df <- params.df %>%
    dplyr::mutate(value = as.character(.data$value))

  return(params.df)
}


#' Compare user-supplied and default params in rExportQuery functions to create data frame
#' of all params for building the filters section of the POST request body (internal function)
#'
#' @param default The data frame of default params and their values. All values must be character
#' strings.
#' @param user The data frame of user-supplied params and their values. All values must be character
#' strings.
#'
#' @return A data frame of all params and values that should be used as filters in the body of  the
#' POST request.
#'
#' @keywords internal
#'

EQ_CompareParams <- function(default, user) {
  # filter out any default params that user entered a value for
  default.params <- default %>%
    dplyr::filter(!.data$param %in% user$param)

  # combine user supplied and default params
  all.params <- user %>%
    dplyr::full_join(default.params, by = names(user))

  # remove intermediate objects
  rm(default.params, default, user)

  # return all params for use in filter for POST request body
  return(all.params)
}


#' Create character strings to use as the body for POST requests to return counts and retrieve
#' data in rExportQuery functions (internal function)
#'
#' @param comp.params A data frame of the EQ_CompareParams output for the query.
#' @param crosswalk The crosswalk between param names and Expert Query field names for the POST
#' request. This is imported from an internal rExpertQuery reference file.
#' @param extract The Expert Query Data extract type.
#'
#' @return A list containing two character strings. The first character string is for the body
#' of the count POST request. The second character string is for the body of the data POST request.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
#'

EQ_CreateBody <- function(comp.params, crosswalk, extract) {
  # date params
  date.params <- c(
    "assess_date_end", "assess_date_start", "cd_cycle_end", "cd_cycle_start",
    "comp_date_end", "comp_date_start", "cycle_first_end", "cycle_first_start",
    "cycle_last_end", "cycle_last_start", "expect_attain_cycle_hi",
    "expect_attain_cycle_lo", "fisc_year_start", "fisc_year_end",
    "mon_end_date_hi", "mon_end_date_lo", "mon_start_date_hi", "mon_start_date_lo",
    "report_cycle", "seas_end_date_hi", "seas_end_date_lo", "seas_start_date_hi",
    "seas_start_date_lo", "tmdl_cycle_hi", "tmdl_cycle_lo", "tmdl_date_end",
    "tmdl_date_start"
  )

  # text string query params
  query.params <- c("doc_query")

  # create param filters for POST
  params.body <- comp.params %>%
    dplyr::filter(
      !.data$value %in% c("NULL", "latest"),
      .data$param != "api_key"
    ) %>%
    dplyr::mutate(value = dplyr::case_when(
      .data$param == "report_cycle" & value == "any" ~ "-1",
      .data$param == "region" & !is.null(value) & value != "10" ~ paste0("0", value),
      .data$param %in% c(
        "au_status", "delisted",
        "pollutant_ind", "vis",
        "in_meas", "indian_country"
      ) & !is.null(.data$value) ~ substr(.data$value, 1, 1),
      .data$param == "use_support" & .data$value == "Fully Supporting" ~ "F",
      .data$param == "use_support" & .data$value == "Not Supporting" ~ "N",
      .data$param == "use_support" & .data$value == "Insufficient Information" ~ "I",
      .data$param == "use_support" & .data$value == "Not Assessed" ~ "X",
      .data$param %in% c(
        "assess_date_end", "assess_date_start",
        "mon_end_date", "mon_start_date"
      ) ~ format(
        as.Date(.data$value, "%Y-%m-%d"),
        "%m-%d-%Y"
      ),
      .default = as.character(.data$value)
    )) %>%
    dplyr::left_join(crosswalk, by = dplyr::join_by("param")) %>%
    dplyr::mutate(value = gsub('c\\(|\\)|"', "", .data$value)) %>%
    tidyr::separate_rows(.data$value, sep = ",\\s*") %>%
    dplyr::mutate(value = paste0('"', .data$value, '"')) %>%
    dplyr::group_by(.data$eq_name) %>%
    dplyr::mutate(value = paste0(.data$value, collapse = ",")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(value = dplyr::case_when(
      !.data$param %in% date.params & !.data$param %in% query.params ~ paste0(
        '"', .data$eq_name,
        '":', "[",
        .data$value, "]"
      ),
      .default = paste0('"', .data$eq_name, '":', .data$value)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = paste0(.data$value, collapse = ",")) %>%
    dplyr::select("value") %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # setup body for finding row count of query
  count.setup <- paste0(
    '{"filters":{',
    params.body, "}}"
  )

  # select filter column
  extract.filter <- dplyr::case_when(
    extract == "actions" ~ extract,
    extract == "act_docs" ~ "action_documents",
    extract == "assessments" ~ extract,
    extract == "aus" ~ "assessment_units",
    extract == "au_mls" ~ "au_mls",
    extract == "catch_corr" ~ "catchment_correspondence",
    extract == "sources" ~ extract,
    extract == "tmdl" ~ extract
  )


  # create string of column names base on extract selection
  columns.string <- readr::read_csv(system.file("extdata", "EQColumnsForPOST.csv",
                                                package = "rExpertQuery"
  ), show_col_types = FALSE) %>%
    dplyr::select("col.name", dplyr::all_of(extract.filter)) %>%
    dplyr::filter(!is.na(get(extract.filter))) %>%
    dplyr::arrange(get(extract.filter)) %>%
    dplyr::select("col.name") %>%
    dplyr::mutate(
      col.name = paste0('"', .data$col.name, '"'),
      col.name = paste0(.data$col.name, collapse = ",")
    ) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # create column string for POST
  extract.cols <- paste0('"columns":[', columns.string, "]}")

  # set up body for POST including filters, options, and columns
  body.setup <- paste0(
    '{"filters":{',
    params.body, "},",
    '"options":{"format":"csv"},',
    extract.cols
  )

  post.bodies <- list(count.setup, body.setup)

  rm(comp.params, params.body, count.setup, body.setup)

  return(post.bodies)
}



#' Create header for count and data POST requests (internal function)
#'
#' @param key Character string. The api key unique to the user.
#'
#' @return A character string for the POST header.
#'
#' @keywords internal
#'
EQ_CreateHeader <- function(key) { # create headers for POST

  headers.setup <- c(
    `X-Api-Key` = key,
    Accept = "application/json"
  )

  rm(key)

  return(headers.setup)
}

#' Create POST request and GET content from Expert Query via Expert Query web services (internal
#' function)
#'
#' @param headers Character string. Header for POST request created in EQ_CreateHeader.
#' @param body.list List of character strings for count and query POSTs created in EQ_CreateBody.
#' @param extract Character string. The Expert Query Data profile type.
#'
#' @return A data frame of the query result or a printed message if the query rows exceed one
#' million.
#'
#' @keywords internal
#'

EQ_PostAndContent <- function(headers, body.list, extract) {
  # base url to build requests
  base.url <- "https://api.epa.gov/expertquery/api/attains/"

  # extract name to add to url for request
  extract.url.name <- dplyr::case_when(
    extract == "actions" ~ extract,
    extract == "act_docs" ~ "actionDocuments",
    extract == "assessments" ~ extract,
    extract == "aus" ~ "assessmentUnits",
    extract == "au_mls" ~ "assessmentUnitsMonitoringLocations",
    extract == "catch_corr" ~ "catchmentCorrespondence",
    extract == "sources" ~ extract,
    extract == "tmdl" ~ extract
  )

  # extract name to add to print messages for user
  function.url.name <- dplyr::case_when(
    extract == "actions" ~ "EQ_Actions",
    extract == "act_docs" ~ "EQ_ActionDocuments",
    extract == "assessments" ~ "EQ_Assessments",
    extract == "aus" ~ "EQ_AssessmentUnits",
    extract == "au_mls" ~ "EQ_AUsMLs",
    extract == "catch_corr" ~ "EQ_CatchCorr",
    extract == "sources" ~ "EQ_Sources",
    extract == "tmdl" ~ "EQ_TMDL"
  )

  # create url for query
  query.url <- paste0(base.url, extract.url.name)

  # request to find number of results
  row.res <- httr2::request(paste0(query.url, "/count")) %>%
    httr2::req_timeout(30) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(!!!headers) %>%
    httr2::req_body_raw(body.list[[1]],
                        type = "application/json"
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  # stop function if row count exceeds one million
  if (isTRUE(row.res$count > row.res$maxCount)) {
    stop(paste0(
      function.url.name,
      ": The current query exceeds the maximum query size of ",
      format(row.res$maxCount, big.mark = ","), " rows.",
      "Please refine the search or use EQ_NationalExtract to import",
      " the Expert Query National Extract."
    ))
  }

  # if row count is less than one million, print message with row count and continue
  if (isTRUE(row.res$count < row.res$maxCount)) {
    print(paste0(
      function.url.name,
      ": The current query will return ",
      format(row.res$count, big.mark = ","), " rows."
    ))
  }

  # remove intermediate objects
  rm(row.res)

  # request to return query results
  query.res <- httr2::request(query.url) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(!!!headers) %>%
    httr2::req_body_raw(body.list[[2]],
                        type = "application/json"
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    readr::read_csv()

  # remove intermediate objects
  rm(headers, base.url, extract.url.name, function.url.name, query.url, body.list)

  return(query.res)
}

#' Format Plan Summary Links as URLs
#'
#' @param .data Data frame to convert planSummaryLink to functional URL links for use in data tables.
#' @param url.col Column name containing string for formatting. Default is "planSummaryLink".
#'
#' @return The .data data frame with planSummaryLink entries formatted as URL links.
#'
#' @export
#'

EQ_FormatPlanLinks <- function(.data, url.col = "planSummaryLink") {

  .data <- .data %>%
    dplyr::mutate(!!url.col := paste0("<a href='",
                                      !!sym(url.col),
                                      "' target='_blank'>",
                                      !!sym(url.col),
                                      "</a>"))
  return(.data)
}
