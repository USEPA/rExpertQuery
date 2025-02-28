#' Expert Query Extract Params
#'
#' Return the crosswalk for params and filter names for POST request for the specified Expert Query
#' Extract.
#'
#' @param extract enter extract type. Options are: "actions", "act_docs",
#' "assessments", "aus", "au_mls", "catch_corr", "sources", and "tmdl".
#'
#' @return A df of the params for the selected extract.
#'
EQ_ExtractParams <- function(extract = NULL)  {

  # select filter column
  extract.filter <- dplyr::case_when(
    extract == "actions" ~ extract,
    extract == "act_docs" ~ "action_documents",
    extract == "assessments" ~ extract,
    extract == "aus" ~ "assessment_units",
    extract == "au_mls" ~ "assessment_units_mls",
    extract == "catch_corr" ~ "catchment_correspondence",
    extract == "sources" ~ extract,
    extract == "tmdl" ~ extract
  )


  # import crosswalk ref file
  params.cw <- utils::read.csv(file = "inst/extdata/EQParamsCrosswalk.csv") %>%
    dplyr::filter(.data[[extract.filter]] == "yes") %>%
    dplyr::select(param, eq_name)

  # return the crosswalk
  return(params.cw)
}

#' Expert Query Default Params
#'
#' Get default params from the rExpert Query export functions.
#'
#' @param func The Expert Query exported function to call parameters from
#'
#' @return A data frame of the default params for the selected function.
#'

EQ_DefaultParams <- function(func) {

# create df of function formals
params.df <- formals(func) %>%
  as.list() %>%
  tibble::enframe(name = "param", value = "value") %>%
  as.data.frame()

return(params.df)
}

#' Expert Query Format Params
#'
#' Format user-supplied or default params from rExpertQuery exported functions to transform all
#' param values to character strings.
#'
#' @param .data The data frame of params and their values. The value column may contain character,
#' numeric, or language values.
#'
#' @return A data frame of the params and their values. All values are character strings.
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
  }
  )

  params.df <- params.df %>%
    dplyr::mutate(value = as.character(value))

  return(params.df)
}

#' Expert Query Compare Params
#'
#' Compare user-supplied and default params in rExportQuery exported functions to create data frame
#'of all params that should be used to build the filters section of the POST request body.
#'
#' @param default The data frame of default params and their values. All values must be character
#' strings.
#' @param user The data frame of user-supplied params and their values. All values must be character
#' strings.
#'
#' @return A data frame of all params and values that should be used as filters in the body of  the
#' POST request.
#'

EQ_CompareParams <- function(default, user) {

  # filter out any default params that user entered a value for
    default.params <- default %>%
    dplyr::filter(!param %in% user$param)

  # combine user supplied and default params
  all.params <- user %>%
    dplyr::full_join(default.params, by = names(user))

  # remove intermediate objects
  rm(default.params, default, user)

  # return all params for use in filter for POST request body
  return(all.params)
}

#' Expert Query Create Body
#'
#' Create character strings to use as the body for POST requests to return counts and retrieve
#' data in exported rExportQuery functions.
#'
#' @param comp.params A data frame of the EQ_CompareParams output for the query.
#' @param crosswalk The crosswalk between param names and Expert Query field names for the POST
#' request. This is imported from an internal rExpertQuery reference file.
#' @param extract The Expert Query Data extract type.
#'
#' @return A list containing two character strings. The first character string is for the body
#' of the count POST request. The second character string is for the body of the data POST request.
#'

  EQ_CreateBody <- function(comp.params, crosswalk, extract) {

    # date params
    date.params <- c("report_cycle", "assess_date_end", "assess_date_start",
                     "mon_end_date", "mon_start_date", "comp_date_end", "comp_date_start",
                     "tmdl_date_end", "tmdl_date_start")

    # create param filters for POST
    params.body <- comp.params %>%
      dplyr::filter(!value %in% c("NULL", "latest"),
                    param != "api_key") %>%
      dplyr::mutate(value = dplyr::case_when(
        param == "report_cycle" & value == "any" ~ "-1",
        param == "region" & !is.null(value) ~  paste0("0", value),
        param %in% c("au_status", "delisted",
                     "pollutant_ind", "vis",
                     "in_meas", "indian_country") & !is.null(value) ~ substr(value, 1, 1),
        param == "use_support" & value == "Fully Supporting" ~ "F",
        param == "use_support" & value == "Not Supporting" ~ "N",
        param == "use_support" & value == "Insufficient Information" ~ "I",
        param == "use_support" & value == "Not Assessed" ~ "X",
        param %in% c("assess_date_end", "assess_date_start",
                      "mon_end_date", "mon_start_date") ~ format(as.Date(value, "%Y-%m-%d"),
                                                                 "%m-%d-%Y"),
        .default = as.character(value)
      )) %>%
      dplyr::left_join(crosswalk, by = dplyr::join_by(param)) %>%
      dplyr::mutate(value = gsub('c\\(|\\)|"', '', value)) %>%
      tidyr::separate_rows(value, sep = ',\\s*') %>%
      dplyr::mutate(value = paste0('"', value, '"')) %>%
      dplyr::group_by(eq_name) %>%
      dplyr::mutate(value = paste0(value, collapse = ",")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(value = dplyr::case_when(
        !param %in% date.params ~ paste0('"', eq_name, '":', "[", value, "]"),
        .default = paste0('"', eq_name, '":', value))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = paste0(value, collapse = ",")) %>%
      dplyr::select(value) %>%
      dplyr::distinct() %>%
      dplyr::pull()

    # setup body for finding row count of query
    count.setup <- paste0(
      '{"filters":{',
      params.body, '}}'
    )

    # select filter column
    extract.filter <- dplyr::case_when(
      extract == "actions" ~ extract,
      extract == "act_docs" ~ "action_documents",
      extract == "assessments" ~ extract,
      extract == "aus" ~ "assessment_units",
      extract == "au_mls" ~ "assessment_units_mls",
      extract == "catch_corr" ~ "catchment_correspondence",
      extract == "sources" ~ extract,
      extract == "tmdl" ~ extract
    )


    # create string of column names base on extract selection
    columns.string <- utils::read.csv(file = "inst/extdata/EQColumnsForPOST.csv") %>%
      dplyr::select(col.name, dplyr::all_of(extract.filter)) %>%
      dplyr::filter(!is.na(get(extract.filter))) %>%
      dplyr::arrange(get(extract.filter)) %>%
      dplyr::select(columns.string$col.name) %>%
      dplyr::mutate(col.name = paste0('"', columns.string$col.name, '"'),
                    col.name = paste0(columns.string$col.name, collapse = ',')) %>%
      dplyr::distinct() %>%
      dplyr::pull()

    #create column string for POST
    extract.cols <- paste0('"columns":[', columns.string, "]}")

    # set up body for POST including filters, options, and columns
    body.setup <- paste0(
      '{"filters":{',
      params.body, '},',
      '"options":{"format":"csv"},',
      extract.cols
    )

    post.bodies <- list(count.setup, body.setup)

    rm(comp.params, params.body, count.setup, body.setup)

   return(post.bodies)
  }


  #' Expert Query Create Header
  #'
  #' Create header for count and data POST requests.
  #'
  #' @param key Character string. The api key unique to the user.
  #'
  #' @return A character string for the POST header.
  #'
  EQ_CreateHeader <- function(key) { # create headers for POST

    headers.setup <- c(
      `X-Api-Key` = key,
      `Content-Type` = "application/json",
      Accept = "application/json"
    )

    rm(key)

    return(headers.setup)

  }

  #' Expert Query Create POST and Get Content
  #'
  #' Create POST request and get content from Expert Query web services via exported rExportQuery
  #' functions.
  #'
  #' @param headers Character string. Header for POST request created in EQ_CreateHeader.
  #' @param body.list List of character strings for count and query POSTs created in EQ_CreateBody.
  #' @param extract Character string. The Expert Query Data profile type.
  #'
  #' @return A data frame of the query result or a printed message if the query rows exceed one
  #' million.

  EQ_PostAndContent <- function(headers, body.list, extract) {

    base.url <- "https://api.epa.gov/expertquery/api/attains/"

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

    query.url <- paste0(base.url, extract.url.name)


    row.res <- httr::POST(url = paste0(query.url, "/count"),
                          httr::add_headers(.headers = headers),
                          body = body.list[[1]])

    row.n <- httr::content(row.res, as = "parse", encoding = "UTF-8")

    # stop function if row count exceeds one million
    if(isTRUE(row.n$count > row.n$maxCount)) {
      stop(paste0(function.url.name,
                  ": The current query exceeds the maximum query size of ",
                  format(row.n$maxCount, big.mark = ","), " rows.",
                  "Please refine the search or use EQ_NationalExtract to import",
                  " the Expert Query National Extract."))
    }

    # if row count is less than one million, print message with row count and continue
    if(isTRUE(row.n$count < row.n$maxCount)) {
      print(paste0(function.url.name,
                   ": The current query will return ",
                   format(row.n$count, big.mark = ","), " rows."))
    }

    # remove intermediate objects
    rm(row.res, row.n)

    query.res <- httr::POST(url = query.url,
                            httr::add_headers(.headers = headers),
                            body = body.list[[2]])

    query.df <- suppressWarnings(httr::content(query.res, as = "parsed", encoding = "UTF-8"))

    # remove intermediate objects
    rm(headers, base.url, extract.url.name, function.url.name, query.url, body.list)

    return(query.df)
  }


  #' Pipe operator
  #'
  #' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
  #'
  #' @name %>%
  #' @rdname pipe
  #' @keywords internal
  #' @importFrom magrittr %>%
  #' @usage lhs \%>\% rhs
  #' @export
  #' @param lhs A value or the magrittr placeholder.
  #' @param rhs A function call using the magrittr semantics.
  #' @return The result of calling `rhs(lhs)`.
  NULL
