#' Expert Query Extract Params
#'
#' Return the crosswalk for params for the specified Exper Query Extract.
#'
#' @param extract enter extract type. Options are: "actions", "act_docs",
#' "assessments", "aus", "au_mls", "catch_corr", "sources", and "tmdl".
#'
#' @return A df of the params for the selecte extract
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
#' Format user-supplied params in the Expert Query export functions.
#'
#' @param funct The Expert Query exported function to call parameters from
#'
#' @return A df of the params for the selecte extract
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
#' Format user-supplied params in the Expert Query export functions.
#'
#' @param funct The Expert Query exported function to call parameters from
#'
#' @return A df of the params for the selecte extract
#'

EQ_FormatParams <- function(.data) {
  # change language to character in value column

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
#' Compare user-supplied and default params in the Expert Query export functions to create df of
#' all params that should be used to build body for post request.
#'
#' @param funct The Expert Query exported function to call parameters from
#'
#' @return A df of the params for the selecte extract
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

  # return all params for use in body
  return(all.params)
}

#' Expert Query Create Body
#'
#' Compare body for count and query post rquests
#'
#' @param comp.params The EQ_CompareParams output from the query.
#' @param crosswalk The crosswalk between params and Expert Query field names.
#' @param extract The Expert Query Data profile type.
#'
#' @return A df of the params for the selecte extract
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
      dplyr::mutate(value = case_when(
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
      dplyr::select(col.name) %>%
      dplyr::mutate(col.name = paste0('"', col.name, '"'),
                    col.name = paste0(col.name, collapse = ',')) %>%
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
  #' Create header for POST
  #'
  #' @param key api key unique to each user.
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
  #' Create POST and get content from Expert Query Webservices
  #'
  #' @param headers Headers for POST request created in EQ_CreateHeader.
  #' @param body.list List of character strings for count and query POSTs created in EPA_CreateBody.
  #' @param extract The Expert Query Data profile type.
  #'
  #' @return A df of the query result or a printed message if the query rows exceed one million.

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
                  "Please refine the search or use the Expert Query National Extract."))
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

  #' Expert Query Domain Values
  #'
  #' Provides information on allowable values for a param by leveraging ATTAINS web services.
  #'
  #' @param query_param Character string. A param name from a rExpertQuery function. Default =
  #' NULL. When query_param = NULL, a df of the param names with allowable value information
  #' provided by ATTAINS web services is provided.
  #'
  #' @return A df allowable values for the selected query_param.
  #'
  EQ_DomainValues <- function(query_param = NULL) {

    base.url <- "https://attains.epa.gov/attains-public/api/domains"


    params.cw <- utils::read.csv(file = "inst/extdata/EQParamsCrosswalk.csv") %>%
      dplyr::select(param, eq_name) %>%


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
                  "Please refine the search or use the Expert Query National Extract."))
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
