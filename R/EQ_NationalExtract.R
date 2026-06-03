#' Returns user-specified Expert Query National Extracts of ATTAINS data for use cases
#' where nationwide data is desired.
#'
#' This function is also useful in situations where the the desired query would
#' yield more than 1 million rows as the national extracts can be sorted and filtered after import.
#' The National Extracts are large files and can take several minutes to download and import.
#'
#' National extracts can and more information about Expert Query can be found here:
#' https://owapps.epa.gov/expertquery/national-downloads
#'
#' * If using VPN, suggest signing out of VPN before running this function as it can cause
#' 403 error and prevent download of national extracts.
#'
#' ** The National Extracts are large files. You must have enough memory available in order for
#' these functions to import them into R successfully.
#'
#' @param extract Character argument. Specifies which Expert Query National Extract should be
#' imported. Options are "actions" (Actions), "assessments" (Assessments), "au" (Assessment Units),
#' "au_mls" (Assessment Units with Monitoring Locations), "catch_corr" (Catchment Correspondence),
#' "sources" (Sources), and "tmdl" (TMDLs). There is no national extract option available for
#' Actions Documents. The default is NULL which means no extract will be returned.
#' @param max_retries Integer. The number of retry attempts.
#' @param limit Integer. The number of rows that should be returned. Primarily used
#' for testing or development purposes. Default is NULL (all available rows returned).
#'
#' @return A data frame containing the user-specified national extract. The columns returned will
#' vary based on the extract selected and are as follows:
#'
#' "actions" (Actions): "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "parameterGroup", "parameter", "actionType", "actionId",
#' "actionName", "actionAgency", "inIndianCountry", "includeInMeasure", "completionDate",
#' "assessmentUnitId", "assessmentUnitName", "fiscalYearEstablished", "locationDescription".
#' "waterSize", "waterSizeUnits", and "planSummaryLink".
#'
#' "assessments" (Assessments): "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "reportingCycle", "cycleLastAssessed", "assessmentUnitId",
#' "assessmentUnitName", "assessmentUnitStatus", "overallStatus", "epaIrCategory",
#' "stateIrCategory", "useGroup", "useName", "useClassName", "useSupport", "useIrCategory",
#' "useStateIrCategory", "monitoringStartDate", "monitoringEndDate", "assessmentDate",
#' "assessmentTypes", assessmentMethods", "assessmentBasis", "parameterGroup", "parameterName",
#' "parameterStatus", "parameterAttainment", parameterIrCategory" "parameterStateIrCategory",
#' delisted", "delistedReason", "pollutantIndicator", "cycleFirstListed",
#' "alternateListingIdentifier", "vision303dPriority", "cwa303dPriorityRanking",
#' "cycleScheduledForTmdl", "cycleExpectedToAttain", "consentDecreeCycle", "cycleId",
#' "seasonStartDate", "seasonEndDate", "associatedActionId", "associatedActionName",
#' "associatedActionType", "associatedActionStatus", "associatedActionAgency",
#' "locationDescription", "sizeSource", "sourceScale", "waterSize", and "waterSizeUnits".
#'
#'
#' "aus" (Assessment Units): "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "useClassName", "assessmentUnitId", "assessmentUnitName",
#' "assessmentUnitStatus", "reportingCycle", "cycleId", "locationDescription", "sizeSource",
#' "sourceScale", "waterSize", and "waterSizeUnits".
#'
#' "au_mls" (Assessment Units with Monitoring Locations): "objectId", "region", "state",
#' "organizationType", "organizationId", "organizationName", "waterType", "useClassName",
#' "monitoringLocationId", "monitoringLocationOrgId", "assessmentUnitId", "assessmentUnitName",
#' "assessmentUnitStatus", "reportingCycle", "cycleId", locationDescription",
#' "monitoringLocationDataLink", "sizeSource", "sourceScale", "waterSize", and "waterSizeUnits"
#'
#' "catch_corr" (Catchment Correspondence): "objectId", "region", "state", organizationType",
#' "organizationId", "organizationName", "assessmentUnitId", "assessmentUnitName",
#' "catchmentNhdPlusId", "reportingCycle", and "cycleId".
#'
#' "sources" (Sources): "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "assessmentUnitId", "assessmentUnitName", "reportingCycle",
#' "overallStatus", "epaIrCategory", "stateIrCategory", "parameterGroup", "causeName",
#' "sourceName", "confirmed", "cycleId", "locationDescription", "waterSize", and "waterSizeUnits".
#'
#' "tmdl" (TMDLs): "objectId", "region", "state", "organizationType", "organizationId",
#' "organizationName", "waterType", "pollutantGroup", "pollutant", "addressedParameterGroup",
#' "addressedParameter", "sourceType", "npdesIdentifier", "otherIdentifier", "actionId",
#' "actionName", "actionAgency", "inIndianCountry", "explicitMarginOfSafety",
#' implicitMarginOfSafety", "includeInMeasure", "completionDate", "tmdlDate",
#' "fiscalYearEstablished", "assessmentUnitId",  "assessmentUnitName", "loadAllocation",
#' "loadAllocationUnits", "locationDescription", "tmdlEndpoint", "waterSize", "waterSizeUnits",
#' "wasteLoadAllocation", and "planSummaryLink".
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' assessments <- EQ_NationalExtract(extract = "assessments")
#'
#' aus_monloc <- EQ_NationalExtract(extract = "au_mls")
#' }
#'
EQ_NationalExtract <- function(extract = NULL,
                               max_retries = 3,
                               limit = NULL) {
  if (is.null(extract)) {
    stop("EQ_NationalExtract: Function requires user to select Expert Query Profile to return.")
  }

  if (is.null(extract) ||
    !extract %in% c(
      "actions", "assessments", "aus", "au_mls",
      "catch_corr", "sources", "tmdl"
    )) {
    stop("EQ_NationalExtract: Function requires user to select Expert Query Profile to return.")
  }

  base.url <- "https://cg-7343d0e5-571f-451f-971f-8aaaf971df7e.s3-us-gov-west-1.amazonaws.com/"

  nat.url <- "national-downloads/"

  # parse JSON over httr2, tolerant of text/plain
  .get_json_httr2 <- function(url) {
    req <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(30)

    resp <- httr2::req_perform(req)

    # accept text/plain and fall back to jsonlite if needed
    tryCatch(
      httr2::resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE),
      error = function(e) {
        jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
      }
    )
  }

  latest.json <- .get_json_httr2(paste0(base.url, nat.url, "latest.json"))

  folder.num <- latest.json$julian

  # select profile based on user selection
  # when json is updated, date.print will be determined for each profile below label
  file <- switch(extract,
    "actions" = "actions",
    "assessments" = "assessments",
    "aus" = "assessment_units",
    "au_mls" = "assessment_units_monitoring_locations",
    "catch_corr" = "catchment_correspondence",
    "sources" = "sources",
    "tmdl" = "tmdl"
  )

  label <- switch(extract,
    "actions" = "Actions Profile",
    "assessments" = "Assessments Profile",
    "aus" = "Assessment Units Profile",
    "au_mls" = "Assessment Units with Monitoring Locations Profile",
    "catch_corr" = "Catchment Correspondence Profile",
    "sources" = "Sources Profile",
    "tmdl" = "Total Maximum Daily Load Profile"
  )

  update.dates <- .get_json_httr2(paste0(base.url, nat.url, folder.num, "/ready.json"))

  update.dates <- update.dates$details

  update.date <- update.dates |>
    dplyr::filter(name == paste0("attains_app.profile_", file)) |>
    dplyr::select(last_refresh_end_time) |>
    dplyr::pull() |>
    lubridate::as_datetime() |>
    lubridate::with_tz(tz = "US/Eastern") |>
    format("%B %d, %Y at %I:%M %p %Z")

  print(paste0(
    "EQ_NationalExtract: downloading ", label, " (Expert Query National Extract).",
    " It was last updated on ", update.date, "."
  ))

  url <- paste0(base.url, nat.url, folder.num, "/", file, ".csv", ".zip")

  # Helper function for retry logic
  nat.extract.retries <- function(url, max_retries) {
    temp <- tempfile(fileext = ".zip")
    for (attempt in seq_len(max_retries)) {
      tryCatch(
        {
          resp <- httr2::request(url) |>
            httr2::req_timeout(1200) |>
            httr2::req_method("GET") |>
            httr2::req_perform(path = temp)

          zip_raw <- httr2::resp_body_raw(resp)

          writeBin(zip_raw, temp)

          # If successful, return unzipped file
          unzipped.file <- utils::unzip(temp, exdir = tempdir())
          csv.file <- unzipped.file[grep("\\.csv$", unzipped.file, ignore.case = TRUE)]
          # can add verbose = FALSE, if we want to remove the progress bar here
          df <- data.table::fread(csv.file, check.names = FALSE)
          unlink(temp)
          unlink(unzipped.file)
          return(df)
        },
        error = function(e) {
          if (attempt == max_retries) {
            stop(paste0("Failed to download and unzip file after ", max_retries, " attempts. Error: ", e$message))
          } else {
            message(paste0("Attempt ", attempt, " failed: ", e$message, ". Retrying..."))
            Sys.sleep(1) # Optional: wait before retrying
          }
        }
      )
    }
    stop("Download and unzip failed after maximum retries.")
  }

  # open large csv file
  df <- nat.extract.retries(url, max_retries)

  # import cross walk to convert column names to match other rExpertQuery function output
  # import crosswalk ref file
  col.cw <- data.table::fread(system.file("extdata", "EQColumnsForPOST.csv",
    package = "rExpertQuery"
  ), check.names = TRUE) |>
    dplyr::select(col.name, nat_extract, position = dplyr::all_of(file)) |>
    dplyr::filter(!is.na(position)) |>
    dplyr::arrange(position)

  # combine the three TMDLENDPOINT columns to match output from EQ_TMDLs function
  if (extract == "tmdl") {
    df <- df |>
      dplyr::mutate(
        TMDLENDPOINT1 = ifelse(is.na(TMDLENDPOINT1), "", TMDLENDPOINT1),
        TMDLENDPOINT2 = ifelse(is.na(TMDLENDPOINT2), "", TMDLENDPOINT2),
        TMDLENDPOINT3 = ifelse(is.na(TMDLENDPOINT3), "", TMDLENDPOINT3)
      ) |>
      dplyr::mutate(TMDLENDPOINT = paste0(
        TMDLENDPOINT1, TMDLENDPOINT2,
        TMDLENDPOINT3
      )) |>
      dplyr::select(-"TMDLENDPOINT1", -"TMDLENDPOINT2", -"TMDLENDPOINT3")
  }

  # change column names
  data.table::setnames(df, as.character(col.cw$nat_extract), as.character(col.cw$col.name),
    skip_absent = TRUE
  )

  # change order of columns
  data.table::setcolorder(df, as.character(col.cw$col.name))

  # remove intermediate objects
  rm(
    url, latest.json, base.url, nat.url, folder.num, update.date, update.dates, label, file,
    col.cw
  )

  # if limit is not NULL
  if (!is.null(limit)) {
    if (!is.numeric(limit)) {
      stop("Param 'limit' must be an integer")
    }

    if (is.numeric(limit)) {
      df <- df |>
        dplyr::slice_head(n = limit)
    }
  }

  return(df)
}
