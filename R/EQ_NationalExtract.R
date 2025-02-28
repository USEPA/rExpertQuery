#' Import Expert Query National Extract
#'
#' Returns data frame of user-specified Expert Query National Extracts for use cases where
#' nationwide data is desired. Also useful in situations where the the desired query would yield
#' more than 1 million rows as the national extracts can be sorted and filtered after import. The
#' National Extracts are large files and can take several minutes to download and import.
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
#' HRM NOTE 2/11/25 - can download but not import catchment correspondence due to file size.
#'
#' @param extract Character argument. Specifies which Expert Query National Extract should be
#' imported. Options are "actions" (Actions), "assessments" (Assessments), "au" (Assessment Units),
#' "au_mls" (Assessment Units with Monitoring Locations), "catch_corr" (Catchment Correspondence),
#' "sources" (Sources), and "tmdl" (TMDLs). There is no national extract option available for
#' Actions Documents. The default is NULL which means no extract will be returned.
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
EQ_NationalExtract <- function(extract = NULL) {
  if (is.null(extract)) {
    stop("EQ_NationalExtract: Function requires user to select Expert Query Profile to return.")
  }

  if (is.null(extract) &
      !extract %in% c("actions", "assessments", "aus", "au_mls",
                      "catch_corr", "sources", "tmdl")) {
    stop("EQ_NationalExtract: Function requires user to select Expert Query Profile to return.")
  }

  base.url <- "https://cg-7343d0e5-571f-451f-971f-8aaaf971df7e.s3-us-gov-west-1.amazonaws.com/"

  nat.url <- "national-downloads/"

  latest.json <- jsonlite::fromJSON(paste0(base.url, nat.url, "latest.json"))

  folder.num <- latest.json$julian

  date.print <- format(lubridate::as_datetime(folder.num), "%B %d, %Y")

  # select profile based on user selection
  # when json is updated, date.print will be determined for each profile below label
  if (extract == "actions") {
    file <- "actions.csv"

    label <- "Actions Profile"
  }

  if (extract == "assessments") {
    file <- "assessments.csv"

    label <- "Assessments Profile"
  }

  if (extract == "aus") {
    file <- "assessment_units.csv"

    label <- "Assessment Units Profile"

    extract <- "assessment_units"
  }

  if (extract == "au_mls") {
    file <- "assessment_units_monitoring_locations.csv"

    label <- "Assessment Units with Monitoring Locations Profile"

    extract <- "assessment_units_mls"
  }

  if (extract == "catch_corr") {
    file <- "catchment_correspondence.csv"

    label <- "Catchment Correspondance Profile"
  }

  if (extract == "sources") {
    file <- "sources.csv"

    label <- "Sources Profile"
  }

  if (extract == "tmdl") {
    file <- "tmdl.csv"

    label <- "Total Maximum Daily Load Profile"
  }

  print(paste0(
    "EQ_NationalExtract: downloading ", label, " (Expert Query National Extract).",
    " It was last updated on ", date.print, "."
  ))

  url <- paste0(base.url, nat.url, folder.num, "/", file, ".zip")

  # set up tempfile
  temp <- tempfile(fileext = ".zip")

  # increase timeout (for large files)
  options(timeout = 1200)

  # download zipped file
  httr::GET(url, httr::write_disk(temp, overwrite = TRUE))

  print(paste0(
    "EQ_NationalExtract: unzipping ", label, " (Expert Query National Extract)."
  ))

  # unzip file
  unzipped.file <- utils::unzip(temp, exdir = tempdir())

  # identify csv file to read in
  csv.file <- unzipped.file[grep("\\.csv$", unzipped.file, ignore.case = TRUE)]

  print(paste0(
    "EQ_NationalExtract: ", "opening ", label, " (Expert Query National Extract)."
  ))

  # open large csv file
  # can add verbose = FALSE, if we want to remove the progress bar here
  df <- data.table::fread(csv.file)

  # import cross walk to convert column names to match other rExpertQuery function output
  # import crosswalk ref file
  col.cw <- utils::read.csv(file = "inst/extdata/EQColumnsForPOST.csv") %>%
    dplyr::select(col.name, nat_extract, dplyr::all_of(extract)) %>%
    dplyr::filter(!is.na(.data[[extract]])) %>%
    dplyr::arrange((.data[[extract]]))

  # combine the three TMDLENDPOINT columns to match output from EQ_TMDLs function
  if(extract == "tmdl") {
    df <- df %>%
      dplyr::mutate(TMDLENDPOINT1 = ifelse(is.na(TMDLENDPOINT1), "", TMDLENDPOINT1),
                    TMDLENDPOINT2 = ifelse(is.na(TMDLENDPOINT2), "", TMDLENDPOINT2),
                    TMDLENDPOINT3 = ifelse(is.na(TMDLENDPOINT3), "", TMDLENDPOINT3)) %>%
      dplyr::mutate(TMDLENDPOINT = paste0(TMDLENDPOINT1, TMDLENDPOINT2, TMDLENDPOINT3)) %>%
      dplyr::select(-TMDLENDPOINT1, -TMDLENDPOINT2, -TMDLENDPOINT3)
  }

  # change column names
  data.table::setnames(df, as.character(col.cw$nat_extract), as.character(col.cw$col.name),
                       skip_absent = TRUE)

  # change order of columns
  data.table::setcolorder(df, as.character(col.cw$col.name))

  unlink(temp)

  unlink(unzipped.file)

  # remove intermediate objects
  rm(
    url, latest.json, base.url, nat.url, folder.num, date.print, label, file, temp,
    unzipped.file, csv.file, col.cw
  )

  return(df)
}
