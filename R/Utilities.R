# set global variables
utils::globalVariables(c(
  ".data", ":=", ".env", "attains_ws_field", "attains_ws_name", "domain"
))

#' Include .data from rlang to prevent global variable warnings
#' @importFrom rlang .data
