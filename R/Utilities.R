# set global variables
utils::globalVariables(c(
  ".data", ":="
))

#' Include .data from rlang to prevent global variable warnings
#' @importFrom rlang .data
