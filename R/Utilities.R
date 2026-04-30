# set global variables
utils::globalVariables(c(
  ":=",
  ".data",
   ".env",
  "attains_ws_field",
  "attains_ws_name",
  "col.name",
  "domain",
  "end_time",
  "eq_name",
  "eq_param",
  "last_refresh",
  "name",
  "nat_extract",
  "param",
  "position",
  "TMDLENDPOINT1",
  "TMDLENDPOINT2",
  "TMDLENDPOINT3",
  "value"
))

#' Include .data from rlang to prevent global variable warnings
#' @importFrom rlang .data
