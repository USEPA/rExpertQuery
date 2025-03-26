hrm_api <- "VDJtMKffFkeFIwTOrdD282tybxj06df8oxrgfNf8"

orgcheck <- "MDE_EASP"

cat <- "4B"

MD_check <- EQ_Assessments(org_id = "MDE_EASP",
                           use_ir_cat = cat,
                           api_key = hrm_api)


MD_check2 <- EQ_Assessments(org_id = orgcheck, api_key = hrm_api)


as_string(MD_check2)


my_function <- function(...) {
  # Capture all arguments using ...
  args <- list(...)

  # Create a data frame from the captured arguments
  df <- data.frame(args, stringsAsFactors = FALSE) # Use stringsAsFactors = FALSE to prevent character vectors from becoming factors

  return(df)
}


# Example usage:
result_df <- my_function(name = "Alice", age = 30, city = "Carbondale")
print(result_df)
