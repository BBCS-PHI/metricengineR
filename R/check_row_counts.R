#' Check Row Counts
#'
#' Compares the number of rows in an input data frame with a reference data
#' frame and reports whether the row counts match.
#'
#' @param df A data frame whose row count will be checked.
#' @param reference_data A reference data frame to compare against.
#'
#' @return Invisibly returns a one-row data frame containing the input row
#'   count, reference row count, difference, and whether the counts match.
#'
#' @details
#' A message is printed showing whether the row counts match. A difference in
#' row counts may be expected where multiple input rows are collapsed into a
#' single calculated output row.
#'
#' @export
check_row_counts <- function(
    df,
    reference_data
) {
  
  # Validate inputs
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.data.frame(reference_data)){
    stop(
      "`reference_data` must be a data frame.",
      call. = FALSE
    )
  }
  
  
  # Count rows
  
  input_rows <- nrow(df)
  reference_rows <- nrow(reference_data)
  row_difference <- input_rows - reference_rows
  row_counts_match <- input_rows == reference_rows
  
  
  # Report result
  
  if(row_counts_match){
    
    cli::cli_alert_success(
      "Row counts match: {input_rows} rows."
    )
    
  } else {
    
    cli::cli_alert_warning(
      paste0(
        "Row counts do not match. ",
        "Input: {input_rows} rows | ",
        "Reference: {reference_rows} rows | ",
        "Difference: {row_difference}."
      )
    )
    
    cli::cli_alert_info(
      paste0(
        "This may be expected where multiple input rows are collapsed ",
        "into a single calculated output row."
      )
    )
  }
  
  
  # Return summary invisibly
  # So the output can also be stored as a variable
  
  invisible(
    data.frame(
      input_rows = input_rows,
      reference_rows = reference_rows,
      row_difference = row_difference,
      row_counts_match = row_counts_match
    )
  )
}