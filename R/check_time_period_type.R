#' Check Time Period Type
#'
#' Checks whether `time_period_type` is populated for all rows.
#'
#' @param df A data frame containing `indicator_id`, `start_date`, `end_date`,
#'   and `time_period_type`.
#'
#' @return A data frame containing records with missing or blank
#'   `time_period_type`. Returns an empty data frame if no issues are found.
#'
#' @details
#' A message is printed indicating whether the check passed or whether any
#' records have a missing `time_period_type`.
#'
#' @export
check_time_period_type <- function(df) {
  
  # Validate input 
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    "indicator_id",
    "start_date",
    "end_date",
    "time_period_type"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0){
    stop(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  # Identify rows with missing time period type 
  
  missing_rows <- df |>
    dplyr::filter(
      is.na(.data$time_period_type) |
        trimws(.data$time_period_type) == ""
    ) |>
    dplyr::distinct(
      .data$indicator_id,
      .data$start_date,
      .data$end_date
    )
  
  
  # Report result 
  
  if(nrow(missing_rows) == 0L){
    
    message(
      "\u2705 PASS: time_period_type is populated for all rows."
    )
    
  } else {
    
    message(
      "\u26A0\uFE0F WARNING: Found ",
      nrow(missing_rows),
      " record(s) with missing time_period_type."
    )
    
  }
  
  
  # Return problematic rows 
  
  missing_rows
}