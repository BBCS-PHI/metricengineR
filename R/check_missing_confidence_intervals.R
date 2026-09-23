#' Check Missing Confidence Intervals
#'
#' Checks for rows where `indicator_value` is populated but either
#' `lower_ci95` or `upper_ci95` is missing.
#'
#' @param df A data frame containing `indicator_id`, `indicator_value`,
#'   `value_type_code`, `lower_ci95`, and `upper_ci95`.
#'
#' @return A data frame containing rows with missing confidence intervals.
#'   Returns an empty data frame if no issues are found.
#'
#' @details
#' A message is printed indicating whether the check passed or whether any
#' rows have missing confidence intervals.
#'
#' @export
check_missing_confidence_intervals <- function(df) {
  
  # Validate input 
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    "indicator_id",
    "indicator_value",
    "value_type_code",
    "lower_ci95",
    "upper_ci95"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0L){
    stop(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  # Identify missing confidence intervals 
  
  missing_ci_rows <- df |>
    dplyr::filter(
      !is.na(.data$indicator_value),
      is.na(.data$lower_ci95) |
        is.na(.data$upper_ci95)
    )
  
  
  # Report result
  if(nrow(missing_ci_rows) == 0L){
    
    cli::cli_alert_success(
      "No missing confidence intervals."
    )
    
  } else {
    
    failed_ids <- missing_ci_rows |>
      dplyr::distinct(
        .data$indicator_id
      ) |>
      dplyr::pull(
        .data$indicator_id
      )
    
    cli::cli_alert_warning(
      paste0(
        "Some rows have missing confidence intervals. ",
        "This may be acceptable for value types where confidence intervals ",
        "are not expected. Indicator ID(s): ",
        paste(
          sort(failed_ids),
          collapse = ", "
        )
      )
    )
  }
  
  # Return rows with missing confidence intervals 
  
  missing_ci_rows
}
