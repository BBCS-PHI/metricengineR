#' Check Invalid Percentages
#'
#' Checks percentage indicators for values greater than 100.
#'
#' @param df A data frame containing `indicator_id`, `value_type_code`,
#'   and `indicator_value`.
#'
#' @return A data frame containing percentage records with
#'   `indicator_value` greater than 100. Returns an empty data frame if no
#'   issues are found.
#'
#' @details
#' Percentage indicators are identified using `value_type_code == 2`.
#' Values above 100 are reported as warnings because they may be valid for
#' metrics where actual performance exceeds a planned or target value.
#'
#' @export
check_invalid_percentages <- function(df) {
  
  # Validate input
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    "indicator_id",
    "value_type_code",
    "indicator_value"
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
  
  
  # Identify percentage values above 100
  
  invalid_rows <- df |>
    dplyr::filter(
      .data$value_type_code == 2L,
      !is.na(.data$indicator_value),
      .data$indicator_value > 100
    )
  
  
  # Report result
  
  if(nrow(invalid_rows) > 0L){
    
    failed_indicators <- invalid_rows |>
      dplyr::distinct(
        .data$indicator_id
      ) |>
      dplyr::pull(
        .data$indicator_id
      )
    
    cli::cli_alert_warning(
      paste0(
        "Found percentage values greater than 100 for ",
        "indicator ID(s): ",
        paste(
          sort(failed_indicators),
          collapse = ", "
        ),
        ". This may be valid for metrics comparing actual performance against ",
        "planned or target values, where the actual numerator exceeds the ",
        "planned denominator. Please review these indicators to confirm that ",
        "values above 100% are expected."
      )
    )
    
  } else {
    
    cli::cli_alert_success(
      "No percentage values greater than 100 were found."
    )
  }
  
  # Return problematic rows
  
  invalid_rows
}
