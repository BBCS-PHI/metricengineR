#' Check Active Indicator Values
#'
#' Checks that active indicators have a populated `indicator_value`.
#'
#' @param df A data frame containing indicator results, including
#'   `indicator_id`, `indicator_value`, `value_type_code`, and `denominator`.
#' @param metadata A data frame containing `indicator_id` and `status_code`.
#'
#' @return A data frame containing active indicator rows with unexpected
#'   missing `indicator_value`. Returns an empty data frame if no issues are found.
#'
#' @details
#' Indicators with `status_code == 1` are treated as active.
#'
#' Missing `indicator_value` is allowed for value types `2`, `9`, and `10`
#' when the denominator is missing or equal to zero.
#'
#' @export
check_active_indicator_values <- function(df, metadata) {
  
  # Validate inputs 
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.data.frame(metadata)){
    stop(
      "`metadata` must be a data frame.",
      call. = FALSE
    )
  }
  
  
  required_df_cols <- c(
    "indicator_id",
    "indicator_value",
    "value_type_code",
    "denominator"
  )
  
  missing_df_cols <- setdiff(
    required_df_cols,
    names(df)
  )
  
  if(length(missing_df_cols) > 0L){
    stop(
      paste0(
        "Missing required columns in `df`: ",
        paste(missing_df_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  required_metadata_cols <- c(
    "indicator_id",
    "status_code"
  )
  
  missing_metadata_cols <- setdiff(
    required_metadata_cols,
    names(metadata)
  )
  
  if(length(missing_metadata_cols) > 0L){
    stop(
      paste0(
        "Missing required columns in `metadata`: ",
        paste(missing_metadata_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  # Identify active indicators 
  
  active_ids <- metadata |>
    dplyr::filter(
      .data$status_code == 1L
    ) |>
    dplyr::distinct(
      .data$indicator_id
    ) |>
    dplyr::pull(
      .data$indicator_id
    )
  
  
  # Identify unexpected missing indicator values 
  
  failures <- df |>
    dplyr::filter(
      .data$indicator_id %in% active_ids,
      is.na(.data$indicator_value),
      !(
        .data$value_type_code %in% c(2L, 9L, 10L) & # Percentage, percentage change, and percentage point difference
          (
            is.na(.data$denominator) |
              .data$denominator == 0
          )
      )
    )
  
  
  # Report result 
  
  if(nrow(failures) == 0L){
    
    cli::cli_alert_success(
      "All active indicators have a populated indicator_value, excluding expected missing values."
    )
    
  } else {
    
    failed_ids <- failures |>
      dplyr::distinct(
        .data$indicator_id
      ) |>
      dplyr::pull(
        .data$indicator_id
      )
    
    cli::cli_alert_danger(
      paste0(
        "Found active indicators with unexpected missing indicator_value ",
        "for indicator ID(s): ",
        paste(
          failed_ids,
          collapse = ", "
        )
      )
    )
  }
  
  # Return failing rows 
  
  failures
}