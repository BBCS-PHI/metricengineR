#' Check Active Indicator Values
#'
#' Checks that active indicators have a populated `indicator_value`, excluding
#' cases where a value cannot be calculated because the numerator or denominator
#' is unavailable.
#'
#' @param df A data frame containing indicator results, including
#'   `indicator_id`, `indicator_value`, `value_type_code`, `numerator`,
#'   and `denominator`.
#' @param metadata A data frame containing `indicator_id`, `status_code`,
#'   and `precalculated`.
#'
#' @return A data frame containing active indicator rows with unexpected
#'   missing `indicator_value`. Returns an empty data frame if no issues are
#'   found.
#'
#' @details
#' Indicators with `status_code == 1` are treated as active.
#'
#' For calculated indicators, a missing `indicator_value` is treated as
#' expected when either the numerator or denominator is missing, or when the
#' denominator is equal to zero. These rows are considered incalculable rather
#' than data quality failures.
#'
#' This includes calculation methods that intentionally return missing values
#' when calculation requirements are not met. For example, a directly
#' age-standardised rate may return a missing result when the underlying event
#' count is insufficient for calculation.
#'
#' Missing `indicator_value` is also allowed for precalculated indicators,
#' as published values may be unavailable or suppressed at source.
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
    "numerator",
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
        paste(
          missing_df_cols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  required_metadata_cols <- c(
    "indicator_id",
    "status_code",
    "precalculated"
  )
  
  missing_metadata_cols <- setdiff(
    required_metadata_cols,
    names(metadata)
  )
  
  if(length(missing_metadata_cols) > 0L){
    stop(
      paste0(
        "Missing required columns in `metadata`: ",
        paste(
          missing_metadata_cols,
          collapse = ", "
        )
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
  
  
  # Identify precalculated indicators
  
  precalculated_ids <- metadata |>
    dplyr::filter(
      .data$precalculated == "Yes"
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
      
      # Only active indicators
      .data$indicator_id %in% active_ids,
      
      # Indicator value is missing
      is.na(.data$indicator_value),
      
      # Precalculated values may be unavailable or suppressed at source
      !(.data$indicator_id %in% precalculated_ids),
      
      # Calculated values may legitimately be missing when
      # the required inputs are unavailable
      !(
        is.na(.data$numerator) |
          is.na(.data$denominator) |
          .data$denominator == 0
      )
    )
  
  # Report result
  
  if(nrow(failures) == 0L){
    
    cli::cli_alert_success(
      paste0(
        "All active indicators have a populated indicator_value, ",
        "excluding expected missing values."
      )
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