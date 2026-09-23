#' Check Duplicate Records
#'
#' Checks for duplicate metric records based on a set of key columns.
#'
#' @param df A data frame containing metric data.
#' @param key_cols Character vector of columns used to identify unique records.
#'   Defaults to `indicator_id`, `start_date`, `end_date`, `aggregation_id`,
#'   `age_group_code`, `sex_code`, `ethnicity_code`, `imd_code`,
#'   `value_type_code`, and `source_code`.
#' @param indicator_filter Optional vector of indicator IDs to check.
#'   If `NULL`, all indicators are checked.
#'
#' @return A data frame containing duplicated key combinations and their
#'   `row_count`. Returns an empty data frame if no duplicates are found.
#'
#' @details
#' A message is printed indicating whether duplicate records were found.
#'
#' @export
check_duplicates <- function(
    df,
    key_cols = c(
      "indicator_id",
      "start_date",
      "end_date",
      "aggregation_id",
      "age_group_code",
      "sex_code",
      "ethnicity_code",
      "imd_code",
      "value_type_code",
      "source_code"
    ),
    indicator_filter = NULL
) {
  
  # Validate input
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  missing_cols <- setdiff(
    key_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0L){
    stop(
      paste0(
        "Key columns not found in `df`: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  # Apply optional indicator filter
  
  data_to_check <- df
  
  if(!is.null(indicator_filter)){
    
    data_to_check <- data_to_check |>
      dplyr::filter(
        .data$indicator_id %in% indicator_filter
      )
  }
  
  
  # Identify duplicate key combinations
  
  result <- data_to_check |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(key_cols)
      )
    ) |>
    dplyr::summarise(
      row_count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$row_count > 1L
    ) |>
    dplyr::arrange(
      .data$indicator_id
    )
  
  
  # Report result
  
  if(nrow(result) > 0L){
    
    cli::cli_alert_danger(
      paste0(
        "Duplicate records found: ",
        nrow(result),
        " duplicate key combination(s)."
      )
    )
    
  } else {
    
    cli::cli_alert_success(
      "No duplicate records found for the specified key columns."
    )
  }
  
  # Return duplicate key combinations
  
  result
}
