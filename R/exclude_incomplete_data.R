#' Exclude Incomplete Reporting Periods
#'
#' Removes rows belonging to reporting periods that are not yet complete.
#'
#' The function uses completed historical records to identify the typical
#' reporting period end date and, optionally, the typical duration of each
#' reporting cycle.
#'
#' @param df A data frame containing reporting period data.
#' @param period_type_col Name of the column containing the reporting period
#'   type, such as `"Calendar"` or `"Financial"`.
#'   Defaults to `"period_type"`.
#' @param start_date_col Name of the column containing period start dates.
#'   Defaults to `"start_date"`.
#' @param end_date_col Name of the column containing period end dates.
#'   Defaults to `"end_date"`.
#' @param time_period_col Optional name of the column containing the time period
#'   type, such as `"Monthly"`, `"Quarterly"`, or `"1 year"`.
#'   Defaults to `NULL`.
#' @param indicator_col Optional name of the indicator identifier column.
#'   Defaults to `NULL`.
#' @param current_date Date used to determine the latest completed reporting
#'   cycle. Defaults to `Sys.Date()`.
#' @param enforce_duration Logical indicating whether records should also be
#'   checked against the typical duration of the reporting period.
#'   Defaults to `TRUE`.
#' @param tolerance_days Number of days a record duration may differ from the
#'   typical duration and still be considered valid. Defaults to `5L`.
#'
#' @return The original data frame with incomplete or invalid reporting periods
#'   removed.
#'
#' @details
#' The typical reporting cycle end date is identified from completed historical
#' records using the most common month and day of the end date.
#'
#' For monthly data, the latest completed period is the end of the previous
#' calendar month.
#'
#' When `enforce_duration = TRUE`, records whose duration differs from the
#' typical duration by more than `tolerance_days` are excluded.
#'
#' A message is printed showing how many rows were excluded.
#'
#' @examples
#' example_data <- data.frame(
#'   period_type = c("Monthly", "Monthly"),
#'   start_date = as.Date(c("2026-01-01", "2026-02-01")),
#'   end_date = as.Date(c("2026-01-31", "2026-02-28"))
#' )
#'
#' exclude_incomplete_data(
#'   df = example_data,
#'   period_type_col = "period_type",
#'   start_date_col = "start_date",
#'   end_date_col = "end_date"
#' )
#' @export
exclude_incomplete_data <- function(
    df,
    period_type_col = "period_type",
    start_date_col = "start_date",
    end_date_col = "end_date",
    time_period_col = NULL,
    indicator_col = NULL,
    current_date = Sys.Date(),
    enforce_duration = TRUE,
    tolerance_days = 5L
) {
  
  # Validate inputs 
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    period_type_col,
    start_date_col,
    end_date_col,
    time_period_col,
    indicator_col
  )
  
  required_cols <- required_cols[
    !is.null(required_cols) &
      !is.na(required_cols)
  ]
  
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
  
  if(!inherits(current_date, "Date")){
    stop(
      "`current_date` must be a Date.",
      call. = FALSE
    )
  }
  
  if(
    !is.logical(enforce_duration) ||
    length(enforce_duration) != 1L ||
    is.na(enforce_duration)
  ){
    stop(
      "`enforce_duration` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  
  # Prepare data 
  
  prepared_data <- df |>
    dplyr::mutate(
      
      .row_id = dplyr::row_number(),
      
      period_type_standardised = # Convert "Calendar" or "Financial Year" period type into a lowercase
        stringr::str_to_lower(
          as.character(
            .data[[period_type_col]]
          )
        ),
      
      time_period_standardised =  # Convert "1 year" or "Quarterly" and so on time period type into a lowercase
        if(!is.null(time_period_col)){
          
          stringr::str_to_lower(
            as.character(
              .data[[time_period_col]]
            )
          )
          
        } else {
          
          NA_character_
        }
    )
  
  
  # Decide which fields define a reporting cycle 
  
  grouping_columns <- "period_type_standardised"
  
  if(!is.null(time_period_col)){
    
    grouping_columns <- c(
      grouping_columns,
      "time_period_standardised"
    )
  }
  
  
  # Learn the usual end date and duration from completed historical records 
  
  period_metadata <- prepared_data |>
    dplyr::filter(
      !is.na(.data[[end_date_col]]),
      .data[[end_date_col]] <= current_date # Filters out future data
    ) |>
    dplyr::mutate(
      
      end_month_day =
        format(
          .data[[end_date_col]],
          "%m-%d"
        ),
      
      duration_days =
        dplyr::if_else(
          !is.na(.data[[start_date_col]]),
          as.numeric(
            .data[[end_date_col]] -
              .data[[start_date_col]]
          ) + 1,
          NA_real_
        )
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(grouping_columns)
      )
    ) |>
    dplyr::summarise(
      
      typical_end_month_day =
        most_common_value(
          .data$end_month_day
        ),
      
      typical_duration_days =
        stats::median(
          .data$duration_days,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
  
  
  # Add expected period information to each row 
  
  checked_data <- prepared_data |>
    dplyr::left_join(
      period_metadata,
      by = grouping_columns
    ) |>
    dplyr::mutate(
      
      # Default to 31 December if an expected end date cannot be learned
      typical_end_month_day =
        dplyr::coalesce(
          .data$typical_end_month_day,
          "12-31"
        ),
      
      # Last fully completed month
      latest_completed_month_end =
        lubridate::floor_date(
          current_date,
          "month"
        ) -
        lubridate::days(1),
      
      # Expected annual/cycle end during the current year
      expected_cycle_end_this_year =
        as.Date(
          sprintf(
            "%d-%s",
            lubridate::year(current_date),
            .data$typical_end_month_day
          )
        ),
      
      # Work out the latest cycle that can be considered complete
      latest_completed_cycle =
        dplyr::case_when(
          
          .data$time_period_standardised == "monthly" ~
            .data$latest_completed_month_end,
          
          .data$expected_cycle_end_this_year <= current_date ~
            .data$expected_cycle_end_this_year,
          
          TRUE ~
            as.Date(
              sprintf(
                "%d-%s",
                lubridate::year(current_date) - 1L,
                .data$typical_end_month_day
              )
            )
        ),
      
      # Calculate the actual duration of each record
      actual_duration_days =
        dplyr::if_else(
          
          !is.na(.data[[start_date_col]]) &
            !is.na(.data[[end_date_col]]),
          
          as.numeric(
            .data[[end_date_col]] -
              .data[[start_date_col]]
          ) + 1,
          
          NA_real_
        ),
      
      # Check whether the record duration is close to the normal duration
      duration_is_valid =
        dplyr::if_else(
          
          enforce_duration &
            !is.na(.data$typical_duration_days) &
            !is.na(.data$actual_duration_days),
          
          abs(
            .data$actual_duration_days -
              .data$typical_duration_days
          ) <= tolerance_days,
          
          TRUE
        )
    )
  
  
  # Keep only completed and valid periods 
  
  result <- checked_data |>
    dplyr::filter(
      
      !is.na(.data[[end_date_col]]),
      
      .data[[end_date_col]] <=
        .data$latest_completed_cycle,
      
      .data$duration_is_valid
    )
  
  
  # Report number of excluded rows 
  
  excluded_rows <- nrow(df) - nrow(result)
  
  message(
    "Excluded ",
    excluded_rows,
    " row(s) out of ",
    nrow(df),
    "."
  )
  
  
  # Remove temporary columns 
  
  result |>
    dplyr::select(
      -dplyr::all_of(
        c(
          ".row_id",
          "period_type_standardised",
          "time_period_standardised",
          "typical_end_month_day",
          "typical_duration_days",
          "latest_completed_month_end",
          "expected_cycle_end_this_year",
          "latest_completed_cycle",
          "actual_duration_days",
          "duration_is_valid"
        )
      )
    )
}