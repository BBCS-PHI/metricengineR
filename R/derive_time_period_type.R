#' Derive Time Period Type
#'
#' Classifies a date range into a standard metric-engine time period type based
#' on the difference between `start_date` and `end_date`.
#'
#' Supported time period types are:
#'
#' \itemize{
#'   \item `"Monthly"` - a one-month reporting period.
#'   \item `"Quarterly"` - a three-month reporting period.
#'   \item `"1 year"` - a one-year reporting period.
#'   \item `"2 year pooled"` - a reporting period covering two consecutive years.
#'   \item `"3 year pooled"` - a reporting period covering three consecutive years.
#'   \item `"5 year pooled"` - a reporting period covering five consecutive years.
#' }
#'
#' @param start_date A Date vector containing the start date of each reporting
#'   period.
#' @param end_date A Date vector containing the end date of each reporting
#'   period.
#'
#' @return A character vector containing the derived time period type.
#'   Periods that do not match one of the supported time period types return
#'   `NA`.
#'
#' @details
#' Time period type is derived from the actual duration between `start_date`
#' and `end_date`.
#'
#' The function accepts periods ending either on the anniversary date or one
#' day before the anniversary date. This allows standard reporting periods such
#' as `2024-01-01` to `2024-12-31` to be classified as `"1 year"`.
#'
#' Pooled periods are classified according to their actual duration. For
#' example:
#'
#' \itemize{
#'   \item `2020-01-01` to `2021-12-31` is `"2 year pooled"`.
#'   \item `2020-01-01` to `2022-12-31` is `"3 year pooled"`.
#'   \item `2020-01-01` to `2024-12-31` is `"5 year pooled"`.
#' }
#'
#' @examples
#' derive_time_period_type(
#'   start_date = as.Date("2024-01-01"),
#'   end_date = as.Date("2024-12-31")
#' )
#'
#' derive_time_period_type(
#'   start_date = as.Date("2024-01-01"),
#'   end_date = as.Date("2024-03-31")
#' )
#'
#' derive_time_period_type(
#'   start_date = as.Date("2020-01-01"),
#'   end_date = as.Date("2021-12-31")
#' )
#'
#' derive_time_period_type(
#'   start_date = as.Date("2020-01-01"),
#'   end_date = as.Date("2022-12-31")
#' )
#'
#' @export
derive_time_period_type <- function(
    start_date,
    end_date
) {
  
  # Validate inputs 
  
  if(!inherits(start_date, "Date")){
    stop(
      "`start_date` must be a Date vector.",
      call. = FALSE
    )
  }
  
  if(!inherits(end_date, "Date")){
    stop(
      "`end_date` must be a Date vector.",
      call. = FALSE
    )
  }
  
  if(length(start_date) != length(end_date)){
    stop(
      "`start_date` and `end_date` must have the same length.",
      call. = FALSE
    )
  }
  
  
  # Check whether dates match a specified period 
  
  matches_period <- function(period){
    
    anniversary <- lubridate::add_with_rollback(
      start_date,
      period
    )
    
    (end_date == anniversary) |
      (end_date == anniversary - lubridate::days(1))
  }
  
  
  # Derive time period type 
  
  dplyr::case_when(
    
    matches_period(
      lubridate::period(months = 1)
    ) ~ "Monthly",
    
    matches_period(
      lubridate::period(months = 3)
    ) ~ "Quarterly",
    
    matches_period(
      lubridate::period(years = 1)
    ) ~ "1 year",
    
    matches_period(
      lubridate::period(years = 2)
    ) ~ "2 year pooled",
    
    matches_period(
      lubridate::period(years = 3)
    ) ~ "3 year pooled",
    
    matches_period(
      lubridate::period(years = 5)
    ) ~ "5 year pooled",
    
    TRUE ~ NA_character_
  )
}

