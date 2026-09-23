#' Check DASR Age Group Codes
#'
#' Checks that each DASR indicator has only one unique `age_group_code`.
#' DASR indicators are identified using `value_type_code == 4`.
#'
#' @param df A data frame containing `indicator_id`, `age_group_code`,
#'   and `value_type_code`.
#'
#' @return A data frame containing DASR indicators with more than one unique
#'   `age_group_code`. Returns an empty data frame if no issues are found.
#'
#' @details
#' A message is printed indicating whether the check passed or whether any DASR
#' indicators have multiple age group codes.
#'
#' @export
check_dasr_age_group_code <- function(df) {
  
  # Validate input 
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    "indicator_id",
    "age_group_code",
    "value_type_code"
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
  
  
  # Keep DASR indicators and count unique age group codes 
  
  age_group_summary <- df |>
    dplyr::filter(
      .data$value_type_code == 4L
    ) |>
    dplyr::distinct(
      .data$indicator_id,
      .data$age_group_code
    ) |>
    dplyr::count(
      .data$indicator_id,
      name = "age_group_count"
    ) |>
    dplyr::arrange(
      .data$indicator_id
    )
  
  
  # Keep indicators with more than one age group code 
  
  failures <- age_group_summary |>
    dplyr::filter(
      .data$age_group_count > 1L
    )
  
  
  # Report result 
  
  if(nrow(failures) == 0L){
    
    cli::cli_alert_success(
      "Each DASR indicator has one unique age_group_code."
    )
    
  } else {
    
    cli::cli_alert_danger(
      paste0(
        nrow(failures),
        " DASR indicator(s) have more than one age_group_code."
      )
    )
  }
  
  
  # Return failing indicators 
  
  failures
}
