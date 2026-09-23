#' Check Source Code
#'
#' Checks that each indicator is associated with exactly one `source_code`.
#'
#' @param df A data frame containing indicator data.
#' @param indicator_col Name of the indicator ID column.
#'   Defaults to `"indicator_id"`.
#' @param source_code_col Name of the source code column.
#'   Defaults to `"source_code"`.
#'
#' @return A data frame containing indicators associated with more than one
#'   source code. Returns an empty data frame if no issues are found.
#'
#' @details
#' Each indicator should have only one source to prevent the same indicator
#' data being sourced through multiple routes, such as SQL and SharePoint.
#'
#' @export
check_source_code <- function(
    df,
    indicator_col = "indicator_id",
    source_code_col = "source_code"
) {
  
  # Validate input
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    indicator_col,
    source_code_col
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0L){
    stop(
      paste0(
        "Columns not found in `df`: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # Identify indicators with multiple source codes
  
  results <- df |>
    dplyr::group_by(
      .data[[indicator_col]]
    ) |>
    dplyr::summarise(
      n_source_codes = dplyr::n_distinct(
        .data[[source_code_col]],
        na.rm = TRUE
      ),
      source_codes = paste(
        unique(
          stats::na.omit(
            .data[[source_code_col]]
          )
        ),
        collapse = ", "
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$n_source_codes > 1L
    )
  
  # Report result
  
  if(nrow(results) == 0L){
    
    cli::cli_alert_success(
      "No indicators have more than one source code."
    )
    
  } else {
    
    cli::cli_alert_warning(
      "{nrow(results)} indicator(s) have more than one source code."
    )
  }
  
  # Return problematic indicators
  
  results
}
