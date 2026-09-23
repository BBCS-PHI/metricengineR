#' Check Missing Values
#'
#' Checks selected columns for missing or blank values.
#'
#' @param df A data frame containing the data to check.
#' @param cols Character vector of column names to check. Defaults to
#'   `indicator_id`, `start_date`, `end_date`, `time_period_type`,
#'   `combination_id`, and `source_code`.
#'
#' @return A data frame containing rows with missing values and an additional
#'   `missing_columns` column showing which fields are missing.
#'   Returns an empty data frame if no issues are found.
#'
#' @details
#' Character columns are treated as missing if they contain `NA`, an empty
#' string, or only whitespace.
#'
#' @export
check_missing_values <- function(
    df,
    cols = c(
      "indicator_id",
      "start_date",
      "end_date",
      "time_period_type",
      "combination_id",
      "source_code"
    )
) {
  
  # Validate input -----------------------------------------------------------
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.character(cols) || length(cols) == 0L){
    stop(
      "`cols` must be a character vector containing at least one column name.",
      call. = FALSE
    )
  }
  
  missing_cols <- setdiff(
    cols,
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
  
  
  # Check each required column ----------------------------------------------
  
  missing_checks <- lapply(
    cols,
    function(col){
      
      x <- df[[col]]
      
      missing <- is.na(x)
      
      if(is.character(x)){
        missing <- missing | trimws(x) == ""
      }
      
      missing
    }
  )
  
  names(missing_checks) <- cols
  
  missing_matrix <- do.call(
    cbind,
    missing_checks
  )
  
  
  # Identify rows containing missing values ---------------------------------
  
  rows_with_missing <- rowSums(missing_matrix) > 0L
  
  missing_rows <- df[
    rows_with_missing,
    ,
    drop = FALSE
  ]
  
  
  # Add the names of missing columns ----------------------------------------
  
  missing_rows$missing_columns <- apply(
    missing_matrix[rows_with_missing, , drop = FALSE],
    1,
    function(x){
      paste(
        cols[x],
        collapse = ", "
      )
    }
  )
  
  
  # Report result ------------------------------------------------------------
  
  if(nrow(missing_rows) == 0L){
    
    cli::cli_alert_success(
      "No missing values in the checked columns."
    )
    
  } else {
    
    columns_with_missing <- cols[
      colSums(missing_matrix) > 0L
    ]
    
    cli::cli_alert_warning(
      paste0(
        "Found ",
        nrow(missing_rows),
        " row(s) with missing values. ",
        "Affected column(s): ",
        paste(
          columns_with_missing,
          collapse = ", "
        ),
        "."
      )
    )
  }
  
  
  # Return problematic rows --------------------------------------------------
  
  missing_rows
}
