#' Calculate crude rates
#'
#' Calculates crude rates for rows identified as crude rate value types.
#' Confidence intervals can optionally be calculated using
#' `PHEindicatormethods::phe_rate()`.
#'
#' @param df A data frame containing the values to calculate.
#' @param indicator_id_col Name of the column containing indicator IDs.
#'   Defaults to `"indicator_id"`.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param numerator_col Name of the numerator column.
#'   Defaults to `"numerator"`.
#' @param denominator_col Name of the denominator column.
#'   Defaults to `"denominator"`.
#' @param value_multiplier_col Name of the column containing the value multiplier.
#'   Defaults to `"value_multiplier"`.
#' @param confidence_intervals_required Logical indicating whether confidence
#'   intervals should be calculated. Defaults to `FALSE`.
#'
#' @return A data frame containing crude rate rows with calculated `value`,
#'   `lowercl`, and `uppercl` columns.
#'
#' @details
#' Rows are eligible for calculation when the numerator, denominator, and value
#' multiplier are present and the denominator is greater than zero.
#'
#' Each indicator must have a single value multiplier. Different indicators
#' may use different value multipliers.
#'
#' Crude rates are calculated as numerator divided by denominator multiplied
#' by the value multiplier. Numerators cannot be negative.
#'
#' When confidence intervals are required, `PHEindicatormethods::phe_rate()`
#' is applied separately to each indicator using that indicator's value
#' multiplier.
#'
#' Ineligible rows are retained with calculated values set to `NA`.
#'
#' @export
calculate_crude_rate <- function(
    df,
    indicator_id_col = "indicator_id",
    value_type_code_col = "value_type_code",
    numerator_col = "numerator",
    denominator_col = "denominator",
    value_multiplier_col = "value_multiplier",
    confidence_intervals_required = FALSE
){
  
  # Validate input
  
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    indicator_id_col,
    value_type_code_col,
    numerator_col,
    denominator_col,
    value_multiplier_col
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0L){
    stop(
      "Missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if(
    !is.logical(confidence_intervals_required) ||
    length(confidence_intervals_required) != 1L ||
    is.na(confidence_intervals_required)
  ){
    stop(
      "`confidence_intervals_required` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  
  # Filter crude rate rows
  
  result <- df |>
    dplyr::filter(
      .data[[value_type_code_col]] == 3L
    )
  
  
  # Check each indicator has only one value multiplier
  
  multiplier_check <- result |>
    dplyr::filter(
      !is.na(.data[[value_multiplier_col]])
    ) |>
    dplyr::group_by(
      .data[[indicator_id_col]]
    ) |>
    dplyr::summarise(
      multiplier_count = dplyr::n_distinct(
        .data[[value_multiplier_col]]
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$multiplier_count > 1L
    )
  
  if(nrow(multiplier_check) > 0L){
    
    problem_indicators <- paste(
      multiplier_check[[indicator_id_col]],
      collapse = ", "
    )
    
    stop(
      paste0(
        "More than one value multiplier found for indicator_id(s): ",
        problem_indicators,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  # Define rows eligible for calculation
  
  eligible <- !is.na(result[[numerator_col]]) &
    !is.na(result[[denominator_col]]) &
    result[[denominator_col]] > 0 &
    !is.na(result[[value_multiplier_col]])
  
  
  # Eligible rows must have an indicator ID
  
  if(
    any(
      is.na(result[[indicator_id_col]][eligible])
    )
  ){
    stop(
      "`indicator_id` cannot be missing for eligible crude rate calculations.",
      call. = FALSE
    )
  }
  
  
  # Crude rate numerators cannot be negative
  
  if(
    any(
      result[[numerator_col]][eligible] < 0
    )
  ){
    stop(
      "Numerator cannot be negative for crude rate calculations.",
      call. = FALSE
    )
  }
  
  
  # Initialise calculated columns
  
  result$value <- rep(
    NA_real_,
    nrow(result)
  )
  
  result$lowercl <- rep(
    NA_real_,
    nrow(result)
  )
  
  result$uppercl <- rep(
    NA_real_,
    nrow(result)
  )
  
  
  # Calculate crude rates without confidence intervals
  
  if(!confidence_intervals_required){
    
    result$value[eligible] <-
      result[[numerator_col]][eligible] /
      result[[denominator_col]][eligible] *
      result[[value_multiplier_col]][eligible]
    
    return(result)
  }
  
  
  # Calculate crude rates with confidence intervals
  
  if(any(eligible)){
    
    eligible_rows <- which(
      eligible
    )
    
    indicator_groups <- split(
      eligible_rows,
      result[[indicator_id_col]][eligible_rows]
    )
    
    
    for(rows in indicator_groups){
      
      df_rate <- result[rows, ] |>
        dplyr::mutate(
          numerator_rate = .data[[numerator_col]],
          denominator_rate = .data[[denominator_col]]
        )
      
      multiplier_value <- unique(
        df_rate[[value_multiplier_col]]
      )[1]
      
      
      rate_result <- PHEindicatormethods::phe_rate(
        df_rate,
        x = numerator_rate,
        n = denominator_rate,
        confidence = 0.95,
        type = "standard",
        multiplier = multiplier_value
      )
      
      
      result$value[rows] <- rate_result$value
      result$lowercl[rows] <- rate_result$lowercl
      result$uppercl[rows] <- rate_result$uppercl
    }
  }
  
  
  result
}