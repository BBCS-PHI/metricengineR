#' Calculate directly age-standardised rates
#'
#' Calculates directly age-standardised rates for rows with value type code 4
#' using `PHEindicatormethods::calculate_dsr()`.
#'
#' @param df A data frame containing the values to calculate.
#' @param age_metadata A data frame containing `indicator_id` and
#'   `single_age_code`.
#' @param value_type_code_col Name of the column containing value type codes.
#'   Defaults to `"value_type_code"`.
#' @param numerator_col Name of the numerator column.
#'   Defaults to `"numerator"`.
#' @param denominator_col Name of the denominator column.
#'   Defaults to `"denominator"`.
#' @param value_multiplier_col Name of the column containing the value multiplier.
#'   Defaults to `"value_multiplier"`.
#' @param confidence_intervals_required Logical indicating whether confidence
#'   intervals should be retained. Defaults to `TRUE`.
#'
#' @return A data frame containing DASR results with calculated `value`,
#'   `lowercl`, `uppercl`, and final `age_group_code`.
#'
#' @details
#' DASRs are calculated using the ESP 2013 standard population.
#' Calculations are performed separately for each value multiplier so that
#' `PHEindicatormethods::calculate_dsr()` receives a single multiplier value.
#'
#' Each DASR calculation must contain one consistent value multiplier.
#'
#' When confidence intervals are not required, `lowercl` and `uppercl` are
#' returned as `NA`.
#'
#' @export
calculate_dasr <- function(
    df,
    age_metadata,
    value_type_code_col = "value_type_code",
    numerator_col = "numerator",
    denominator_col = "denominator",
    value_multiplier_col = "value_multiplier",
    confidence_intervals_required = TRUE){
  
  # Validate inputs
  if(!is.data.frame(df)){
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.data.frame(age_metadata)){
    stop(
      "`age_metadata` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.logical(confidence_intervals_required) ||
     length(confidence_intervals_required) != 1 ||
     is.na(confidence_intervals_required)){
    
    stop(
      "`confidence_intervals_required` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  required_cols <- c(
    value_type_code_col,
    numerator_col,
    denominator_col,
    value_multiplier_col,
    "age_group_code"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(df)
  )
  
  if(length(missing_cols) > 0){
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Required columns from age metadata 
  required_age_cols <- c(
    "indicator_id",
    "single_age_code"
  )
  
  missing_age_cols <- setdiff(
    required_age_cols,
    names(age_metadata)
  )
  
  if(length(missing_age_cols) > 0){
    stop(
      "Missing required age metadata columns: ",
      paste(missing_age_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Create DASR grouping keys
  dasr_keys <- c(
    "indicator_id",
    "start_date",
    "end_date",
    "imd_code",
    "aggregation_id",
    "sex_code",
    "ethnicity_code",
    "creation_date",
    value_type_code_col,
    "source_code",
    "time_period_type",
    "combination_id"
  )
  
  missing_keys <- setdiff(
    dasr_keys,
    names(df)
  )
  
  if(length(missing_keys) > 0){
    stop(
      "Missing DASR grouping columns: ",
      paste(missing_keys, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Age metadata must contain one row per indicator
  if(anyDuplicated(age_metadata$indicator_id)){
    stop(
      "`age_metadata` must contain one row per indicator.",
      call. = FALSE
    )
  }
  
  # Create ESP 2013 standard population lookup
  esp2013_lookup <- PHEindicatormethods::esp2013 |>
    dplyr::as_tibble() |>
    dplyr::rename(
      std_pop = dplyr::all_of("value")
    ) |>
    dplyr::mutate(
      age_group_code = as.integer(c(1:18, 18))
    ) |>
    dplyr::group_by(
      .data$age_group_code
    ) |>
    dplyr::summarise(
      std_pop = sum(.data$std_pop),
      .groups = "drop"
    )
  
  # Prepare DASR rows
  
  df_calc <- df |>
    dplyr::filter(
      .data[[value_type_code_col]] == 4L
    ) |>
    dplyr::mutate(
      numerator_dasr = .data[[numerator_col]],
      denominator_dasr = .data[[denominator_col]],
      multiplier_dasr = .data[[value_multiplier_col]]
    ) |>
    dplyr::left_join(
      esp2013_lookup,
      by = "age_group_code"
    )

  
  # Return empty result if there are no DASR rows
  if(nrow(df_calc) == 0){
    
    result <- df_calc |>
      dplyr::select(
        dplyr::all_of(dasr_keys)
      ) |>
      dplyr::mutate(
        total_count = numeric(),
        total_pop = numeric(),
        value = numeric(),
        lowercl = numeric(),
        uppercl = numeric(),
        age_group_code = integer()
      )
    
    return(result)
  }
  
  # Check all age groups matched the ESP 2013 lookup
  if(any(is.na(df_calc$std_pop))){
    stop(
      "Some age group codes could not be matched to the ESP 2013 population.",
      call. = FALSE
    )
  }
  
  # Value multiplier cannot be missing
  if(any(is.na(df_calc$multiplier_dasr))){
    stop(
      "Value multiplier cannot be missing for DASR calculations.",
      call. = FALSE
    )
  }
  
  # Check each DASR calculation has one multiplier
  multiplier_check <- df_calc |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(dasr_keys)
      )
    ) |>
    dplyr::summarise(
      n_multiplier = dplyr::n_distinct(
        .data$multiplier_dasr
      ),
      .groups = "drop"
    )
  
  if(any(multiplier_check$n_multiplier != 1)){
    stop(
      "Each DASR calculation must have exactly one value multiplier.",
      call. = FALSE
    )
  }
  
  # Get unique multiplier values
  multiplier_values <- unique(
    df_calc$multiplier_dasr
  )
  
  output_list <- vector(
    "list",
    length(multiplier_values)
  )
  
  # Calculate DASRs separately for each multiplier
  # because PHEindicatormethods::calculate_dsr() accepts multiplier as a single scalar value
  for(i in seq_along(multiplier_values)){
    
    multiplier_value <- multiplier_values[i]
    
    df_multiplier <- df_calc |>
      dplyr::filter(
        .data$multiplier_dasr == multiplier_value
      )
    
    output_list[[i]] <- df_multiplier |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(dasr_keys)
        )
      ) |>
      PHEindicatormethods::calculate_dsr(
        x = numerator_dasr,
        n = denominator_dasr,
        stdpop = std_pop,
        type = "standard",
        multiplier = multiplier_value
      ) |>
      dplyr::ungroup()
  }
  
  output <- dplyr::bind_rows(
    output_list
    ) |>  dplyr::rename_with(
    ~ numerator_col,
    dplyr::all_of("total_count")
  ) |>
    dplyr::rename_with(
      ~ denominator_col,
      dplyr::all_of("total_pop")
    )
  
  # Remove confidence intervals when not required
  if(!confidence_intervals_required){
    
    output <- output |>
      dplyr::mutate(
        lowercl = NA_real_,
        uppercl = NA_real_
      )
  }
  
  # Add final single age group code for each indicator_id
  result <- output |>
    dplyr::left_join(
      age_metadata |>
        dplyr::select(
          dplyr::all_of(
            c("indicator_id", "single_age_code")
          )
        ),
      by = "indicator_id"
    ) |>
    dplyr::mutate(
      age_group_code = .data$single_age_code
    ) |>
    dplyr::select(
      -dplyr::all_of("single_age_code")
    )
  
  result
}
