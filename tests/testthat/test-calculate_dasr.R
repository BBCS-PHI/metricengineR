create_test_dasr_rows <- function(
    indicator_id,
    age_codes,
    value_multiplier,
    combination_id,
    sex_code = 1L,
    imd_code = 1L,
    ethnicity_code = 1L){
  
  n_rows <- length(age_codes)
  
  data.frame(
    indicator_id = rep(as.integer(indicator_id), n_rows),
    start_date = rep(as.Date("2025-01-01"), n_rows),
    end_date = rep(as.Date("2025-12-31"), n_rows),
    numerator = seq(10, by = 2, length.out = n_rows),
    denominator = seq(10000, by = 500, length.out = n_rows),
    indicator_value = rep(NA_real_, n_rows),
    lower_ci95 = rep(NA_real_, n_rows),
    upper_ci95 = rep(NA_real_, n_rows),
    imd_code = rep(as.integer(imd_code), n_rows),
    aggregation_id = rep(151L, n_rows),
    age_group_code = as.integer(age_codes),
    sex_code = rep(as.integer(sex_code), n_rows),
    ethnicity_code = rep(as.integer(ethnicity_code), n_rows),
    creation_date = rep(as.Date("2026-01-01"), n_rows),
    value_type_code = rep(4L, n_rows),
    source_code = rep(1L, n_rows),
    value_multiplier = rep(value_multiplier, n_rows),
    time_period_type = rep("1 year", n_rows),
    combination_id = rep(as.integer(combination_id), n_rows)
  )
}

test_dasr_data <- dplyr::bind_rows(
  
  # Indicator 101: age groups 5-18
  create_test_dasr_rows(
    indicator_id = 101L,
    age_codes = 5:18,
    value_multiplier = 100000,
    combination_id = 1L,
    sex_code = 1L,
    imd_code = 1L,
    ethnicity_code = 1L
  ),
  
  # Indicator 102: age groups 10-18
  create_test_dasr_rows(
    indicator_id = 102L,
    age_codes = 10:18,
    value_multiplier = 100000,
    combination_id = 2L,
    sex_code = 2L,
    imd_code = 3L,
    ethnicity_code = 10L
  ),
  
  # Indicator 103: age groups 1-18
  create_test_dasr_rows(
    indicator_id = 103L,
    age_codes = 1:18,
    value_multiplier = 1000,
    combination_id = 3L,
    sex_code = 1L,
    imd_code = 5L,
    ethnicity_code = 21L
  )
)

age_metadata <- data.frame(
  indicator_id = c(
    101L,
    102L,
    103L
  ),
  single_age_code = c(
    31L,
    999L,
    71L
  )
)

# Test 1: Calculates one DASR for each indicator
testthat::test_that("calculate_dasr calculates DASRs for different age ranges", {
  
  result <- calculate_dasr(
    test_dasr_data,
    age_metadata
  )
  
  testthat::expect_equal(
    nrow(result),
    3
  )
  
  testthat::expect_setequal(
    result$indicator_id,
    c(101L, 102L, 103L)
  )
  
  testthat::expect_true(
    all(!is.na(result$value))
  )
  
  testthat::expect_true(
    all(!is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(!is.na(result$uppercl))
  )
})

# Test 2: DASR for age groups 5-18 matches PHEindicatormethods::calculate_dsr()
testthat::test_that("calculate_dasr matches PHE calculate_dsr for age groups 5 to 18", {
  
  df_5_18 <- test_dasr_data |>
    dplyr::filter(
      .data$indicator_id == 101L
    )
  
  esp2013_lookup <- PHEindicatormethods::esp2013 |>
    dplyr::as_tibble() |>
    dplyr::rename(
      std_pop = value
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
  
  expected <- df_5_18 |>
    dplyr::left_join(
      esp2013_lookup,
      by = "age_group_code"
    ) |>
    PHEindicatormethods::calculate_dsr(
      x = numerator,
      n = denominator,
      stdpop = std_pop,
      type = "standard",
      multiplier = 100000
    )
  
  result <- calculate_dasr(
    test_dasr_data,
    age_metadata
  ) |>
    dplyr::filter(
      .data$indicator_id == 101L
    )
  
  testthat::expect_equal(
    result$value,
    expected$value
  )
  
  testthat::expect_equal(
    result$lowercl,
    expected$lowercl
  )
  
  testthat::expect_equal(
    result$uppercl,
    expected$uppercl
  )
})

# Test 3: Different age ranges are calculated separately
testthat::test_that("calculate_dasr keeps different DASR groups separate", {
  
  result <- calculate_dasr(
    test_dasr_data,
    age_metadata
  )
  
  result_5_18 <- result |>
    dplyr::filter(
      .data$indicator_id == 101L
    )
  
  result_10_18 <- result |>
    dplyr::filter(
      .data$indicator_id == 102L
    )
  
  testthat::expect_equal(
    nrow(result_5_18),
    1
  )
  
  testthat::expect_equal(
    nrow(result_10_18),
    1
  )
  
  testthat::expect_false(
    result_5_18$value == result_10_18$value
  )
})

# Test 4: Different multipliers are supported
testthat::test_that("calculate_dasr supports different value multipliers", {
  
  df_1000 <- create_test_dasr_rows(
    indicator_id = 101L,
    age_codes = 5:18,
    value_multiplier = 1000,
    combination_id = 4
  )
  
  df_100000 <- create_test_dasr_rows(
    indicator_id = 102L,
    age_codes = 5:18,
    value_multiplier = 100000,
    combination_id = 4
  )
  
  df <- rbind(
    df_1000,
    df_100000
  )
  
  metadata <- data.frame(
    indicator_id = c(
      101L,
      102L
    ),
    single_age_code = c(
      31L,
      999L
    )
  )
  
  result <- calculate_dasr(
    df,
    metadata
  )
  
  value_1000 <- result$value[
    result$indicator_id == 101L
  ]
  
  value_100000 <- result$value[
    result$indicator_id == 102L
  ]
  
  testthat::expect_equal(
    value_100000,
    value_1000 * 100
  )
})

# Test 5: Confidence intervals can be removed
testthat::test_that("calculate_dasr removes confidence intervals when not required", {
  
  result <- calculate_dasr(
    test_dasr_data,
    age_metadata,
    confidence_intervals_required = FALSE
  )
  
  testthat::expect_true(
    all(!is.na(result$value))
  )
  
  testthat::expect_true(
    all(is.na(result$lowercl))
  )
  
  testthat::expect_true(
    all(is.na(result$uppercl))
  )
})

# Test 6: Only DASR rows are calculated
testthat::test_that("calculate_dasr filters to value type code 4", {
  
  non_dasr <- test_dasr_data[1, ]
  
  non_dasr$indicator_id <- 1000L
  non_dasr$value_type_code <- 2L
  
  df <- rbind(
    test_dasr_data,
    non_dasr
  )
  
  result <- calculate_dasr(
    df,
    age_metadata
  )
  
  testthat::expect_equal(
    nrow(result),
    3
  )
  
  testthat::expect_false(
    1000L %in% result$indicator_id
  )
})

# Test 7: Final age group comes from age_metadata
testthat::test_that("calculate_dasr adds final age group code from metadata", {
  
  result <- calculate_dasr(
    test_dasr_data,
    age_metadata
  ) |>
    dplyr::arrange(
      .data$indicator_id
    )
  
  expected <- age_metadata |>
    dplyr::arrange(
      .data$indicator_id
    )
  
  testthat::expect_equal(
    result$age_group_code,
    expected$single_age_code
  )
})

# Test 8: Inconsistent multipliers within one DASR are rejected
testthat::test_that("calculate_dasr rejects inconsistent multipliers within a DASR", {
  
  df <- test_dasr_data
  
  rows <- which(
    df$indicator_id == 101L
  )
  
  df$value_multiplier[rows[1]] <- 1000
  
  testthat::expect_error(
    calculate_dasr(
      df,
      age_metadata
    ),
    "Each DASR calculation must have exactly one value multiplier"
  )
})

# Test 9: Missing value multipliers are rejected
testthat::test_that("calculate_dasr rejects missing value multipliers", {
  
  df <- test_dasr_data
  
  df$value_multiplier[1] <- NA
  
  testthat::expect_error(
    calculate_dasr(
      df,
      age_metadata
    ),
    "Value multiplier cannot be missing"
  )
})

# Test 10: Unknown age groups are rejected
testthat::test_that("calculate_dasr rejects age groups not found in ESP 2013", {
  
  df <- test_dasr_data
  
  df$age_group_code[1] <- 99L
  
  testthat::expect_error(
    calculate_dasr(
      df,
      age_metadata
    ),
    "Some age group codes could not be matched"
  )
})

# Test 11: Missing required columns are reported
testthat::test_that("calculate_dasr reports missing required columns", {
  
  df <- test_dasr_data |>
    dplyr::select(
      -denominator
    )
  
  testthat::expect_error(
    calculate_dasr(
      df,
      age_metadata
    ),
    "Missing required columns: denominator"
  )
})

# Test 12: Invalid confidence interval argument is rejected
testthat::test_that("calculate_dasr validates confidence_intervals_required", {
  
  testthat::expect_error(
    calculate_dasr(
      test_dasr_data,
      age_metadata,
      confidence_intervals_required = "TRUE"
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
  
  testthat::expect_error(
    calculate_dasr(
      test_dasr_data,
      age_metadata,
      confidence_intervals_required = NA
    ),
    "`confidence_intervals_required` must be TRUE or FALSE"
  )
})

# Test 13: No DASR rows returns an empty result
testthat::test_that("calculate_dasr handles data with no DASR rows", {
  
  df <- test_dasr_data
  
  df$value_type_code <- 2L
  
  result <- calculate_dasr(
    df,
    age_metadata
  )
  
  testthat::expect_equal(
    nrow(result),
    0
  )
})

# Test 14: calculate_dasr renames total_count and total_pop
test_that("calculate_dasr renames total_count and total_pop", {
  
  df <- data.frame(
    indicator_id = rep(101L, 18),
    start_date = rep(as.Date("2025-01-01"), 18),
    end_date = rep(as.Date("2025-12-31"), 18),
    numerator = rep(1, 18),
    denominator = rep(1000, 18),
    value_multiplier = rep(100000, 18),
    age_group_code = 1:18,
    imd_code = rep(999L, 18),
    aggregation_id = rep(1L, 18),
    sex_code = rep(999L, 18),
    ethnicity_code = rep(999L, 18),
    creation_date = rep(as.Date("2026-01-01"), 18),
    value_type_code = rep(4L, 18),
    source_code = rep(1L, 18),
    time_period_type = rep("1 year", 18),
    combination_id = rep(4L, 18)
  )
  
  age_metadata <- data.frame(
    indicator_id = 101L,
    single_age_code = 999L
  )
  
  result <- calculate_dasr(
    df = df,
    age_metadata = age_metadata
  )
  
  
  # Check renamed columns exist
  
  expect_true(
    "numerator" %in% names(result)
  )
  
  expect_true(
    "denominator" %in% names(result)
  )
  
  
  # Check original PHE output column names have been removed
  
  expect_false(
    "total_count" %in% names(result)
  )
  
  expect_false(
    "total_pop" %in% names(result)
  )
  
  
  # Check values were retained during renaming
  
  expect_equal(
    result$numerator,
    18
  )
  
  expect_equal(
    result$denominator,
    18000
  )
})