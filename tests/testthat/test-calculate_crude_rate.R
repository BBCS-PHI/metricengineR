# Test 1: Basic crude rate calculation without confidence intervals
# Checks that eligible rows calculate correctly and CIs remain NA.
testthat::test_that(
  "calculate_crude_rate calculates crude rates without confidence intervals",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      value_type_code = c(3L, 3L),
      numerator = c(50, 25),
      denominator = c(1000, 500),
      value_multiplier = c(100000, 100000)
    )
    
    result <- calculate_crude_rate(df)
    
    testthat::expect_equal(
      result$value,
      c(5000, 5000)
    )
    
    testthat::expect_true(
      all(is.na(result$lowercl))
    )
    
    testthat::expect_true(
      all(is.na(result$uppercl))
    )
  }
)


# Test 2: Only crude rate rows are returned
# Checks that other value types are excluded.
testthat::test_that(
  "calculate_crude_rate filters to crude rate value type",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L, 104L),
      value_type_code = c(3L, 2L, 1L, 3L),
      numerator = c(50, 20, 30, 40),
      denominator = c(1000, 100, 100, 800),
      value_multiplier = c(100000, 100, 1, 100000)
    )
    
    result <- calculate_crude_rate(df)
    
    testthat::expect_equal(
      nrow(result),
      2
    )
    
    testthat::expect_true(
      all(result$value_type_code == 3L)
    )
  }
)


# Test 3: Ineligible rows are retained with NA values
# Checks missing numerator, zero denominator, missing denominator,
# and missing value multiplier.
testthat::test_that(
  "calculate_crude_rate retains ineligible rows with NA values",
  {
    
    df <- data.frame(
      indicator_id = rep(101L, 5),
      value_type_code = rep(3L, 5),
      numerator = c(50, NA, 20, 30, 40),
      denominator = c(1000, 1000, 0, NA, 800),
      value_multiplier = c(100000, 100000, 100000, 100000, NA)
    )
    
    result <- calculate_crude_rate(df)
    
    testthat::expect_equal(
      nrow(result),
      5
    )
    
    testthat::expect_equal(
      result$value[1],
      5000
    )
    
    testthat::expect_true(
      all(is.na(result$value[2:5]))
    )
  }
)


# Test 4: Negative numerators are rejected
# Crude-rate numerators represent event counts and cannot be negative.
testthat::test_that(
  "calculate_crude_rate rejects negative numerators",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      value_type_code = 3L,
      numerator = -10,
      denominator = 1000,
      value_multiplier = 100000
    )
    
    testthat::expect_error(
      calculate_crude_rate(df),
      "Numerator cannot be negative"
    )
  }
)


# Test 5: Custom column names work
# Checks that the function is not dependent on default column names.
testthat::test_that(
  "calculate_crude_rate works with custom column names",
  {
    
    df <- data.frame(
      metric_id = c(101L, 101L),
      type_code = c(3L, 3L),
      events = c(10, 20),
      population = c(1000, 2000),
      multiplier = c(100000, 100000)
    )
    
    result <- calculate_crude_rate(
      df,
      indicator_id_col = "metric_id",
      value_type_code_col = "type_code",
      numerator_col = "events",
      denominator_col = "population",
      value_multiplier_col = "multiplier"
    )
    
    testthat::expect_equal(
      result$value,
      c(1000, 1000)
    )
  }
)


# Test 6: Confidence intervals are calculated for valid crude rates
# Checks the phe_rate() branch and confirms CIs are returned.
testthat::test_that(
  "calculate_crude_rate calculates confidence intervals",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      value_type_code = c(3L, 3L),
      numerator = c(50, 100),
      denominator = c(10000, 20000),
      value_multiplier = c(100000, 100000)
    )
    
    result <- calculate_crude_rate(
      df,
      confidence_intervals_required = TRUE
    )
    
    testthat::expect_equal(
      result$value,
      c(500, 500)
    )
    
    testthat::expect_true(
      all(!is.na(result$lowercl))
    )
    
    testthat::expect_true(
      all(!is.na(result$uppercl))
    )
    
    testthat::expect_true(
      all(result$lowercl <= result$value)
    )
    
    testthat::expect_true(
      all(result$uppercl >= result$value)
    )
  }
)


# Test 7: Ineligible rows remain NA when confidence intervals are requested
# Checks that rows with denominator = 0 are retained rather than removed.
testthat::test_that(
  "calculate_crude_rate retains ineligible rows when confidence intervals are required",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      value_type_code = c(3L, 3L),
      numerator = c(50, 20),
      denominator = c(10000, 0),
      value_multiplier = c(100000, 100000)
    )
    
    result <- calculate_crude_rate(
      df,
      confidence_intervals_required = TRUE
    )
    
    testthat::expect_equal(
      nrow(result),
      2
    )
    
    testthat::expect_false(
      is.na(result$value[1])
    )
    
    testthat::expect_true(
      is.na(result$value[2])
    )
    
    testthat::expect_true(
      is.na(result$lowercl[2])
    )
    
    testthat::expect_true(
      is.na(result$uppercl[2])
    )
  }
)


# Test 8: Multiple multipliers within the same indicator are rejected
# Each indicator must have one unique value multiplier.
testthat::test_that(
  "calculate_crude_rate rejects multiple multipliers within an indicator",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      value_type_code = c(3L, 3L),
      numerator = c(50, 100),
      denominator = c(10000, 20000),
      value_multiplier = c(1000, 100000)
    )
    
    testthat::expect_error(
      calculate_crude_rate(
        df,
        confidence_intervals_required = TRUE
      ),
      "More than one value multiplier found for indicator_id\\(s\\): 101"
    )
  }
)


# Test 9: Different indicators can use different value multipliers
# Checks that each indicator uses its own metadata-defined multiplier
# when confidence intervals are calculated.
testthat::test_that(
  "calculate_crude_rate allows different multipliers across indicators",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      value_type_code = c(3L, 3L),
      numerator = c(50, 100),
      denominator = c(10000, 20000),
      value_multiplier = c(1000, 100000)
    )
    
    result <- calculate_crude_rate(
      df,
      confidence_intervals_required = TRUE
    )
    
    testthat::expect_equal(
      result$value,
      c(5, 500)
    )
    
    testthat::expect_true(
      all(!is.na(result$lowercl))
    )
    
    testthat::expect_true(
      all(!is.na(result$uppercl))
    )
  }
)


# Test 10: Different indicators can use different multipliers without CIs
# Checks that each row uses its indicator's value multiplier.
testthat::test_that(
  "calculate_crude_rate allows different multipliers without confidence intervals",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      value_type_code = c(3L, 3L),
      numerator = c(10, 10),
      denominator = c(1000, 1000),
      value_multiplier = c(1000, 100000)
    )
    
    result <- calculate_crude_rate(
      df,
      confidence_intervals_required = FALSE
    )
    
    testthat::expect_equal(
      result$value,
      c(10, 1000)
    )
  }
)


# Test 11: Eligible rows must have an indicator ID
# Checks that an indicator ID is required for crude-rate calculations.
testthat::test_that(
  "calculate_crude_rate rejects missing indicator IDs for eligible rows",
  {
    
    df <- data.frame(
      indicator_id = NA_integer_,
      value_type_code = 3L,
      numerator = 50,
      denominator = 10000,
      value_multiplier = 100000
    )
    
    testthat::expect_error(
      calculate_crude_rate(df),
      "`indicator_id` cannot be missing"
    )
  }
)


# Test 12: Non-data-frame inputs are rejected
testthat::test_that(
  "calculate_crude_rate rejects non-data-frame input",
  {
    
    testthat::expect_error(
      calculate_crude_rate(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 13: Missing required columns are reported
testthat::test_that(
  "calculate_crude_rate errors when required columns are missing",
  {
    
    df <- data.frame(
      value_type_code = 3L,
      numerator = 50
    )
    
    testthat::expect_error(
      calculate_crude_rate(df),
      "Missing required columns: indicator_id, denominator, value_multiplier"
    )
  }
)


# Test 14: confidence_intervals_required must be one TRUE or FALSE
testthat::test_that(
  "calculate_crude_rate validates confidence_intervals_required",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      value_type_code = 3L,
      numerator = 50,
      denominator = 10000,
      value_multiplier = 100000
    )
    
    testthat::expect_error(
      calculate_crude_rate(
        df,
        confidence_intervals_required = "TRUE"
      ),
      "`confidence_intervals_required` must be TRUE or FALSE"
    )
    
    testthat::expect_error(
      calculate_crude_rate(
        df,
        confidence_intervals_required = c(TRUE, FALSE)
      ),
      "`confidence_intervals_required` must be TRUE or FALSE"
    )
    
    testthat::expect_error(
      calculate_crude_rate(
        df,
        confidence_intervals_required = NA
      ),
      "`confidence_intervals_required` must be TRUE or FALSE"
    )
  }
)


# Test 15: No eligible rows can still be returned when CIs are requested
# Checks that phe_rate() is not called when nothing can be calculated.
testthat::test_that(
  "calculate_crude_rate handles no eligible rows with confidence intervals",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      value_type_code = c(3L, 3L),
      numerator = c(20, NA),
      denominator = c(0, 10000),
      value_multiplier = c(100000, 100000)
    )
    
    result <- calculate_crude_rate(
      df,
      confidence_intervals_required = TRUE
    )
    
    testthat::expect_equal(
      nrow(result),
      2
    )
    
    testthat::expect_true(
      all(is.na(result$value))
    )
    
    testthat::expect_true(
      all(is.na(result$lowercl))
    )
    
    testthat::expect_true(
      all(is.na(result$uppercl))
    )
  }
)


# Test 16: No crude-rate rows returns an empty result
# Checks that calculated columns are still returned.
testthat::test_that(
  "calculate_crude_rate handles no crude rate rows",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      value_type_code = c(1L, 2L),
      numerator = c(10, 20),
      denominator = c(100, 200),
      value_multiplier = c(1, 100)
    )
    
    result <- calculate_crude_rate(df)
    
    testthat::expect_equal(
      nrow(result),
      0
    )
    
    testthat::expect_true(
      all(
        c(
          "value",
          "lowercl",
          "uppercl"
        ) %in% names(result)
      )
    )
  }
)