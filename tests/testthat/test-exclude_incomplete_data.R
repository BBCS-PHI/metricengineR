# Helper function to create test data

create_test_reporting_data <- function(
    period_type = "Calendar",
    years = 2022:2026,
    time_period_type = "1 year"
) {
  
  if(period_type == "Calendar"){
    
    start_date <- as.Date(
      paste0(years, "-01-01")
    )
    
    end_date <- as.Date(
      paste0(years, "-12-31")
    )
    
  } else if(period_type == "Financial"){
    
    start_date <- as.Date(
      paste0(years, "-04-01")
    )
    
    end_date <- as.Date(
      paste0(years + 1L, "-03-31")
    )
  }
  
  data.frame(
    indicator_id = rep(101L, length(years)),
    period_type = rep(period_type, length(years)),
    time_period_type = rep(time_period_type, length(years)),
    start_date = start_date,
    end_date = end_date
  )
}

# Test 1: Incomplete Calendar year is excluded 

testthat::test_that(
  "exclude_incomplete_data excludes incomplete Calendar years",
  {
    
    df <- create_test_reporting_data(
      period_type = "Calendar",
      years = 2022:2026
    )
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16")
    )
    
    testthat::expect_equal(
      nrow(result),
      4L
    )
    
    testthat::expect_equal(
      max(result$end_date),
      as.Date("2025-12-31")
    )
    
    testthat::expect_false(
      as.Date("2026-12-31") %in% result$end_date
    )
  }
)


# Test 2: Completed Financial year is retained 

testthat::test_that(
  "exclude_incomplete_data identifies completed Financial years",
  {
    
    df <- create_test_reporting_data(
      period_type = "Financial",
      years = 2022:2026
    )
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16")
    )
    
    testthat::expect_equal(
      nrow(result),
      4L
    )
    
    testthat::expect_equal(
      max(result$end_date),
      as.Date("2026-03-31")
    )
    
    testthat::expect_false(
      as.Date("2027-03-31") %in% result$end_date
    )
  }
)


# Test 3: Incomplete month is excluded 

testthat::test_that(
  "exclude_incomplete_data excludes the current incomplete month",
  {
    
    df <- data.frame(
      indicator_id = rep(101L, 5),
      period_type = rep("Calendar", 5),
      time_period_type = rep("Monthly", 5),
      start_date = as.Date(
        c(
          "2026-05-01",
          "2026-06-01",
          "2026-07-01",
          "2026-08-01",
          "2026-09-01"
        )
      ),
      end_date = as.Date(
        c(
          "2026-05-31",
          "2026-06-30",
          "2026-07-31",
          "2026-08-31",
          "2026-09-30"
        )
      )
    )
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16")
    )
    
    testthat::expect_equal(
      nrow(result),
      4L
    )
    
    testthat::expect_equal(
      max(result$end_date),
      as.Date("2026-08-31")
    )
  }
)


# Test 4: Invalid duration is excluded 

testthat::test_that(
  "exclude_incomplete_data excludes records with unusual duration",
  {
    
    df <- create_test_reporting_data(
      period_type = "Calendar",
      years = 2022:2025
    )
    
    # Make the 2024 period much shorter than the others
    df$start_date[
      df$end_date == as.Date("2024-12-31")
    ] <- as.Date("2024-06-01")
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16"),
      enforce_duration = TRUE
    )
    
    testthat::expect_equal(
      nrow(result),
      3L
    )
    
    testthat::expect_false(
      as.Date("2024-12-31") %in% result$end_date
    )
  }
)


# Test 5: Duration checking can be disabled 

testthat::test_that(
  "exclude_incomplete_data can disable duration checking",
  {
    
    df <- create_test_reporting_data(
      period_type = "Calendar",
      years = 2022:2025
    )
    
    df$start_date[
      df$end_date == as.Date("2024-12-31")
    ] <- as.Date("2024-06-01")
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16"),
      enforce_duration = FALSE
    )
    
    testthat::expect_equal(
      nrow(result),
      4L
    )
    
    testthat::expect_true(
      as.Date("2024-12-31") %in% result$end_date
    )
  }
)


# Test 6: Missing end dates are excluded 

testthat::test_that(
  "exclude_incomplete_data excludes missing end dates",
  {
    
    df <- create_test_reporting_data(
      years = 2022:2025
    )
    
    df$end_date[2] <- as.Date(NA)
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16")
    )
    
    testthat::expect_equal(
      nrow(result),
      3L
    )
    
    testthat::expect_false(
      any(is.na(result$end_date))
    )
  }
)


# Test 7: Custom column names work 

testthat::test_that(
  "exclude_incomplete_data works with custom column names",
  {
    
    df <- create_test_reporting_data(
      years = 2022:2026
    ) |>
      dplyr::rename(
        reporting_type = period_type,
        period_start = start_date,
        period_end = end_date,
        reporting_period = time_period_type
      )
    
    result <- exclude_incomplete_data(
      df = df,
      period_type_col = "reporting_type",
      start_date_col = "period_start",
      end_date_col = "period_end",
      time_period_col = "reporting_period",
      indicator_col = "indicator_id",
      current_date = as.Date("2026-09-16")
    )
    
    testthat::expect_equal(
      nrow(result),
      4L
    )
    
    testthat::expect_equal(
      max(result$period_end),
      as.Date("2025-12-31")
    )
  }
)


# Test 8: Temporary columns are removed 

testthat::test_that(
  "exclude_incomplete_data does not return temporary columns",
  {
    
    df <- create_test_reporting_data()
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16")
    )
    
    temporary_columns <- c(
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
    
    testthat::expect_false(
      any(
        temporary_columns %in% names(result)
      )
    )
  }
)


# Test 9: Original columns are retained 

testthat::test_that(
  "exclude_incomplete_data retains original columns",
  {
    
    df <- create_test_reporting_data()
    
    result <- exclude_incomplete_data(
      df = df,
      time_period_col = "time_period_type",
      current_date = as.Date("2026-09-16")
    )
    
    testthat::expect_equal(
      names(result),
      names(df)
    )
  }
)


# Test 10: Exclusion message is returned 

testthat::test_that(
  "exclude_incomplete_data reports the number of excluded rows",
  {
    
    df <- create_test_reporting_data(
      years = 2022:2026
    )
    
    testthat::expect_message(
      exclude_incomplete_data(
        df = df,
        time_period_col = "time_period_type",
        current_date = as.Date("2026-09-16")
      ),
      "Excluded 1 row"
    )
  }
)


# Test 11: Non-data-frame input is rejected 

testthat::test_that(
  "exclude_incomplete_data rejects non-data-frame input",
  {
    
    testthat::expect_error(
      exclude_incomplete_data(
        df = c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 12: Missing required columns are reported 

testthat::test_that(
  "exclude_incomplete_data reports missing required columns",
  {
    
    df <- create_test_reporting_data() |>
      dplyr::select(
        -end_date
      )
    
    testthat::expect_error(
      exclude_incomplete_data(
        df = df
      ),
      "Missing required columns: end_date"
    )
  }
)


# Test 13: Optional time period column is validated when supplied 

testthat::test_that(
  "exclude_incomplete_data validates supplied time period column",
  {
    
    df <- create_test_reporting_data()
    
    testthat::expect_error(
      exclude_incomplete_data(
        df = df,
        time_period_col = "missing_time_period"
      ),
      "Missing required columns: missing_time_period"
    )
  }
)


# Test 14: current_date must be a Date 

testthat::test_that(
  "exclude_incomplete_data validates current_date",
  {
    
    df <- create_test_reporting_data()
    
    testthat::expect_error(
      exclude_incomplete_data(
        df = df,
        current_date = "2026-09-16"
      ),
      "`current_date` must be a Date"
    )
  }
)


# Test 15: enforce_duration must be logical 

testthat::test_that(
  "exclude_incomplete_data validates enforce_duration",
  {
    
    df <- create_test_reporting_data()
    
    testthat::expect_error(
      exclude_incomplete_data(
        df = df,
        enforce_duration = "TRUE"
      ),
      "`enforce_duration` must be TRUE or FALSE"
    )
    
    testthat::expect_error(
      exclude_incomplete_data(
        df = df,
        enforce_duration = c(TRUE, FALSE)
      ),
      "`enforce_duration` must be TRUE or FALSE"
    )
    
    testthat::expect_error(
      exclude_incomplete_data(
        df = df,
        enforce_duration = NA
      ),
      "`enforce_duration` must be TRUE or FALSE"
    )
  }
)