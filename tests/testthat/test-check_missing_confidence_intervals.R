# Helper data 

create_test_confidence_interval_data <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L),
    indicator_value = c(10, 20, 30),
    value_type_code = c(2L, 4L, 2L),
    lower_ci95 = c(8, 18, 28),
    upper_ci95 = c(12, 22, 32)
  )
}


# Test 1: Passes when all confidence intervals are populated 

testthat::test_that(
  "check_missing_confidence_intervals passes when all confidence intervals are populated",
  {
    
    df <- create_test_confidence_interval_data()
    
    testthat::expect_message(
      result <- check_missing_confidence_intervals(df),
      "No missing confidence intervals"
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 2: Missing lower confidence interval is identified 

testthat::test_that(
  "check_missing_confidence_intervals identifies missing lower confidence interval",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$lower_ci95[2] <- NA_real_
    
    suppressMessages(
      result <- check_missing_confidence_intervals(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      102L
    )
  }
)


# Test 3: Missing upper confidence interval is identified 

testthat::test_that(
  "check_missing_confidence_intervals identifies missing upper confidence interval",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$upper_ci95[1] <- NA_real_
    
    suppressMessages(
      result <- check_missing_confidence_intervals(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      101L
    )
  }
)


# Test 4: Missing both confidence intervals is identified 

testthat::test_that(
  "check_missing_confidence_intervals identifies rows where both confidence intervals are missing",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$lower_ci95[3] <- NA_real_
    df$upper_ci95[3] <- NA_real_
    
    suppressMessages(
      result <- check_missing_confidence_intervals(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      103L
    )
  }
)


# Test 5: Missing confidence intervals are ignored when indicator value is NA

testthat::test_that(
  "check_missing_confidence_intervals ignores rows with missing indicator value",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$indicator_value[1] <- NA_real_
    df$lower_ci95[1] <- NA_real_
    df$upper_ci95[1] <- NA_real_
    
    suppressMessages(
      result <- check_missing_confidence_intervals(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 6: Multiple failing rows are returned 

testthat::test_that(
  "check_missing_confidence_intervals returns all failing rows",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$lower_ci95[1] <- NA_real_
    df$upper_ci95[3] <- NA_real_
    
    suppressMessages(
      result <- check_missing_confidence_intervals(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      2L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      c(101L, 103L)
    )
  }
)


# Test 7: Warning message identifies failing indicators 

testthat::test_that(
  "check_missing_confidence_intervals reports failing indicator IDs",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$lower_ci95[1] <- NA_real_
    df$upper_ci95[2] <- NA_real_
    
    testthat::expect_message(
      result <- check_missing_confidence_intervals(df),
      "Indicator ID\\(s\\): 101, 102"
    )
    
    testthat::expect_equal(
      nrow(result),
      2L
    )
  }
)


# Test 8: Warning message is produced when confidence intervals are missing 

testthat::test_that(
  "check_missing_confidence_intervals gives warning message for failures",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$lower_ci95[1] <- NA_real_
    
    testthat::expect_message(
      check_missing_confidence_intervals(df),
      "Some rows have missing confidence intervals"
    )
  }
)


# Test 9: Returned rows retain original columns 

testthat::test_that(
  "check_missing_confidence_intervals retains original columns",
  {
    
    df <- create_test_confidence_interval_data()
    
    df$lower_ci95[1] <- NA_real_
    
    suppressMessages(
      result <- check_missing_confidence_intervals(df)
    )
    
    testthat::expect_equal(
      names(result),
      names(df)
    )
  }
)


# Test 10: Non-data-frame input is rejected 

testthat::test_that(
  "check_missing_confidence_intervals rejects non-data-frame input",
  {
    
    testthat::expect_error(
      check_missing_confidence_intervals(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 11: Missing required columns are reported 

testthat::test_that(
  "check_missing_confidence_intervals reports missing required columns",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = 10,
      value_type_code = 2L
    )
    
    testthat::expect_error(
      check_missing_confidence_intervals(df),
      "Missing required columns"
    )
  }
)