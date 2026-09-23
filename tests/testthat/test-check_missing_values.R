# Helper data

create_test_missing_values_data <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L, 104L),
    start_date = as.Date(
      c(
        "2025-01-01",
        "2025-01-01",
        "2025-01-01",
        "2025-01-01"
      )
    ),
    end_date = as.Date(
      c(
        "2025-12-31",
        "2025-12-31",
        "2025-12-31",
        "2025-12-31"
      )
    ),
    indicator_value = c(10, 20, 30, 40),
    time_period_type = c(
      "1 year",
      "1 year",
      NA,
      "1 year"
    ),
    combination_id = c(
      4L,
      4L,
      4L,
      NA_integer_
    ),
    source_code = c(
      1L,
      NA_integer_,
      1L,
      1L
    )
  )
}


# Test 1: Default columns with missing values are identified

testthat::test_that(
  "check_missing_values identifies missing values in default columns",
  {
    
    df <- create_test_missing_values_data()
    
    suppressMessages(
      result <- check_missing_values(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      3L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      c(102L, 103L, 104L)
    )
  }
)


# Test 2: Missing column names are returned

testthat::test_that(
  "check_missing_values identifies which columns are missing",
  {
    
    df <- create_test_missing_values_data()
    
    suppressMessages(
      result <- check_missing_values(df)
    )
    
    testthat::expect_equal(
      result$missing_columns,
      c(
        "source_code",
        "time_period_type",
        "combination_id"
      )
    )
  }
)


# Test 3: More than one missing column is reported for the same row

testthat::test_that(
  "check_missing_values reports multiple missing columns for one row",
  {
    
    df <- create_test_missing_values_data()
    
    df$start_date[2] <- NA
    df$source_code[2] <- NA_integer_
    
    suppressMessages(
      result <- check_missing_values(
        df,
        cols = c(
          "start_date",
          "source_code"
        )
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      102L
    )
    
    testthat::expect_equal(
      result$missing_columns,
      "start_date, source_code"
    )
  }
)


# Test 4: Specific columns can be checked

testthat::test_that(
  "check_missing_values checks only specified columns",
  {
    
    df <- create_test_missing_values_data()
    
    suppressMessages(
      result <- check_missing_values(
        df,
        cols = "source_code"
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      102L
    )
    
    testthat::expect_equal(
      result$missing_columns,
      "source_code"
    )
  }
)


# Test 5: Columns not selected are not checked

testthat::test_that(
  "check_missing_values ignores missing values outside selected columns",
  {
    
    df <- create_test_missing_values_data()
    
    suppressMessages(
      result <- check_missing_values(
        df,
        cols = c(
          "indicator_id",
          "start_date",
          "end_date"
        )
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 6: Blank character values are identified

testthat::test_that(
  "check_missing_values identifies blank character values",
  {
    
    df <- create_test_missing_values_data()
    
    df$time_period_type[1] <- ""
    
    suppressMessages(
      result <- check_missing_values(
        df,
        cols = "time_period_type"
      )
    )
    
    testthat::expect_true(
      101L %in% result$indicator_id
    )
    
    testthat::expect_true(
      103L %in% result$indicator_id
    )
  }
)


# Test 7: Whitespace-only character values are identified

testthat::test_that(
  "check_missing_values identifies whitespace-only values",
  {
    
    df <- create_test_missing_values_data()
    
    df$time_period_type <- "1 year"
    df$time_period_type[2] <- "   "
    
    suppressMessages(
      result <- check_missing_values(
        df,
        cols = "time_period_type"
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      102L
    )
    
    testthat::expect_equal(
      result$missing_columns,
      "time_period_type"
    )
  }
)


# Test 8: No missing values returns an empty data frame

testthat::test_that(
  "check_missing_values returns empty data frame when no missing values exist",
  {
    
    df <- create_test_missing_values_data()
    
    df$time_period_type <- "1 year"
    df$combination_id <- 4L
    df$source_code <- 1L
    
    suppressMessages(
      result <- check_missing_values(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
    
    testthat::expect_true(
      is.data.frame(result)
    )
  }
)


# Test 9: PASS message is returned

testthat::test_that(
  "check_missing_values reports when no missing values are found",
  {
    
    df <- create_test_missing_values_data()
    
    df$time_period_type <- "1 year"
    df$combination_id <- 4L
    df$source_code <- 1L
    
    testthat::expect_message(
      check_missing_values(df),
      "No missing values in the checked columns"
    )
  }
)


# Test 10: WARNING message is returned

testthat::test_that(
  "check_missing_values reports when missing values are found",
  {
    
    df <- create_test_missing_values_data()
    
    testthat::expect_message(
      check_missing_values(df),
      "Found 3 row"
    )
  }
)


# Test 11: Warning identifies affected columns

testthat::test_that(
  "check_missing_values reports affected columns",
  {
    
    df <- create_test_missing_values_data()
    
    testthat::expect_message(
      check_missing_values(df),
      "Affected column\\(s\\): time_period_type, combination_id, source_code"
    )
  }
)


# Test 12: Returned rows retain original columns

testthat::test_that(
  "check_missing_values retains original columns and adds missing_columns",
  {
    
    df <- create_test_missing_values_data()
    
    suppressMessages(
      result <- check_missing_values(df)
    )
    
    testthat::expect_equal(
      names(result),
      c(
        names(df),
        "missing_columns"
      )
    )
  }
)


# Test 13: Non-data-frame input is rejected

testthat::test_that(
  "check_missing_values rejects non-data-frame input",
  {
    
    testthat::expect_error(
      check_missing_values(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 14: cols must be a character vector

testthat::test_that(
  "check_missing_values rejects non-character cols",
  {
    
    df <- create_test_missing_values_data()
    
    testthat::expect_error(
      check_missing_values(
        df,
        cols = c(1, 2)
      ),
      "`cols` must be a character vector containing at least one column name"
    )
  }
)


# Test 15: cols cannot be empty

testthat::test_that(
  "check_missing_values rejects empty cols",
  {
    
    df <- create_test_missing_values_data()
    
    testthat::expect_error(
      check_missing_values(
        df,
        cols = character(0)
      ),
      "`cols` must be a character vector containing at least one column name"
    )
  }
)


# Test 16: Requested columns must exist

testthat::test_that(
  "check_missing_values rejects columns that do not exist",
  {
    
    df <- create_test_missing_values_data()
    
    testthat::expect_error(
      check_missing_values(
        df,
        cols = c(
          "source_code",
          "missing_column"
        )
      ),
      "Columns not found in `df`: missing_column"
    )
  }
)