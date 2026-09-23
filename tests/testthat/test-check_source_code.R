# Helper data

create_test_source_code_data <- function() {
  
  data.frame(
    indicator_id = c(
      101L, 101L,
      102L, 102L,
      103L
    ),
    source_code = c(
      1L, 1L,
      2L, 2L,
      3L
    )
  )
}


# Test 1: One source code per indicator passes

testthat::test_that(
  "check_source_code passes when each indicator has one source code",
  {
    
    df <- create_test_source_code_data()
    
    testthat::expect_message(
      result <- check_source_code(df),
      "No indicators have more than one source code."
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 2: Multiple source codes are identified

testthat::test_that(
  "check_source_code identifies indicators with multiple source codes",
  {
    
    df <- create_test_source_code_data()
    
    df$source_code[2] <- 2L
    
    suppressMessages(
      result <- check_source_code(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      101L
    )
    
    testthat::expect_equal(
      result$n_source_codes,
      2L
    )
  }
)


# Test 3: Source codes are returned for failing indicators

testthat::test_that(
  "check_source_code returns the source codes for failing indicators",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      source_code = c(1L, 2L)
    )
    
    suppressMessages(
      result <- check_source_code(df)
    )
    
    testthat::expect_equal(
      result$source_codes,
      "1, 2"
    )
  }
)


# Test 4: Repeated rows with the same source do not fail

testthat::test_that(
  "check_source_code allows repeated rows with the same source code",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L, 101L),
      source_code = c(1L, 1L, 1L)
    )
    
    suppressMessages(
      result <- check_source_code(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 5: Multiple failing indicators are returned

testthat::test_that(
  "check_source_code returns all indicators with multiple source codes",
  {
    
    df <- data.frame(
      indicator_id = c(
        101L, 101L,
        102L, 102L,
        103L
      ),
      source_code = c(
        1L, 2L,
        3L, 4L,
        5L
      )
    )
    
    suppressMessages(
      result <- check_source_code(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      2L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      c(101L, 102L)
    )
  }
)


# Test 6: Error message is returned

testthat::test_that(
  "check_source_code reports indicators with multiple source codes",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      source_code = c(1L, 2L)
    )
    
    testthat::expect_message(
      check_source_code(df),
      "1 indicator\\(s\\) have more than one source code"
    )
  }
)


# Test 7: Missing source codes are ignored by this check

testthat::test_that(
  "check_source_code does not treat missing source codes as multiple sources",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      source_code = c(1L, NA_integer_)
    )
    
    suppressMessages(
      result <- check_source_code(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 8: Custom column names can be used

testthat::test_that(
  "check_source_code accepts custom column names",
  {
    
    df <- data.frame(
      metric_id = c(101L, 101L),
      source_id = c(1L, 2L)
    )
    
    suppressMessages(
      result <- check_source_code(
        df,
        indicator_col = "metric_id",
        source_code_col = "source_id"
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
    
    testthat::expect_equal(
      result$metric_id,
      101L
    )
  }
)


# Test 9: Non-data-frame input is rejected

testthat::test_that(
  "check_source_code rejects non-data-frame input",
  {
    
    testthat::expect_error(
      check_source_code(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 10: Missing indicator column is reported

testthat::test_that(
  "check_source_code reports missing indicator column",
  {
    
    df <- data.frame(
      source_code = c(1L, 2L)
    )
    
    testthat::expect_error(
      check_source_code(df),
      "Columns not found in `df`: indicator_id"
    )
  }
)


# Test 11: Missing source code column is reported

testthat::test_that(
  "check_source_code reports missing source code column",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L)
    )
    
    testthat::expect_error(
      check_source_code(df),
      "Columns not found in `df`: source_code"
    )
  }
)