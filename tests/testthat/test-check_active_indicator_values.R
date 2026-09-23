# Helper data 

create_test_active_indicator_data <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L, 104L),
    indicator_value = c(10, NA, NA, 40),
    value_type_code = c(1L, 1L, 2L, 1L),
    denominator = c(100, 200, 0, 400)
  )
}


create_test_active_indicator_metadata <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L, 104L),
    status_code = c(1L, 1L, 1L, 2L)
  )
}


# Test 1: Unexpected missing value is identified 

testthat::test_that(
  "check_active_indicator_values identifies unexpected missing values",
  {
    
    df <- create_test_active_indicator_data()
    metadata <- create_test_active_indicator_metadata()
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
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
  }
)


# Test 2: Populated active indicators pass

testthat::test_that(
  "check_active_indicator_values passes when active indicator values are populated",
  {
    
    df <- create_test_active_indicator_data()
    metadata <- create_test_active_indicator_metadata()
    
    df$indicator_value[2] <- 20
    
    testthat::expect_message(
      result <- check_active_indicator_values(
        df,
        metadata
      ),
      "All active indicators have a populated indicator_value"
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 3: Inactive indicators are ignored 

testthat::test_that(
  "check_active_indicator_values ignores inactive indicators",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 1L,
      denominator = 100
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 2L
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 4: Percentage with zero denominator is allowed 

testthat::test_that(
  "check_active_indicator_values allows missing percentage value when denominator is zero",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 2L,
      denominator = 0
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 5: Percentage with missing denominator is allowed 

testthat::test_that(
  "check_active_indicator_values allows missing percentage value when denominator is missing",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 2L,
      denominator = NA_real_
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 6: Percentage change with zero denominator is allowed 

testthat::test_that(
  "check_active_indicator_values allows missing percentage change value when denominator is zero",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 9L,
      denominator = 0
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 7: Percentage point difference with zero denominator is allowed 

testthat::test_that(
  "check_active_indicator_values allows missing percentage point difference when denominator is zero",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 10L,
      denominator = 0
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 8: Expected value types still fail when denominator is valid 

testthat::test_that(
  "check_active_indicator_values flags missing values when denominator is populated",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      indicator_value = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      value_type_code = c(2L, 9L, 10L),
      denominator = c(100, 100, 100)
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L)
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      3L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      c(101L, 102L, 103L)
    )
  }
)


# Test 9: Other value types are not exempt 

testthat::test_that(
  "check_active_indicator_values does not exempt other value types with zero denominator",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 1L,
      denominator = 0
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
  }
)


# Test 10: Missing value_type_code is still flagged 

testthat::test_that(
  "check_active_indicator_values flags missing indicator value when value type is missing",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = NA_integer_,
      denominator = 100
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
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


# Test 11: Multiple failing indicators are returned 

testthat::test_that(
  "check_active_indicator_values returns all failing active indicators",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      indicator_value = c(
        NA_real_,
        NA_real_,
        30
      ),
      value_type_code = c(1L, 1L, 1L),
      denominator = c(100, 200, 300)
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L)
    )
    
    testthat::expect_message(
      result <- check_active_indicator_values(
        df,
        metadata
      ),
      "101, 102"
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


# Test 12: Error message is produced 

testthat::test_that(
  "check_active_indicator_values gives error message for failures",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 1L,
      denominator = 100
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    testthat::expect_message(
      check_active_indicator_values(
        df,
        metadata
      ),
      "Found active indicators with unexpected missing indicator_value"
    )
  }
)


# Test 13: Non-data-frame df is rejected 

testthat::test_that(
  "check_active_indicator_values rejects non-data-frame df",
  {
    
    metadata <- create_test_active_indicator_metadata()
    
    testthat::expect_error(
      check_active_indicator_values(
        c(1, 2, 3),
        metadata
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 14: Non-data-frame metadata is rejected

testthat::test_that(
  "check_active_indicator_values rejects non-data-frame metadata",
  {
    
    df <- create_test_active_indicator_data()
    
    testthat::expect_error(
      check_active_indicator_values(
        df,
        c(1, 2, 3)
      ),
      "`metadata` must be a data frame"
    )
  }
)


# Test 15: Missing df columns are reported 

testthat::test_that(
  "check_active_indicator_values reports missing df columns",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = 10
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L
    )
    
    testthat::expect_error(
      check_active_indicator_values(
        df,
        metadata
      ),
      "Missing required columns in `df`"
    )
  }
)


# Test 16: Missing metadata columns are reported 

testthat::test_that(
  "check_active_indicator_values reports missing metadata columns",
  {
    
    df <- create_test_active_indicator_data()
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L)
    )
    
    testthat::expect_error(
      check_active_indicator_values(
        df,
        metadata
      ),
      "Missing required columns in `metadata`: status_code"
    )
  }
)