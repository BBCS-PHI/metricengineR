# Helper data

create_test_active_indicator_data <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L, 104L),
    indicator_value = c(10, NA, NA, 40),
    value_type_code = c(1L, 1L, 2L, 1L),
    numerator = c(10, 20, 30, 40),
    denominator = c(100, 200, 0, 400)
  )
}


create_test_active_indicator_metadata <- function() {
  
  data.frame(
    indicator_id = c(101L, 102L, 103L, 104L),
    status_code = c(1L, 1L, 1L, 2L),
    precalculated = c("No", "No", "No", "No")
  )
}


# Test 1: Unexpected missing indicator value is identified

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
      numerator = 10,
      denominator = 100
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 2L,
      precalculated = "No"
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


# Test 4: Missing numerator is allowed

testthat::test_that(
  "check_active_indicator_values allows missing value when numerator is missing",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      indicator_value = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      value_type_code = c(1L, 3L, 4L),
      numerator = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      denominator = c(100, 100, 100)
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L),
      precalculated = c("No", "No", "No")
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


# Test 5: Missing denominator is allowed

testthat::test_that(
  "check_active_indicator_values allows missing value when denominator is missing",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      indicator_value = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      value_type_code = c(1L, 3L, 4L),
      numerator = c(10, 20, 30),
      denominator = c(
        NA_real_,
        NA_real_,
        NA_real_
      )
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L),
      precalculated = c("No", "No", "No")
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


# Test 6: Zero denominator is allowed

testthat::test_that(
  "check_active_indicator_values allows missing value when denominator is zero",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      indicator_value = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      value_type_code = c(1L, 3L, 4L),
      numerator = c(10, 20, 30),
      denominator = c(0, 0, 0)
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L),
      precalculated = c("No", "No", "No")
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


# Test 7: Missing numerator and denominator are allowed

testthat::test_that(
  "check_active_indicator_values allows missing value when numerator and denominator are missing",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      indicator_value = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      value_type_code = c(1L, 3L, 4L),
      numerator = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      denominator = c(
        NA_real_,
        NA_real_,
        NA_real_
      )
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L),
      precalculated = c("No", "No", "No")
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


# Test 8: Incalculable DASR is allowed
# Checks the case where the DASR calculation returns missing numerator,
# denominator, and indicator value.

testthat::test_that(
  "check_active_indicator_values allows incalculable DASR values",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 4L,
      numerator = NA_real_,
      denominator = NA_real_
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L,
      precalculated = "No"
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


# Test 9: Missing indicator value is flagged when calculation inputs are valid

testthat::test_that(
  "check_active_indicator_values flags missing values when calculation inputs are valid",
  {
    
    df <- data.frame(
      indicator_id = c(
        101L,
        102L,
        103L,
        104L,
        105L
      ),
      indicator_value = rep(
        NA_real_,
        5
      ),
      value_type_code = c(
        1L,
        2L,
        3L,
        4L,
        9L
      ),
      numerator = c(
        10,
        20,
        30,
        40,
        50
      ),
      denominator = c(
        100,
        200,
        300,
        400,
        500
      )
    )
    
    metadata <- data.frame(
      indicator_id = c(
        101L,
        102L,
        103L,
        104L,
        105L
      ),
      status_code = rep(
        1L,
        5
      ),
      precalculated = rep(
        "No",
        5
      )
    )
    
    suppressMessages(
      result <- check_active_indicator_values(
        df,
        metadata
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      5L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      c(
        101L,
        102L,
        103L,
        104L,
        105L
      )
    )
  }
)


# Test 10: Zero numerator is not treated as missing

testthat::test_that(
  "check_active_indicator_values does not treat zero numerator as missing",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      indicator_value = c(
        NA_real_,
        NA_real_,
        NA_real_
      ),
      value_type_code = c(2L, 3L, 4L),
      numerator = c(0, 0, 0),
      denominator = c(100, 100, 100)
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L),
      precalculated = c("No", "No", "No")
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


# Test 11: Precalculated indicators with missing values are allowed

testthat::test_that(
  "check_active_indicator_values allows missing values for precalculated indicators",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 1L,
      numerator = 10,
      denominator = 100
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L,
      precalculated = "Yes"
    )
    
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


# Test 12: Multiple failing indicators are returned

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
      value_type_code = c(1L, 3L, 4L),
      numerator = c(10, 20, 30),
      denominator = c(100, 200, 300)
    )
    
    metadata <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      status_code = c(1L, 1L, 1L),
      precalculated = c("No", "No", "No")
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


# Test 13: Failure message is produced

testthat::test_that(
  "check_active_indicator_values reports unexpected missing values",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = 1L,
      numerator = 10,
      denominator = 100
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L,
      precalculated = "No"
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


# Test 14: Missing value type still fails when calculation inputs are valid

testthat::test_that(
  "check_active_indicator_values flags missing value when value type is missing",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = NA_real_,
      value_type_code = NA_integer_,
      numerator = 10,
      denominator = 100
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L,
      precalculated = "No"
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


# Test 15: Non-data-frame df is rejected

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


# Test 16: Non-data-frame metadata is rejected

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


# Test 17: Missing df columns are reported

testthat::test_that(
  "check_active_indicator_values reports missing df columns",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = 10
    )
    
    metadata <- data.frame(
      indicator_id = 101L,
      status_code = 1L,
      precalculated = "No"
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


# Test 18: Missing metadata columns are reported

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
      "Missing required columns in `metadata`"
    )
  }
)