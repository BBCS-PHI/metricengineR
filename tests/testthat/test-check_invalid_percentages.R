# Helper data

create_test_invalid_percentage_data <- function() {
  
  data.frame(
    indicator_id = c(
      101L,
      102L,
      103L,
      104L
    ),
    value_type_code = c(
      2L,
      2L,
      4L,
      2L
    ),
    indicator_value = c(
      50,
      120,
      150,
      NA_real_
    )
  )
}


# Test 1: Percentage values above 100 are identified

testthat::test_that(
  "check_invalid_percentages identifies percentage values above 100",
  {
    
    df <- create_test_invalid_percentage_data()
    
    suppressMessages(
      result <- check_invalid_percentages(df)
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
      result$indicator_value,
      120
    )
  }
)


# Test 2: Percentage values equal to 100 are allowed

testthat::test_that(
  "check_invalid_percentages allows percentage values equal to 100",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      value_type_code = 2L,
      indicator_value = 100
    )
    
    suppressMessages(
      result <- check_invalid_percentages(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 3: Percentage values below 100 are allowed

testthat::test_that(
  "check_invalid_percentages allows percentage values below 100",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      value_type_code = 2L,
      indicator_value = 75
    )
    
    suppressMessages(
      result <- check_invalid_percentages(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 4: Non-percentage value types are ignored

testthat::test_that(
  "check_invalid_percentages ignores non-percentage value types",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      value_type_code = 4L,
      indicator_value = 150
    )
    
    suppressMessages(
      result <- check_invalid_percentages(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 5: Missing indicator values are ignored

testthat::test_that(
  "check_invalid_percentages ignores missing indicator values",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      value_type_code = 2L,
      indicator_value = NA_real_
    )
    
    suppressMessages(
      result <- check_invalid_percentages(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 6: Multiple invalid percentages are returned

testthat::test_that(
  "check_invalid_percentages returns all invalid percentage rows",
  {
    
    df <- data.frame(
      indicator_id = c(
        101L,
        102L,
        103L
      ),
      value_type_code = c(
        2L,
        2L,
        2L
      ),
      indicator_value = c(
        110,
        150,
        80
      )
    )
    
    suppressMessages(
      result <- check_invalid_percentages(df)
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


# Test 7: Warning message identifies failing indicators

testthat::test_that(
  "check_invalid_percentages reports indicator IDs with values above 100",
  {
    
    df <- data.frame(
      indicator_id = c(
        101L,
        102L
      ),
      value_type_code = c(
        2L,
        2L
      ),
      indicator_value = c(
        110,
        125
      )
    )
    
    testthat::expect_message(
      check_invalid_percentages(df),
      "indicator ID\\(s\\): 101, 102"
    )
  }
)


# Test 8: PASS message is returned

testthat::test_that(
  "check_invalid_percentages reports when no invalid percentages are found",
  {
    
    df <- data.frame(
      indicator_id = c(
        101L,
        102L
      ),
      value_type_code = c(
        2L,
        2L
      ),
      indicator_value = c(
        50,
        100
      )
    )
    
    testthat::expect_message(
      check_invalid_percentages(df),
      "No percentage values greater than 100 were found"
    )
  }
)


# Test 9: WARNING message is returned

testthat::test_that(
  "check_invalid_percentages reports when invalid percentages are found",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      value_type_code = 2L,
      indicator_value = 120
    )
    
    testthat::expect_message(
      check_invalid_percentages(df),
      "Found percentage values greater than 100"
    )
  }
)


# Test 10: Returned rows retain original columns

testthat::test_that(
  "check_invalid_percentages retains original columns",
  {
    
    df <- create_test_invalid_percentage_data()
    
    suppressMessages(
      result <- check_invalid_percentages(df)
    )
    
    testthat::expect_equal(
      names(result),
      names(df)
    )
  }
)


# Test 11: Non-data-frame input is rejected

testthat::test_that(
  "check_invalid_percentages rejects non-data-frame input",
  {
    
    testthat::expect_error(
      check_invalid_percentages(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 12: Missing required columns are reported

testthat::test_that(
  "check_invalid_percentages reports missing required columns",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      indicator_value = 120
    )
    
    testthat::expect_error(
      check_invalid_percentages(df),
      "Missing required columns: value_type_code"
    )
  }
)