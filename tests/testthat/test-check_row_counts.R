# Test 1: Matching row counts 

testthat::test_that(
  "check_row_counts identifies matching row counts",
  {
    
    df <- data.frame(
      x = 1:5
    )
    
    reference_data <- data.frame(
      x = 11:15
    )
    
    testthat::expect_message(
      result <- check_row_counts(
        df,
        reference_data
      ),
      "Row counts match"
    )
    
    testthat::expect_equal(
      result$input_rows,
      5L
    )
    
    testthat::expect_equal(
      result$reference_rows,
      5L
    )
    
    testthat::expect_equal(
      result$row_difference,
      0L
    )
    
    testthat::expect_true(
      result$row_counts_match
    )
  }
)


# Test 2: Different row counts 

testthat::test_that(
  "check_row_counts identifies different row counts",
  {
    
    df <- data.frame(
      x = 1:4
    )
    
    reference_data <- data.frame(
      x = 1:6
    )
    
    testthat::expect_message(
      result <- check_row_counts(
        df,
        reference_data
      ),
      "Row counts do not match"
    )
    
    testthat::expect_equal(
      result$input_rows,
      4L
    )
    
    testthat::expect_equal(
      result$reference_rows,
      6L
    )
    
    testthat::expect_equal(
      result$row_difference,
      -2L
    )
    
    testthat::expect_false(
      result$row_counts_match
    )
  }
)


# Test 3: Positive row difference 

testthat::test_that(
  "check_row_counts returns positive difference when input has more rows",
  {
    
    df <- data.frame(
      x = 1:8
    )
    
    reference_data <- data.frame(
      x = 1:5
    )
    
    suppressMessages(
      result <- check_row_counts(
        df,
        reference_data
      )
    )
    
    testthat::expect_equal(
      result$row_difference,
      3L
    )
    
    testthat::expect_false(
      result$row_counts_match
    )
  }
)


# Test 4: Empty data frames can be compared 

testthat::test_that(
  "check_row_counts handles empty data frames",
  {
    
    df <- data.frame(
      x = integer()
    )
    
    reference_data <- data.frame(
      x = integer()
    )
    
    suppressMessages(
      result <- check_row_counts(
        df,
        reference_data
      )
    )
    
    testthat::expect_equal(
      result$input_rows,
      0L
    )
    
    testthat::expect_equal(
      result$reference_rows,
      0L
    )
    
    testthat::expect_true(
      result$row_counts_match
    )
  }
)


# Test 5: df must be a data frame 

testthat::test_that(
  "check_row_counts rejects non-data-frame df input",
  {
    
    reference_data <- data.frame(
      x = 1:3
    )
    
    testthat::expect_error(
      check_row_counts(
        df = c(1, 2, 3),
        reference_data = reference_data
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 6: reference_data must be a data frame 

testthat::test_that(
  "check_row_counts rejects non-data-frame reference_data input",
  {
    
    df <- data.frame(
      x = 1:3
    )
    
    testthat::expect_error(
      check_row_counts(
        df = df,
        reference_data = c(1, 2, 3)
      ),
      "`reference_data` must be a data frame"
    )
  }
)