# Test 1: One age group code per DASR indicator passes 

testthat::test_that(
  "check_dasr_age_group_code passes when each DASR indicator has one age group code",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L, 102L, 102L),
      age_group_code = c(18L, 18L, 19L, 19L),
      value_type_code = c(4L, 4L, 4L, 4L)
    )
    
    testthat::expect_message(
      result <- check_dasr_age_group_code(df),
      "Each DASR indicator has one unique age_group_code"
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 2: Multiple age group codes are identified 

testthat::test_that(
  "check_dasr_age_group_code identifies indicators with multiple age group codes",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L, 102L, 102L),
      age_group_code = c(18L, 19L, 20L, 20L),
      value_type_code = c(4L, 4L, 4L, 4L)
    )
    
    testthat::expect_message(
      result <- check_dasr_age_group_code(df),
      "1 DASR indicator\\(s\\) have more than one age_group_code"
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
      result$age_group_count,
      2L
    )
  }
)


# Test 3: Duplicate rows do not create false failures 

testthat::test_that(
  "check_dasr_age_group_code ignores duplicate age group rows",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L, 101L),
      age_group_code = c(18L, 18L, 18L),
      value_type_code = c(4L, 4L, 4L)
    )
    
    suppressMessages(
      result <- check_dasr_age_group_code(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 4: Non-DASR indicators are ignored 

testthat::test_that(
  "check_dasr_age_group_code ignores non-DASR indicators",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L, 102L, 102L),
      age_group_code = c(18L, 19L, 20L, 21L),
      value_type_code = c(2L, 2L, 4L, 4L)
    )
    
    suppressMessages(
      result <- check_dasr_age_group_code(df)
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


# Test 5: Missing age group code is treated as a distinct value 

testthat::test_that(
  "check_dasr_age_group_code flags missing age group code when another code is present",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      age_group_code = c(18L, NA_integer_),
      value_type_code = c(4L, 4L)
    )
    
    suppressMessages(
      result <- check_dasr_age_group_code(df)
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
      result$age_group_count,
      2L
    )
  }
)


# Test 6: No DASR rows returns empty result 

testthat::test_that(
  "check_dasr_age_group_code handles data with no DASR rows",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      age_group_code = c(18L, 19L),
      value_type_code = c(2L, 7L)
    )
    
    testthat::expect_message(
      result <- check_dasr_age_group_code(df),
      "Each DASR indicator has one unique age_group_code"
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 7: Non-data-frame input is rejected 

testthat::test_that(
  "check_dasr_age_group_code rejects non-data-frame input",
  {
    
    testthat::expect_error(
      check_dasr_age_group_code(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 8: Missing required columns are reported 

testthat::test_that(
  "check_dasr_age_group_code reports missing required columns",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      value_type_code = c(4L, 4L)
    )
    
    testthat::expect_error(
      check_dasr_age_group_code(df),
      "Missing required columns: age_group_code"
    )
  }
)