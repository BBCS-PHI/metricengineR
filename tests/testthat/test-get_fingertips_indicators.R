# Test 1: Indicator IDs cannot be empty

testthat::test_that(
  "get_fingertips_indicators rejects empty indicator IDs",
  {
    
    testthat::expect_error(
      get_fingertips_indicators(
        indicator_ids = character(0)
      ),
      "`indicator_ids` must contain at least one non-missing indicator ID"
    )
  }
)


# Test 2: Indicator IDs cannot contain missing values

testthat::test_that(
  "get_fingertips_indicators rejects missing indicator IDs",
  {
    
    testthat::expect_error(
      get_fingertips_indicators(
        indicator_ids = c(93725, NA)
      ),
      "`indicator_ids` must contain at least one non-missing indicator ID"
    )
  }
)


# Test 3: Delay cannot be negative

testthat::test_that(
  "get_fingertips_indicators rejects negative delay",
  {
    
    testthat::expect_error(
      get_fingertips_indicators(
        indicator_ids = 93725,
        delay_seconds = -1
      ),
      "`delay_seconds` must be a single number greater than or equal to 0"
    )
  }
)


# Test 4: Delay must contain one value

testthat::test_that(
  "get_fingertips_indicators requires one delay value",
  {
    
    testthat::expect_error(
      get_fingertips_indicators(
        indicator_ids = 93725,
        delay_seconds = c(0.5, 1)
      ),
      "`delay_seconds` must be a single number greater than or equal to 0"
    )
  }
)


# Test 5: Live Fingertips API request works

testthat::test_that(
  "get_fingertips_indicators retrieves data from the Fingertips API",
  {
    
    testthat::skip_if(
      Sys.getenv("RUN_LIVE_FINGERTIPS_TESTS") != "true",
      "Live Fingertips tests are not enabled"
    )
    
    suppressMessages(
      result <- get_fingertips_indicators(
        indicator_ids = 93725,
        delay_seconds = 0
      )
    )
    
    testthat::expect_true(
      is.data.frame(result)
    )
    
    testthat::expect_gt(
      nrow(result),
      0L
    )
  }
)

# Test 5: Live Fingertips API request works for retrieving multiple indicators
testthat::test_that(
  "get_fingertips_indicators can retrieve multiple indicators",
  {
    
    testthat::skip_if(
      Sys.getenv("RUN_LIVE_FINGERTIPS_TESTS") != "true",
      "Live Fingertips tests are not enabled"
    )
    
    suppressMessages(
      result <- get_fingertips_indicators(
        indicator_ids = c(
          93725,
          93726
        ),
        delay_seconds = 0.5
      )
    )
    
    testthat::expect_true(
      is.data.frame(result)
    )
    
    testthat::expect_gt(
      nrow(result),
      0L
    )
  }
)