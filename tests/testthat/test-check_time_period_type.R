# Test 1: Passes when all time period types are populated 

testthat::test_that(
  "check_time_period_type passes when all values are populated",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      start_date = as.Date(
        c(
          "2025-01-01",
          "2025-04-01"
        )
      ),
      end_date = as.Date(
        c(
          "2025-12-31",
          "2026-03-31"
        )
      ),
      time_period_type = c(
        "1 year",
        "1 year"
      )
    )
    
    testthat::expect_message(
      result <- check_time_period_type(df),
      "PASS: time_period_type is populated for all rows"
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 2: Missing NA values are identified 

testthat::test_that(
  "check_time_period_type identifies missing values",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      start_date = as.Date(
        c(
          "2025-01-01",
          "2025-01-01"
        )
      ),
      end_date = as.Date(
        c(
          "2025-12-31",
          "2025-12-31"
        )
      ),
      time_period_type = c(
        "1 year",
        NA_character_
      )
    )
    
    testthat::expect_message(
      result <- check_time_period_type(df),
      "WARNING: Found 1 record"
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


# Test 3: Blank values are identified 

testthat::test_that(
  "check_time_period_type identifies blank values",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L),
      start_date = as.Date(
        c(
          "2025-01-01",
          "2025-01-01"
        )
      ),
      end_date = as.Date(
        c(
          "2025-12-31",
          "2025-12-31"
        )
      ),
      time_period_type = c(
        "1 year",
        ""
      )
    )
    
    suppressMessages(
      result <- check_time_period_type(df)
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


# Test 4: Whitespace-only values are identified 

testthat::test_that(
  "check_time_period_type identifies whitespace-only values",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      start_date = as.Date("2025-01-01"),
      end_date = as.Date("2025-12-31"),
      time_period_type = "   "
    )
    
    suppressMessages(
      result <- check_time_period_type(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
  }
)


# Test 5: Duplicate missing records are returned once 

testthat::test_that(
  "check_time_period_type returns distinct missing records",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 101L),
      start_date = as.Date(
        c(
          "2025-01-01",
          "2025-01-01"
        )
      ),
      end_date = as.Date(
        c(
          "2025-12-31",
          "2025-12-31"
        )
      ),
      time_period_type = c(
        NA_character_,
        NA_character_
      )
    )
    
    suppressMessages(
      result <- check_time_period_type(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      1L
    )
  }
)


# Test 6: Multiple missing records are returned 

testthat::test_that(
  "check_time_period_type returns all distinct missing records",
  {
    
    df <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      start_date = as.Date(
        c(
          "2025-01-01",
          "2025-01-01",
          "2025-01-01"
        )
      ),
      end_date = as.Date(
        c(
          "2025-12-31",
          "2025-12-31",
          "2025-12-31"
        )
      ),
      time_period_type = c(
        NA_character_,
        "",
        "1 year"
      )
    )
    
    suppressMessages(
      result <- check_time_period_type(df)
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


# Test 7: Non-data-frame input is rejected 

testthat::test_that(
  "check_time_period_type rejects non-data-frame input",
  {
    
    testthat::expect_error(
      check_time_period_type(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 8: Missing required columns are reported 

testthat::test_that(
  "check_time_period_type reports missing required columns",
  {
    
    df <- data.frame(
      indicator_id = 101L,
      start_date = as.Date("2025-01-01"),
      end_date = as.Date("2025-12-31")
    )
    
    testthat::expect_error(
      check_time_period_type(df),
      "Missing required columns: time_period_type"
    )
  }
)