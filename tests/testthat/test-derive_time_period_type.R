# Test 1: Monthly period is identified 

testthat::test_that(
  "derive_time_period_type identifies monthly periods",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2024-01-31")
    )
    
    testthat::expect_equal(
      result,
      "Monthly"
    )
  }
)


# Test 2: Quarterly period is identified 

testthat::test_that(
  "derive_time_period_type identifies quarterly periods",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2024-03-31")
    )
    
    testthat::expect_equal(
      result,
      "Quarterly"
    )
  }
)


# Test 3: One-year period is identified 

testthat::test_that(
  "derive_time_period_type identifies one-year periods",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2024-12-31")
    )
    
    testthat::expect_equal(
      result,
      "1 year"
    )
  }
)

# Test 4: Two-year pooled period is identified 

testthat::test_that(
  "derive_time_period_type identifies three-year pooled periods",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2021-12-31")
    )
    
    testthat::expect_equal(
      result,
      "2 year pooled"
    )
  }
)

# Test 5: Three-year pooled period is identified 

testthat::test_that(
  "derive_time_period_type identifies three-year pooled periods",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2022-12-31")
    )
    
    testthat::expect_equal(
      result,
      "3 year pooled"
    )
  }
)


# Test 6: Five-year pooled period is identified 

testthat::test_that(
  "derive_time_period_type identifies five-year pooled periods",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2020-04-01"),
      end_date = as.Date("2025-03-31")
    )
    
    testthat::expect_equal(
      result,
      "5 year pooled"
    )
  }
)


# Test 7: Anniversary date is accepted 

testthat::test_that(
  "derive_time_period_type accepts anniversary end dates",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2025-01-01")
    )
    
    testthat::expect_equal(
      result,
      "1 year"
    )
  }
)


# Test 7: Multiple rows are classified correctly 

testthat::test_that(
  "derive_time_period_type works with Date vectors",
  {
    
    start_date <- as.Date(
      c(
        "2024-01-01",
        "2024-01-01",
        "2024-01-01",
        "2020-01-01",
        "2020-04-01"
      )
    )
    
    end_date <- as.Date(
      c(
        "2024-01-31",
        "2024-03-31",
        "2024-12-31",
        "2022-12-31",
        "2025-03-31"
      )
    )
    
    result <- derive_time_period_type(
      start_date,
      end_date
    )
    
    expected <- c(
      "Monthly",
      "Quarterly",
      "1 year",
      "3 year pooled",
      "5 year pooled"
    )
    
    testthat::expect_equal(
      result,
      expected
    )
  }
)


# Test 8: Unsupported periods return NA 

testthat::test_that(
  "derive_time_period_type returns NA for unsupported periods",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2024-05-31")
    )
    
    testthat::expect_true(
      is.na(result)
    )
  }
)


# Test 9: start_date must be a Date vector 

testthat::test_that(
  "derive_time_period_type validates start_date",
  {
    
    testthat::expect_error(
      derive_time_period_type(
        start_date = "2024-01-01",
        end_date = as.Date("2024-12-31")
      ),
      "`start_date` must be a Date vector"
    )
  }
)


# Test 10: end_date must be a Date vector 

testthat::test_that(
  "derive_time_period_type validates end_date",
  {
    
    testthat::expect_error(
      derive_time_period_type(
        start_date = as.Date("2024-01-01"),
        end_date = "2024-12-31"
      ),
      "`end_date` must be a Date vector"
    )
  }
)


# Test 11: Date vectors must have the same length 

testthat::test_that(
  "derive_time_period_type requires equal input lengths",
  {
    
    testthat::expect_error(
      derive_time_period_type(
        start_date = as.Date(
          c(
            "2024-01-01",
            "2025-01-01"
          )
        ),
        end_date = as.Date("2024-12-31")
      ),
      "`start_date` and `end_date` must have the same length"
    )
  }
)


# Test 12: Leap-year dates are handled 

testthat::test_that(
  "derive_time_period_type handles leap-year dates",
  {
    
    result <- derive_time_period_type(
      start_date = as.Date("2024-02-29"),
      end_date = as.Date("2025-02-27")
    )
    
    testthat::expect_equal(
      result,
      "1 year"
    )
  }
)