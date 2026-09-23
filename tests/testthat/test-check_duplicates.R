# Helper data

create_test_duplicate_data <- function() {
  
  data.frame(
    indicator_id = c(
      101L, 101L, 101L,
      102L, 102L,
      103L
    ),
    start_date = as.Date(
      c(
        "2025-01-01",
        "2025-01-01",
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
        "2025-12-31",
        "2025-12-31",
        "2025-12-31"
      )
    ),
    aggregation_id = c(
      151L, 151L, 151L,
      151L, 151L,
      151L
    ),
    age_group_code = c(
      18L, 18L, 19L,
      18L, 18L,
      18L
    ),
    sex_code = c(
      0L, 0L, 0L,
      1L, 2L,
      0L
    ),
    ethnicity_code = c(
      999L, 999L, 999L,
      999L, 999L,
      999L
    ),
    imd_code = c(
      999L, 999L, 999L,
      999L, 999L,
      999L
    ),
    value_type_code = c(
      2L, 2L, 2L,
      2L, 2L,
      4L
    ),
    source_code = c(
      1L, 1L, 1L,
      1L, 1L,
      2L
    ),
    indicator_value = c(
      10, 11, 12,
      20, 21,
      30
    )
  )
}


# Test 1: Duplicate records are identified

testthat::test_that(
  "check_duplicates identifies duplicate key combinations",
  {
    
    df <- create_test_duplicate_data()
    
    suppressMessages(
      result <- check_duplicates(df)
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
      result$row_count,
      2L
    )
  }
)


# Test 2: Different age groups are not treated as duplicates

testthat::test_that(
  "check_duplicates distinguishes different age groups",
  {
    
    df <- create_test_duplicate_data()
    
    df <- df[
      c(1, 3),
      ,
      drop = FALSE
    ]
    
    suppressMessages(
      result <- check_duplicates(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 3: Different sex codes are not treated as duplicates

testthat::test_that(
  "check_duplicates distinguishes different sex codes",
  {
    
    df <- create_test_duplicate_data()
    
    df <- df[
      c(4, 5),
      ,
      drop = FALSE
    ]
    
    suppressMessages(
      result <- check_duplicates(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 4: Different value types are not treated as duplicates

testthat::test_that(
  "check_duplicates distinguishes different value types",
  {
    
    df <- create_test_duplicate_data()
    
    df <- df[
      c(1, 2),
      ,
      drop = FALSE
    ]
    
    df$value_type_code[2] <- 4L
    
    suppressMessages(
      result <- check_duplicates(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 5: Different source codes are not treated as duplicates

testthat::test_that(
  "check_duplicates distinguishes different source codes",
  {
    
    df <- create_test_duplicate_data()
    
    df <- df[
      c(1, 2),
      ,
      drop = FALSE
    ]
    
    df$source_code[2] <- 2L
    
    suppressMessages(
      result <- check_duplicates(df)
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 6: Different indicator values can still be duplicates

testthat::test_that(
  "check_duplicates ignores measure values when identifying duplicates",
  {
    
    df <- create_test_duplicate_data()
    
    suppressMessages(
      result <- check_duplicates(df)
    )
    
    testthat::expect_equal(
      result$indicator_id,
      101L
    )
    
    testthat::expect_equal(
      result$row_count,
      2L
    )
  }
)


# Test 7: No duplicates returns an empty data frame

testthat::test_that(
  "check_duplicates returns empty data frame when no duplicates exist",
  {
    
    df <- create_test_duplicate_data()
    
    df <- df[
      c(1, 3, 4, 5, 6),
      ,
      drop = FALSE
    ]
    
    suppressMessages(
      result <- check_duplicates(df)
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


# Test 8: PASS message is returned

testthat::test_that(
  "check_duplicates reports when no duplicates are found",
  {
    
    df <- create_test_duplicate_data()
    
    df <- df[
      c(1, 3, 4, 5, 6),
      ,
      drop = FALSE
    ]
    
    testthat::expect_message(
      check_duplicates(df),
      "No duplicate records found"
    )
  }
)


# Test 9: FAIL message is returned

testthat::test_that(
  "check_duplicates reports when duplicates are found",
  {
    
    df <- create_test_duplicate_data()
    
    testthat::expect_message(
      check_duplicates(df),
      "Duplicate records found: 1 duplicate key combination"
    )
  }
)


# Test 10: Indicator filter limits the check

testthat::test_that(
  "check_duplicates applies indicator filter",
  {
    
    df <- create_test_duplicate_data()
    
    suppressMessages(
      result <- check_duplicates(
        df,
        indicator_filter = 102L
      )
    )
    
    testthat::expect_equal(
      nrow(result),
      0L
    )
  }
)


# Test 11: Indicator filter can include duplicate indicators

testthat::test_that(
  "check_duplicates identifies duplicates within indicator filter",
  {
    
    df <- create_test_duplicate_data()
    
    suppressMessages(
      result <- check_duplicates(
        df,
        indicator_filter = 101L
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


# Test 12: Custom key columns can be used

testthat::test_that(
  "check_duplicates accepts custom key columns",
  {
    
    df <- create_test_duplicate_data()
    
    suppressMessages(
      result <- check_duplicates(
        df,
        key_cols = c(
          "indicator_id",
          "start_date",
          "end_date"
        )
      )
    )
    
    testthat::expect_true(
      nrow(result) > 1L
    )
  }
)


# Test 13: Duplicate count is correct

testthat::test_that(
  "check_duplicates returns the correct duplicate row count",
  {
    
    df <- create_test_duplicate_data()
    
    extra_row <- df[1, ]
    
    df <- rbind(
      df,
      extra_row
    )
    
    suppressMessages(
      result <- check_duplicates(df)
    )
    
    testthat::expect_equal(
      result$row_count[
        result$indicator_id == 101L &
          result$age_group_code == 18L
      ],
      3L
    )
  }
)


# Test 14: Non-data-frame input is rejected

testthat::test_that(
  "check_duplicates rejects non-data-frame input",
  {
    
    testthat::expect_error(
      check_duplicates(
        c(1, 2, 3)
      ),
      "`df` must be a data frame"
    )
  }
)


# Test 15: Missing key columns are reported

testthat::test_that(
  "check_duplicates reports missing key columns",
  {
    
    df <- create_test_duplicate_data() |>
      dplyr::select(
        -sex_code
      )
    
    testthat::expect_error(
      check_duplicates(df),
      "Key columns not found in `df`: sex_code"
    )
  }
)


# Test 16: Custom missing key columns are reported

testthat::test_that(
  "check_duplicates validates custom key columns",
  {
    
    df <- create_test_duplicate_data()
    
    testthat::expect_error(
      check_duplicates(
        df,
        key_cols = c(
          "indicator_id",
          "missing_column"
        )
      ),
      "Key columns not found in `df`: missing_column"
    )
  }
)