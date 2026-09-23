# Test 1: Excel file is read correctly 

testthat::test_that(
  "read_excel_file reads Excel data correctly",
  {
    
    # Create dummy data
    test_data <- data.frame(
      indicator_id = c(101L, 102L, 103L),
      value = c(10.5, 20.5, 30.5),
      status = c("Active", "Active", "Inactive")
    )
    
    # Create temporary Excel file
    test_file <- tempfile(
      fileext = ".xlsx"
    )
    
    writexl::write_xlsx(
      list(
        Data = test_data
      ),
      path = test_file
    )
    
    # Read using package function
    result <- read_excel_file(
      file_path = test_file,
      sheet_name = "Data"
    )
    
    testthat::expect_equal(
      nrow(result),
      3L
    )
    
    testthat::expect_equal(
      result$indicator_id,
      c(101, 102, 103)
    )
    
    testthat::expect_equal(
      result$value,
      c(10.5, 20.5, 30.5)
    )
    
    testthat::expect_equal(
      result$status,
      c("Active", "Active", "Inactive")
    )
  }
)


# Test 2: Source file name is added 

testthat::test_that(
  "read_excel_file adds source_file column",
  {
    
    test_data <- data.frame(
      indicator_id = c(101L, 102L)
    )
    
    test_file <- tempfile(
      pattern = "test_excel_",
      fileext = ".xlsx"
    )
    
    writexl::write_xlsx(
      list(
        Data = test_data
      ),
      path = test_file
    )
    
    result <- read_excel_file(
      file_path = test_file,
      sheet_name = "Data"
    )
    
    testthat::expect_true(
      "source_file" %in% names(result)
    )
    
    testthat::expect_true(
      all(
        result$source_file ==
          basename(test_file)
      )
    )
  }
)


# Test 3: Sheet can be selected by name 

testthat::test_that(
  "read_excel_file reads the requested sheet",
  {
    
    sheet_1 <- data.frame(
      value = c(1, 2)
    )
    
    sheet_2 <- data.frame(
      value = c(10, 20)
    )
    
    test_file <- tempfile(
      fileext = ".xlsx"
    )
    
    writexl::write_xlsx(
      list(
        First = sheet_1,
        Second = sheet_2
      ),
      path = test_file
    )
    
    result <- read_excel_file(
      file_path = test_file,
      sheet_name = "Second"
    )
    
    testthat::expect_equal(
      result$value,
      c(10, 20)
    )
  }
)


# Test 4: Default reads first sheet 

testthat::test_that(
  "read_excel_file reads first sheet by default",
  {
    
    first_sheet <- data.frame(
      value = c(1, 2, 3)
    )
    
    second_sheet <- data.frame(
      value = c(10, 20, 30)
    )
    
    test_file <- tempfile(
      fileext = ".xlsx"
    )
    
    writexl::write_xlsx(
      list(
        First = first_sheet,
        Second = second_sheet
      ),
      path = test_file
    )
    
    result <- read_excel_file(
      file_path = test_file
    )
    
    testthat::expect_equal(
      result$value,
      c(1, 2, 3)
    )
  }
)


# Test 5: Missing file is rejected 

testthat::test_that(
  "read_excel_file reports missing files",
  {
    
    testthat::expect_error(
      read_excel_file(
        file_path = "file_that_does_not_exist.xlsx"
      ),
      "Excel file does not exist"
    )
  }
)


# Test 6: Invalid sheet returns NULL and warning

testthat::test_that(
  "read_excel_file returns NULL when sheet cannot be read",
  {
    
    test_data <- data.frame(
      value = c(1, 2, 3)
    )
    
    test_file <- tempfile(
      fileext = ".xlsx"
    )
    
    writexl::write_xlsx(
      list(
        Data = test_data
      ),
      path = test_file
    )
    
    testthat::expect_message(
      result <- read_excel_file(
        file_path = test_file,
        sheet_name = "SheetDoesNotExist"
      ),
      "Could not process"
    )
    
    testthat::expect_null(
      result
    )
  }
)


# Test 7: Invalid file_path input is rejected 
testthat::test_that(
  "read_excel_file validates file_path",
  {
    
    testthat::expect_error(
      read_excel_file(
        file_path = c(
          "file1.xlsx",
          "file2.xlsx"
        )
      ),
      "`file_path` must be a single file path"
    )
    
    testthat::expect_error(
      read_excel_file(
        file_path = NA_character_
      ),
      "`file_path` must be a single file path"
    )
  }
)


# Test 8: Invalid sheet_name input is rejected 

testthat::test_that(
  "read_excel_file validates sheet_name",
  {
    
    test_data <- data.frame(
      value = 1
    )
    
    test_file <- tempfile(
      fileext = ".xlsx"
    )
    
    writexl::write_xlsx(
      test_data,
      path = test_file
    )
    
    testthat::expect_error(
      read_excel_file(
        file_path = test_file,
        sheet_name = c(
          "Sheet1",
          "Sheet2"
        )
      ),
      "`sheet_name` must contain a single sheet name or sheet number"
    )
  }
)