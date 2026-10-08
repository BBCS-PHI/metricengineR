# Test 1: ZIP and direct CSV resources are processed successfully

testthat::test_that(
  "get_publication_data processes ZIP and CSV resources successfully",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        c(
          "https://example.com/january-2026",
          "https://example.com/february-2026"
        )
      },
    
    get_resource_link = function(
    publication_url,
    ...
    ){
      
      if(grepl("january", publication_url)){
        
        return(
          "https://example.com/january-2026.zip"
        )
      }
      
      "https://example.com/february-2026.csv"
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ){
      
      output_file <- file.path(
        download_folder,
        basename(resource_url)
      )
      
      
      # Create a real CSV file for the direct CSV resource
      
      if(grepl("\\.csv$", resource_url)){
        
        writeLines(
          c(
            "value",
            "february"
          ),
          output_file
        )
      }
      
      output_file
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ){
      
      file.path(
        extract_folder,
        tools::file_path_sans_ext(
          basename(zip_file)
        )
      )
    },
    
    read_csv_folder = function(
    folder,
    all_columns_character
    ){
      
      tibble::tibble(
        source_file = "january.csv",
        source_folder = basename(folder),
        value = "january"
      )
    },
    
    .package = "metricengineR"
    )
    
    
    result <- suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "[a-z]+-[0-9]{4}$",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      nrow(result$data),
      2L
    )
    
    testthat::expect_equal(
      nrow(result$catalogue),
      2L
    )
    
    testthat::expect_equal(
      length(result$publication_links),
      2L
    )
    
    testthat::expect_equal(
      nrow(result$failed_resources),
      0L
    )
    
    testthat::expect_true(
      all(
        result$catalogue$processing_status == "Processed"
      )
    )
    
    testthat::expect_equal(
      sort(result$catalogue$file_type),
      c("csv", "zip")
    )
    
    testthat::expect_true(
      all(
        c(
          "publication_url",
          "resource_url",
          "downloaded_file",
          "file_type",
          "extracted_folder",
          "processing_status"
        ) %in% names(result$catalogue)
      )
    )
  }
)


# Test 2: No publication links returns an empty result

testthat::test_that(
  "get_publication_data handles no publication links",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        character(0)
      },
    
    .package = "metricengineR"
    )
    
    
    result <- suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      nrow(result$data),
      0L
    )
    
    testthat::expect_equal(
      nrow(result$catalogue),
      0L
    )
    
    testthat::expect_equal(
      nrow(result$failed_resources),
      0L
    )
    
    testthat::expect_equal(
      result$publication_links,
      character(0)
    )
  }
)


# Test 3: Missing downloadable resource is recorded as a failure

testthat::test_that(
  "get_publication_data records missing resources",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        "https://example.com/january-2026"
      },
    
    get_resource_link = function(
    publication_url,
    ...
    ){
      
      NA_character_
    },
    
    .package = "metricengineR"
    )
    
    
    result <- suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      nrow(result$data),
      0L
    )
    
    testthat::expect_equal(
      nrow(result$catalogue),
      1L
    )
    
    testthat::expect_equal(
      result$catalogue$processing_status,
      "Resource not found"
    )
    
    testthat::expect_equal(
      nrow(result$failed_resources),
      1L
    )
    
    testthat::expect_equal(
      result$failed_resources$publication_url,
      "https://example.com/january-2026"
    )
  }
)


# Test 4: Failed download is recorded correctly

testthat::test_that(
  "get_publication_data handles failed downloads",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        "https://example.com/january-2026"
      },
    
    get_resource_link = function(
    publication_url,
    ...
    ){
      
      "https://example.com/january-2026.zip"
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ){
      
      NA_character_
    },
    
    .package = "metricengineR"
    )
    
    
    result <- suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      nrow(result$data),
      0L
    )
    
    testthat::expect_equal(
      result$catalogue$processing_status,
      "Download failed"
    )
    
    testthat::expect_equal(
      nrow(result$failed_resources),
      1L
    )
    
    testthat::expect_equal(
      result$failed_resources$processing_status,
      "Download failed"
    )
  }
)


# Test 5: Failed ZIP extraction is recorded correctly

testthat::test_that(
  "get_publication_data handles failed extractions",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        "https://example.com/january-2026"
      },
    
    get_resource_link = function(
    publication_url,
    ...
    ){
      
      "https://example.com/january-2026.zip"
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ){
      
      file.path(
        download_folder,
        "january-2026.zip"
      )
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ){
      
      NA_character_
    },
    
    .package = "metricengineR"
    )
    
    
    result <- suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      nrow(result$data),
      0L
    )
    
    testthat::expect_true(
      is.na(
        result$catalogue$extracted_folder
      )
    )
    
    testthat::expect_equal(
      result$catalogue$processing_status,
      "Extraction failed"
    )
    
    testthat::expect_equal(
      nrow(result$failed_resources),
      1L
    )
  }
)


# Test 6: Failure while reading an extracted ZIP resource is recorded

testthat::test_that(
  "get_publication_data handles failed reads",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        "https://example.com/january-2026"
      },
    
    get_resource_link = function(
    publication_url,
    ...
    ){
      
      "https://example.com/january-2026.zip"
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ){
      
      file.path(
        download_folder,
        "january-2026.zip"
      )
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ){
      
      file.path(
        extract_folder,
        "january-2026"
      )
    },
    
    read_csv_folder = function(
    folder,
    all_columns_character
    ){
      
      stop(
        "Could not read CSV data."
      )
    },
    
    .package = "metricengineR"
    )
    
    
    result <- suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      nrow(result$data),
      0L
    )
    
    testthat::expect_equal(
      result$catalogue$processing_status,
      "Read failed"
    )
    
    testthat::expect_equal(
      nrow(result$failed_resources),
      1L
    )
  }
)


# Test 7: Extracted folder with no CSV data is recorded correctly

testthat::test_that(
  "get_publication_data handles extracted resources with no CSV data",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        "https://example.com/january-2026"
      },
    
    get_resource_link = function(
    publication_url,
    ...
    ){
      
      "https://example.com/january-2026.zip"
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ){
      
      file.path(
        download_folder,
        "january-2026.zip"
      )
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ){
      
      file.path(
        extract_folder,
        "january-2026"
      )
    },
    
    read_csv_folder = function(
    folder,
    all_columns_character
    ){
      
      tibble::tibble()
    },
    
    .package = "metricengineR"
    )
    
    
    result <- suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      nrow(result$data),
      0L
    )
    
    testthat::expect_equal(
      result$catalogue$processing_status,
      "No CSV data found"
    )
    
    testthat::expect_equal(
      nrow(result$failed_resources),
      1L
    )
  }
)


# Test 8: Only folders extracted during the current workflow are read

testthat::test_that(
  "get_publication_data reads only current extracted folders",
  {
    
    test_folder <- tempfile()
    
    on.exit(
      unlink(
        test_folder,
        recursive = TRUE
      ),
      add = TRUE
    )
    
    folders_read <- character(0)
    
    
    testthat::local_mocked_bindings(
      
      get_child_links = function(
    parent_url,
    publication_pattern
      ){
        
        c(
          "https://example.com/january-2026",
          "https://example.com/february-2026"
        )
      },
    
    get_resource_link = function(
    publication_url,
    ...
    ){
      
      paste0(
        publication_url,
        ".zip"
      )
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ){
      
      file.path(
        download_folder,
        basename(resource_url)
      )
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ){
      
      file.path(
        extract_folder,
        tools::file_path_sans_ext(
          basename(zip_file)
        )
      )
    },
    
    read_csv_folder = function(
    folder,
    all_columns_character
    ){
      
      folders_read <<- c(
        folders_read,
        folder
      )
      
      tibble::tibble(
        value = "test"
      )
    },
    
    .package = "metricengineR"
    )
    
    
    suppressMessages(
      get_publication_data(
        parent_url = "https://example.com/publications",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = test_folder
      )
    )
    
    
    testthat::expect_equal(
      length(folders_read),
      2L
    )
    
    testthat::expect_true(
      all(
        basename(folders_read) %in%
          c(
            "january-2026",
            "february-2026"
          )
      )
    )
  }
)


# Test 9: parent_url is validated

testthat::test_that(
  "get_publication_data validates parent_url",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = tempfile()
      ),
      "`parent_url` must be a single non-empty character value"
    )
  }
)


# Test 10: publication_pattern is validated

testthat::test_that(
  "get_publication_data validates publication_pattern",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "",
        resource_text = "CSV Data",
        download_folder = tempfile()
      ),
      "`publication_pattern` must be a single non-empty character value"
    )
  }
)


# Test 11: resource_text is validated

testthat::test_that(
  "get_publication_data validates resource_text",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "test",
        resource_text = "",
        download_folder = tempfile()
      ),
      "`resource_text` must be a single non-empty character value"
    )
  }
)


# Test 12: download_folder is validated

testthat::test_that(
  "get_publication_data validates download_folder",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = ""
      ),
      "`download_folder` must be a single non-empty character value"
    )
  }
)


# Test 13: dataset_pattern is validated

testthat::test_that(
  "get_publication_data validates dataset_pattern",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = tempfile(),
        dataset_pattern = ""
      ),
      "`dataset_pattern` must be a single non-empty character value"
    )
  }
)


# Test 14: period_pattern is validated

testthat::test_that(
  "get_publication_data validates period_pattern",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = tempfile(),
        period_pattern = ""
      ),
      "`period_pattern` must be a single non-empty character value"
    )
  }
)


# Test 15: file_pattern is validated

testthat::test_that(
  "get_publication_data validates file_pattern",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = tempfile(),
        file_pattern = ""
      ),
      "`file_pattern` must be a single non-empty character value"
    )
  }
)


# Test 16: match_period is validated

testthat::test_that(
  "get_publication_data validates match_period",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = tempfile(),
        match_period = "Yes"
      ),
      "`match_period` must be TRUE or FALSE"
    )
  }
)


# Test 17: all_columns_character is validated

testthat::test_that(
  "get_publication_data validates all_columns_character",
  {
    
    testthat::expect_error(
      get_publication_data(
        parent_url = "https://example.com",
        publication_pattern = "test",
        resource_text = "CSV Data",
        download_folder = tempfile(),
        all_columns_character = "Yes"
      ),
      "`all_columns_character` must be TRUE or FALSE"
    )
  }
)