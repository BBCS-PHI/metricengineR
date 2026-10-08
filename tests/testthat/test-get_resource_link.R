# Test 1: Correct matching resource URL is returned

testthat::test_that(
  "get_resource_link returns the matching resource URL",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publications/test/june-2026/datasets">
            Datasets
          </a>
        </body>
      </html>
    '
    
    dataset_html <- '
      <html>
        <body>
          <a href="/files/csds-may-2026.zip">
            CSV Data (as ZIP) May 2026
          </a>
          <a href="/files/csds-june-2026.zip">
            CSV Data (as ZIP) June 2026
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets$", x)){
          xml2::read_html(dataset_html)
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    result <- suppressMessages(
      get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/june-2026",
        resource_text = "CSV Data \\(as ZIP\\)"
      )
    )
    
    testthat::expect_equal(
      result,
      "https://digital.nhs.uk/files/csds-june-2026.zip"
    )
  }
)


# Test 2: Publication period selects the correct resource

testthat::test_that(
  "get_resource_link matches the publication period",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publications/test/february-2026/datasets">
            Datasets
          </a>
        </body>
      </html>
    '
    
    dataset_html <- '
      <html>
        <body>
          <a href="/files/january-2026.zip">
            CSV Data (as ZIP) January 2026
          </a>
          <a href="/files/february-2026.zip">
            CSV Data (as ZIP) February 2026
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets$", x)){
          xml2::read_html(dataset_html)
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    result <- suppressMessages(
      get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/february-2026",
        resource_text = "CSV Data \\(as ZIP\\)",
        match_period = TRUE
      )
    )
    
    testthat::expect_equal(
      result,
      "https://digital.nhs.uk/files/february-2026.zip"
    )
  }
)


# Test 3: Resource can be returned without period matching

testthat::test_that(
  "get_resource_link can retrieve a resource without period matching",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publications/test/june-2026/datasets">
            Datasets
          </a>
        </body>
      </html>
    '
    
    dataset_html <- '
      <html>
        <body>
          <a href="/files/test-resource.zip">
            CSV Data (as ZIP)
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets$", x)){
          xml2::read_html(dataset_html)
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    result <- suppressMessages(
      get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/june-2026",
        resource_text = "CSV Data \\(as ZIP\\)",
        match_period = FALSE
      )
    )
    
    testthat::expect_equal(
      result,
      "https://digital.nhs.uk/files/test-resource.zip"
    )
  }
)


# Test 4: No matching resource returns NA

testthat::test_that(
  "get_resource_link returns NA when no matching resource is found",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publications/test/june-2026/datasets">
            Datasets
          </a>
        </body>
      </html>
    '
    
    dataset_html <- '
      <html>
        <body>
          <a href="/files/other-file.zip">
            Other Download June 2026
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets$", x)){
          xml2::read_html(dataset_html)
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    testthat::expect_message(
      result <- get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/june-2026",
        resource_text = "CSV Data \\(as ZIP\\)"
      ),
      "No matching resource found"
    )
    
    testthat::expect_true(
      is.na(result)
    )
  }
)


# Test 5: Missing dataset page returns NA

testthat::test_that(
  "get_resource_link returns NA when no dataset page is found",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publications/test/june-2026/other-page">
            Other page
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        xml2::read_html(publication_html)
      },
      .package = "rvest"
    )
    
    testthat::expect_message(
      result <- get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/june-2026",
        resource_text = "CSV Data \\(as ZIP\\)"
      ),
      "No dataset page found"
    )
    
    testthat::expect_true(
      is.na(result)
    )
  }
)


# Test 6: Publication page read failure returns NA

testthat::test_that(
  "get_resource_link handles a publication page that cannot be read",
  {
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        stop("Connection failed.")
      },
      .package = "rvest"
    )
    
    testthat::expect_message(
      result <- get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/june-2026",
        resource_text = "CSV Data \\(as ZIP\\)"
      ),
      "Could not read publication page"
    )
    
    testthat::expect_true(
      is.na(result)
    )
  }
)


# Test 7: Dataset page read failure returns NA

testthat::test_that(
  "get_resource_link handles a dataset page that cannot be read",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publications/test/june-2026/datasets">
            Datasets
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets$", x)){
          stop("Connection failed.")
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    testthat::expect_message(
      result <- get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/june-2026",
        resource_text = "CSV Data \\(as ZIP\\)"
      ),
      "Could not read dataset page"
    )
    
    testthat::expect_true(
      is.na(result)
    )
  }
)


# Test 8: Invalid publication period causes an error

testthat::test_that(
  "get_resource_link stops when the publication period cannot be identified",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publications/test/latest/datasets">
            Datasets
          </a>
        </body>
      </html>
    '
    
    dataset_html <- '
      <html>
        <body>
          <a href="/files/test.zip">
            CSV Data (as ZIP)
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets$", x)){
          xml2::read_html(dataset_html)
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    testthat::expect_error(
      suppressMessages(
        get_resource_link(
          publication_url = "https://digital.nhs.uk/publications/test/latest",
          resource_text = "CSV Data \\(as ZIP\\)",
          match_period = TRUE
        )
      ),
      "period_pattern.*could not identify"
    )
  }
)


# Test 9: publication_url is validated

testthat::test_that(
  "get_resource_link validates publication_url",
  {
    
    testthat::expect_error(
      get_resource_link(
        publication_url = "",
        resource_text = "CSV Data"
      ),
      "`publication_url` must be a single non-empty character value"
    )
    
    testthat::expect_error(
      get_resource_link(
        publication_url = c(
          "https://digital.nhs.uk/page1",
          "https://digital.nhs.uk/page2"
        ),
        resource_text = "CSV Data"
      ),
      "`publication_url` must be a single non-empty character value"
    )
  }
)


# Test 10: resource_text is validated

testthat::test_that(
  "get_resource_link validates resource_text",
  {
    
    testthat::expect_error(
      get_resource_link(
        publication_url = "https://digital.nhs.uk/test/june-2026",
        resource_text = ""
      ),
      "`resource_text` must be a single non-empty character value"
    )
  }
)


# Test 11: match_period is validated

testthat::test_that(
  "get_resource_link validates match_period",
  {
    
    testthat::expect_error(
      get_resource_link(
        publication_url = "https://digital.nhs.uk/test/june-2026",
        resource_text = "CSV Data",
        match_period = "Yes"
      ),
      "`match_period` must be TRUE or FALSE"
    )
  }
)


# Test 12: dataset_pattern is validated

testthat::test_that(
  "get_resource_link validates dataset_pattern",
  {
    
    testthat::expect_error(
      get_resource_link(
        publication_url = "https://digital.nhs.uk/test/june-2026",
        resource_text = "CSV Data",
        dataset_pattern = ""
      ),
      "`dataset_pattern` must be a single non-empty character value"
    )
  }
)


# Test 13: Numbered dataset pages are supported

testthat::test_that(
  "get_resource_link finds numbered dataset pages",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publication/august-2024/datasets2">
            Data sets
          </a>
        </body>
      </html>
    '
    
    dataset_html <- '
      <html>
        <body>
          <a href="/files/test-august-2024.zip">
            CSV Data (as ZIP) August 2024
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets2$", x)){
          xml2::read_html(dataset_html)
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    result <- suppressMessages(
      get_resource_link(
        publication_url = "https://example.com/publication/august-2024",
        resource_text = "CSV Data \\(as ZIP\\)",
        dataset_pattern = "/datasets[^/]*/?$",
        match_period = FALSE,
        file_pattern = "\\.zip($|\\?)"
      )
    )
    
    testthat::expect_equal(
      result,
      "https://example.com/files/test-august-2024.zip"
    )
  }
)


# Test 14: Dataset pages with additional text are supported

testthat::test_that(
  "get_resource_link finds dataset pages with additional text",
  {
    
    publication_html <- '
      <html>
        <body>
          <a href="/publication/july-2026/datasets---at">
            Data sets
          </a>
        </body>
      </html>
    '
    
    dataset_html <- '
      <html>
        <body>
          <a href="/files/test-july-2026.zip">
            CSV Data (as ZIP) July 2026
          </a>
        </body>
      </html>
    '
    
    testthat::local_mocked_bindings(
      read_html = function(x){
        
        if(grepl("/datasets---at$", x)){
          xml2::read_html(dataset_html)
        } else {
          xml2::read_html(publication_html)
        }
      },
      .package = "rvest"
    )
    
    result <- suppressMessages(
      get_resource_link(
        publication_url = "https://example.com/publication/july-2026",
        resource_text = "CSV Data \\(as ZIP\\)",
        dataset_pattern = "/datasets[^/]*/?$",
        match_period = FALSE,
        file_pattern = "\\.zip($|\\?)"
      )
    )
    
    testthat::expect_equal(
      result,
      "https://example.com/files/test-july-2026.zip"
    )
  }
)