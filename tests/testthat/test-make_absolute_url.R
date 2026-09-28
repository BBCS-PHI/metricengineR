test_that("make_absolute_url converts a relative URL to an absolute URL", {
  
  result <- make_absolute_url(
    "/data-and-information/test",
    "https://digital.nhs.uk"
  )
  
  expect_equal(
    result,
    "https://digital.nhs.uk/data-and-information/test"
  )
})


test_that("make_absolute_url keeps an absolute URL unchanged", {
  
  result <- make_absolute_url(
    "https://digital.nhs.uk/test",
    "https://digital.nhs.uk"
  )
  
  expect_equal(
    result,
    "https://digital.nhs.uk/test"
  )
})


test_that("make_absolute_url validates link", {
  
  expect_error(
    make_absolute_url(
      123,
      "https://digital.nhs.uk"
    ),
    "`link` must be a character vector"
  )
})


test_that("make_absolute_url validates base_url", {
  
  expect_error(
    make_absolute_url(
      "/test",
      c(
        "https://digital.nhs.uk",
        "https://example.com"
      )
    ),
    "`base_url` must be a single character value"
  )
})