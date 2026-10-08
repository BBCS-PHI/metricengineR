#' Get Resource Link
#'
#' Retrieves a resource download link from a publication dataset page.
#'
#' @param publication_url URL of the publication page.
#' @param dataset_pattern Regular expression used to identify the dataset page
#'   from links found on the publication page. Defaults to
#'   `"/datasets[^/]*/?$"`, allowing dataset pages such as `/datasets`,
#'   `/datasets2`, and `/datasets---at`.
#' @param resource_text Text used to identify the required resource link.
#'   Matching is case-insensitive.
#' @param period_pattern Regular expression used to extract the reporting
#'   period from the publication URL. Defaults to
#'   `"[a-z]+-[0-9]{4}/?$"`.
#' @param match_period Logical. If `TRUE`, the resource link text must contain
#'   the reporting period extracted from `publication_url`.
#' @param file_pattern Regular expression used to identify the required file
#'   type. Defaults to ZIP or CSV files using
#'   `"\\.(zip|csv)($|\\?)"`.
#'   
#' @return A character value containing the resource URL, or `NA_character_`
#'   if no matching resource is found.
#'
#' @examples
#' \dontrun{
#' resource_url <- get_resource_link(
#'   publication_url = paste0(
#'     "https://digital.nhs.uk/data-and-information/publications/",
#'     "statistical/community-services-statistics-for-children-young-people-and-adults/",
#'     "june-2026"
#'   ),
#'   resource_text = "CSV Data \\(as ZIP\\)",
#'   period_pattern = "[a-z]+-[0-9]{4}/?$",
#'   match_period = TRUE,
#'   file_pattern = "\\.(zip|csv)($|\\?)"
#' )
#' }
#' @export
get_resource_link <- function(
    publication_url,
    resource_text,
    dataset_pattern = "/datasets[^/]*/?$",
    period_pattern = "[a-z]+-[0-9]{4}/?$",
    match_period = TRUE,
    file_pattern = "\\.(zip|csv)($|\\?)"
) {
  
  # Validate inputs
  
  if(
    !is.character(publication_url) ||
    length(publication_url) != 1L ||
    is.na(publication_url) ||
    !nzchar(publication_url)
  ){
    stop(
      "`publication_url` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(resource_text) ||
    length(resource_text) != 1L ||
    is.na(resource_text) ||
    !nzchar(resource_text)
  ){
    stop(
      "`resource_text` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.logical(match_period) ||
    length(match_period) != 1L ||
    is.na(match_period)
  ){
    stop(
      "`match_period` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(dataset_pattern) ||
    length(dataset_pattern) != 1L ||
    is.na(dataset_pattern) ||
    !nzchar(dataset_pattern)
  ){
    stop(
      "`dataset_pattern` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  # Read publication page
  
  cli::cli_alert_info(
    "Finding dataset page: {publication_url}"
  )
  
  publication_page <- tryCatch(
    rvest::read_html(
      publication_url
    ),
    error = function(e){
      cli::cli_alert_warning(
        "Could not read publication page: {publication_url}"
      )
      return(NULL)
    }
  )
  
  if(is.null(publication_page)){
    return(NA_character_)
  }
  
  
  # Find dataset page
  # Get all dataset download links
  
  dataset_links <- publication_page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stats::na.omit() |>
    unique()
  
  # Detect dataset links using the supplied dataset patterm
  dataset_links <- dataset_links[
    stringr::str_detect(
      dataset_links,
      dataset_pattern
    )
  ]
  
  # Return NA if no dataset page can be found
  
  if(length(dataset_links) == 0L){
    
    cli::cli_alert_warning(
      "No dataset page found for: {publication_url}"
    )
    
    return(NA_character_)
  }
  
  
  # Convert dataset link to absolute URL
  
  dataset_url <- make_absolute_url(
    dataset_links[[1]],
    publication_url
  )
  
  
  cli::cli_alert_info(
    "Checking dataset page: {dataset_url}"
  )
  
  
  # Read dataset page
  
  page <- tryCatch(
    {
      rvest::read_html(
        dataset_url
      )
    },
    error = function(e){
      
      cli::cli_alert_warning(
        "Could not read dataset page: {dataset_url}"
      )
      
      NULL
    }
  )
  
  
  if(is.null(page)){
    return(
      NA_character_
    )
  }
  
  
  # Extract resource links
  
  link_nodes <- page |>
    rvest::html_elements("a")
  
  links <- tibble::tibble(
    link_text = rvest::html_text2(
      link_nodes
    ),
    link_url = rvest::html_attr(
      link_nodes,
      "href"
    )
  ) |>
    dplyr::filter(
      !is.na(.data$link_url),
      stringr::str_detect(
        .data$link_text,
        stringr::regex(
          resource_text,
          ignore_case = TRUE
        )
      ),
      stringr::str_detect(
        .data$link_url,
        stringr::regex(
          file_pattern,
          ignore_case = TRUE
        )
      )
    )
  
  
  # Match publication period
  
  if(match_period){
    
    publication_period <- publication_url |>
      stringr::str_extract(
        period_pattern
      ) |>
      stringr::str_remove(
        "/$"
      )
    
    
    if(is.na(publication_period)){
      stop(
        "`period_pattern` could not identify a reporting period in `publication_url`.",
        call. = FALSE
      )
    }
    
    
    publication_period_text <- publication_period |>
      stringr::str_replace_all(
        "-",
        " "
      ) |>
      stringr::str_to_title()
    
    
    links <- links |>
      dplyr::filter(
        stringr::str_detect(
          .data$link_text,
          stringr::fixed(
            publication_period_text,
            ignore_case = TRUE
          )
        )
      )
  }
  
  
  # Get unique resource URLs
  
  links <- links |>
    dplyr::pull(
      .data$link_url
    ) |>
    unique()
  
  
  if(length(links) == 0L){
    
    cli::cli_alert_warning(
      "No matching resource found for: {publication_url}"
    )
    
    return(
      NA_character_
    )
  }
  
  
  # Convert resource link to absolute URL
  
  make_absolute_url(
    links[[1]],
    dataset_url
  )
}