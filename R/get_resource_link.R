#' Get Resource Link
#'
#' Retrieves a resource download link from a publication dataset page.
#'
#' @param publication_url URL of the publication page.
#' @param dataset_suffix Suffix appended to the publication URL to locate the
#'   dataset page. Defaults to `"/datasets"`.
#' @param resource_text Text used to identify the required resource link.
#'   Matching is case-insensitive.
#' @param period_pattern Regular expression used to extract the reporting
#'   period from the publication URL. Defaults to
#'   `"[a-z]+-[0-9]{4}/?$"`.
#' @param match_period Logical. If `TRUE`, the resource link text must contain
#'   the reporting period extracted from `publication_url`.
#' @param file_pattern Regular expression used to identify the required file
#'   type. Defaults to ZIP files using `"\\.zip($|\\?)"`.
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
#'   dataset_suffix = "/datasets",
#'   resource_text = "CSV Data \\(as ZIP\\)",
#'   period_pattern = "[a-z]+-[0-9]{4}/?$",
#'   match_period = TRUE,
#'   file_pattern = "\\.zip($|\\?)"
#' )
#'
#' resource_url
#' }
#'
#' @export
get_resource_link <- function(
    publication_url,
    dataset_suffix = "/datasets",
    resource_text,
    period_pattern = "[a-z]+-[0-9]{4}/?$",
    match_period = TRUE,
    file_pattern = "\\.zip($|\\?)"
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
  
  
  # Build dataset URL
  
  dataset_url <- paste0(
    stringr::str_remove(
      publication_url,
      "/$"
    ),
    dataset_suffix
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