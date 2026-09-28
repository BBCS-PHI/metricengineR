#' Get Publication Data
#'
#' Retrieves publication pages from a parent web page, identifies matching
#' downloadable resources, downloads and extracts the files, and combines
#' CSV data from the extracted folders.
#'
#' This function acts as an orchestration wrapper around:
#' `get_child_links()`, `get_resource_link()`, `download_resource()`,
#' `extract_zip()`, and `read_csv_folder()`.
#'
#' @param parent_url URL of the parent publication page containing links to
#'   individual publication pages.
#' @param publication_pattern Regular expression used to identify publication
#'   links from the parent page.
#' @param resource_text Text used to identify the required downloadable
#'   resource on each publication dataset page.
#' @param download_folder Folder where downloaded and extracted files should
#'   be stored.
#' @param dataset_suffix Suffix appended to each publication URL to locate the
#'   dataset page. Defaults to `"/datasets"`.
#' @param period_pattern Regular expression used to extract the reporting
#'   period from publication URLs. Defaults to
#'   `"[a-z]+-[0-9]{4}/?$"`.
#' @param match_period Logical. If `TRUE`, the resource link text must contain
#'   the reporting period extracted from the publication URL.
#' @param file_pattern Regular expression used to identify the required file
#'   type. Defaults to ZIP files using `"\\.zip($|\\?)"`.
#' @param all_columns_character Logical. If `TRUE`, all CSV columns are read as
#'   character values. Defaults to `TRUE`.
#'
#' @return A list containing:
#' \itemize{
#'   \item `data` - combined CSV data from successfully extracted resources.
#'   \item `catalogue` - publication URLs, resource URLs, downloaded files,
#'   and extracted folders.
#'   \item `publication_links` - publication links identified from the parent
#'   page.
#' }
#'
#' @examples
#' \dontrun{
#' csds_result <- get_publication_data(
#'   parent_url = paste0(
#'     "https://digital.nhs.uk/data-and-information/publications/",
#'     "statistical/community-services-statistics-for-children-young-people-and-adults"
#'   ),
#'   publication_pattern = paste0(
#'     "community-services-statistics-for-children-young-people-and-adults/",
#'     "[a-z]+-[0-9]{4}/?$"
#'   ),
#'   resource_text = "CSV Data \\(as ZIP\\)",
#'   download_folder = "data/CSDS_downloads",
#'   dataset_suffix = "/datasets",
#'   period_pattern = "[a-z]+-[0-9]{4}/?$",
#'   match_period = TRUE,
#'   file_pattern = "\\.zip($|\\?)",
#'   all_columns_character = TRUE
#' )
#'
#' csds_result$data
#' csds_result$catalogue
#' }
#'
#' @export
get_publication_data <- function(
    parent_url,
    publication_pattern,
    resource_text,
    download_folder,
    dataset_suffix = "/datasets",
    period_pattern = "[a-z]+-[0-9]{4}/?$",
    match_period = TRUE,
    file_pattern = "\\.zip($|\\?)",
    all_columns_character = TRUE
) {
  
  # Validate inputs
  
  if(
    !is.character(parent_url) ||
    length(parent_url) != 1L ||
    is.na(parent_url) ||
    !nzchar(parent_url)
  ){
    stop(
      "`parent_url` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(publication_pattern) ||
    length(publication_pattern) != 1L ||
    is.na(publication_pattern) ||
    !nzchar(publication_pattern)
  ){
    stop(
      "`publication_pattern` must be a single non-empty character value.",
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
    !is.character(download_folder) ||
    length(download_folder) != 1L ||
    is.na(download_folder) ||
    !nzchar(download_folder)
  ){
    stop(
      "`download_folder` must be a single non-empty character value.",
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
    !is.logical(all_columns_character) ||
    length(all_columns_character) != 1L ||
    is.na(all_columns_character)
  ){
    stop(
      "`all_columns_character` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  
  # Create download folder
  
  fs::dir_create(
    download_folder,
    recurse = TRUE
  )
  
  
  # Retrieve publication links
  
  cli::cli_alert_info(
    "Retrieving publication links."
  )
  
  publication_links <- get_child_links(
    parent_url = parent_url,
    publication_pattern = publication_pattern
  )
  
  
  if(length(publication_links) == 0L){
    
    cli::cli_alert_warning(
      "No matching publication pages were found."
    )
    
    return(
      list(
        data = tibble::tibble(),
        catalogue = tibble::tibble(),
        publication_links = character(0)
      )
    )
  }
  
  
  cli::cli_alert_info(
    "Processing {length(publication_links)} publication page(s)."
  )
  
  
  # Build resource catalogue
  
  resource_catalogue <- tibble::tibble(
    publication_url = publication_links
  ) |>
    dplyr::mutate(
      resource_url = purrr::map_chr(
        .data$publication_url,
        get_resource_link,
        dataset_suffix = dataset_suffix,
        resource_text = resource_text,
        period_pattern = period_pattern,
        match_period = match_period,
        file_pattern = file_pattern
      )
    ) |>
    dplyr::filter(
      !is.na(.data$resource_url)
    ) |>
    dplyr::distinct(
      .data$resource_url,
      .keep_all = TRUE
    )
  
  
  if(nrow(resource_catalogue) == 0L){
    
    cli::cli_alert_warning(
      "No matching downloadable resources were found."
    )
    
    return(
      list(
        data = tibble::tibble(),
        catalogue = resource_catalogue,
        publication_links = publication_links
      )
    )
  }
  
  
  cli::cli_alert_info(
    "Downloading {nrow(resource_catalogue)} resource(s)."
  )
  
  
  # Download and extract resources
  
  resource_catalogue <- resource_catalogue |>
    dplyr::mutate(
      downloaded_file = purrr::map_chr(
        .data$resource_url,
        download_resource,
        download_folder = download_folder
      ),
      
      extracted_folder = purrr::map_chr(
        .data$downloaded_file,
        extract_zip,
        extract_folder = download_folder
      )
    )
  
  
  # Read only folders extracted during this run
  
  extracted_folders <- resource_catalogue$extracted_folder |>
    purrr::discard(is.na)
  
  
  if(length(extracted_folders) == 0L){
    
    cli::cli_alert_warning(
      "No resources were successfully extracted."
    )
    
    combined_data <- tibble::tibble()
    
  } else {
    
    cli::cli_alert_info(
      "Reading CSV files from {length(extracted_folders)} extracted folder(s)."
    )
    
    combined_data <- purrr::map_dfr(
      extracted_folders,
      read_csv_folder,
      all_columns_character = all_columns_character
    )
  }
  
  
  # Report summary
  
  cli::cli_alert_success(
    paste0(
      "Publication extraction completed with ",
      nrow(combined_data),
      " row(s) retrieved from ",
      length(extracted_folders),
      " extracted resource(s)."
    )
  )
  
  
  # Return outputs
  
  list(
    data = combined_data,
    catalogue = resource_catalogue,
    publication_links = publication_links
  )
}