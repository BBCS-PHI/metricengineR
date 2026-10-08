#' Get Publication Data
#'
#' Retrieves publication pages from a parent web page, identifies matching
#' downloadable resources, downloads the files, and combines CSV data.
#'
#' Dataset pages are identified from links on each publication page using a
#' regular expression. This allows dataset pages such as `/datasets`,
#' `/datasets2`, and `/datasets---at` to be discovered automatically.
#'
#' Downloaded ZIP resources are extracted and their CSV files are combined.
#' Direct CSV resources are read without extraction.
#'
#' A progress bar is displayed while resources are downloaded. Publications
#' that cannot be successfully processed are reported at the end of the
#' process and returned in `failed_resources`.
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
#' @param dataset_pattern Regular expression used to identify the dataset page
#'   from links found on each publication page. Defaults to
#'   `"/datasets[^/]*/?$"`.
#' @param period_pattern Regular expression used to extract the reporting
#'   period from publication URLs. Defaults to
#'   `"[a-z]+-[0-9]{4}/?$"`.
#' @param match_period Logical. If `TRUE`, the resource link text must contain
#'   the reporting period extracted from the publication URL.
#' @param file_pattern Regular expression used to identify the required file
#'   type. Defaults to ZIP or CSV files using
#'   `"\\.(zip|csv)($|\\?)"`.
#' @param all_columns_character Logical. If `TRUE`, all CSV columns are read as
#'   character values. Defaults to `TRUE`.
#'
#' @return A list containing:
#' \itemize{
#'   \item `data` - combined CSV data from successfully processed resources.
#'   \item `catalogue` - publication URLs, resource URLs, downloaded files,
#'   file types, extracted folders, and processing status.
#'   \item `failed_resources` - publications that could not be successfully
#'   processed and the reason for failure.
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
#'   dataset_pattern = "/datasets[^/]*/?$",
#'   period_pattern = "[a-z]+-[0-9]{4}/?$",
#'   match_period = TRUE,
#'   file_pattern = "\\.(zip|csv)($|\\?)",
#'   all_columns_character = TRUE
#' )
#'
#' csds_result$data
#' csds_result$catalogue
#' csds_result$failed_resources
#' }
#'
#' @export
get_publication_data <- function(
    parent_url,
    publication_pattern,
    resource_text,
    download_folder,
    dataset_pattern = "/datasets[^/]*/?$",
    period_pattern = "[a-z]+-[0-9]{4}/?$",
    match_period = TRUE,
    file_pattern = "\\.(zip|csv)($|\\?)",
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
  
  if(
    !is.character(period_pattern) ||
    length(period_pattern) != 1L ||
    is.na(period_pattern) ||
    !nzchar(period_pattern)
  ){
    stop(
      "`period_pattern` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(file_pattern) ||
    length(file_pattern) != 1L ||
    is.na(file_pattern) ||
    !nzchar(file_pattern)
  ){
    stop(
      "`file_pattern` must be a single non-empty character value.",
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
  
  
  # Return empty result if no publication pages are found
  
  if(length(publication_links) == 0L){
    
    cli::cli_alert_warning(
      "No matching publication pages were found."
    )
    
    return(
      list(
        data = tibble::tibble(),
        catalogue = tibble::tibble(),
        failed_resources = tibble::tibble(),
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
        resource_text = resource_text,
        dataset_pattern = dataset_pattern,
        period_pattern = period_pattern,
        match_period = match_period,
        file_pattern = file_pattern
      ),
      downloaded_file = NA_character_,
      file_type = NA_character_,
      extracted_folder = NA_character_,
      processing_status = dplyr::if_else(
        is.na(.data$resource_url),
        "Resource not found",
        "Pending"
      )
    )
  
  
  # Identify resources available for download
  
  download_rows <- which(
    !is.na(resource_catalogue$resource_url)
  )
  
  
  # Download resources
  
  if(length(download_rows) > 0L){
    
    cli::cli_alert_info(
      "Downloading {length(download_rows)} resource(s)."
    )
    
    progress_id <- cli::cli_progress_bar(
      name = "Downloading resources",
      total = length(download_rows)
    )
    
    
    for(i in download_rows){
      
      resource_catalogue$downloaded_file[i] <- download_resource(
        resource_url = resource_catalogue$resource_url[i],
        download_folder = download_folder
      )
      
      cli::cli_progress_update(
        id = progress_id
      )
    }
    
    
    cli::cli_progress_done(
      id = progress_id
    )
    
  } else {
    
    cli::cli_alert_warning(
      "No matching downloadable resources were found."
    )
  }
  
  
  # Identify downloaded file type
  
  resource_catalogue <- resource_catalogue |>
    dplyr::mutate(
      file_type = purrr::map_chr(
        .data$downloaded_file,
        function(downloaded_file){
          
          if(is.na(downloaded_file)){
            return(
              NA_character_
            )
          }
          
          downloaded_file |>
            stringr::str_remove(
              "\\?.*$"
            ) |>
            tools::file_ext() |>
            stringr::str_to_lower()
        }
      )
    )
  
  
  # Extract ZIP resources
  
  resource_catalogue <- resource_catalogue |>
    dplyr::mutate(
      extracted_folder = purrr::map2_chr(
        .data$downloaded_file,
        .data$file_type,
        function(downloaded_file, file_type){
          
          if(
            is.na(downloaded_file) ||
            is.na(file_type) ||
            file_type != "zip"
          ){
            return(
              NA_character_
            )
          }
          
          extract_zip(
            zip_file = downloaded_file,
            extract_folder = download_folder
          )
        }
      )
    )
  
  
  # Record initial processing status
  
  resource_catalogue <- resource_catalogue |>
    dplyr::mutate(
      processing_status = dplyr::case_when(
        
        is.na(.data$resource_url) ~
          "Resource not found",
        
        is.na(.data$downloaded_file) ~
          "Download failed",
        
        .data$file_type == "zip" &
          is.na(.data$extracted_folder) ~
          "Extraction failed",
        
        !is.na(.data$file_type) &
          !(.data$file_type %in% c("zip", "csv")) ~
          "Unsupported file type",
        
        TRUE ~
          "Ready to read"
      )
    )
  
  
  # Read downloaded resources
  
  cli::cli_alert_info(
    "Reading downloaded resources."
  )
  
  data_list <- vector(
    "list",
    nrow(resource_catalogue)
  )
  
  
  for(i in seq_len(nrow(resource_catalogue))){
    
    # Skip resources that failed before the read stage
    
    if(resource_catalogue$processing_status[i] != "Ready to read"){
      next
    }
    
    
    # Read ZIP resource
    
    if(resource_catalogue$file_type[i] == "zip"){
      
      resource_data <- tryCatch(
        {
          read_csv_folder(
            folder = resource_catalogue$extracted_folder[i],
            all_columns_character = all_columns_character
          )
        },
        error = function(e){
          
          cli::cli_alert_warning(
            paste0(
              "Could not read extracted resource ",
              resource_catalogue$publication_url[i],
              ": ",
              conditionMessage(e)
            )
          )
          
          NULL
        }
      )
      
      
      if(is.null(resource_data)){
        
        resource_catalogue$processing_status[i] <- "Read failed"
        next
      }
      
      
      if(ncol(resource_data) == 0L){
        
        resource_catalogue$processing_status[i] <- "No CSV data found"
        next
      }
      
      
      data_list[[i]] <- resource_data
      resource_catalogue$processing_status[i] <- "Processed"
    }
    
    
    # Read direct CSV resource
    
    if(resource_catalogue$file_type[i] == "csv"){
      
      cli::cli_alert_info(
        "Reading CSV: {basename(resource_catalogue$downloaded_file[i])}"
      )
      
      resource_data <- tryCatch(
        {
          
          if(all_columns_character){
            
            readr::read_csv(
              resource_catalogue$downloaded_file[i],
              col_types = readr::cols(
                .default = readr::col_character()
              ),
              show_col_types = FALSE,
              progress = FALSE
            )
            
          } else {
            
            readr::read_csv(
              resource_catalogue$downloaded_file[i],
              show_col_types = FALSE,
              progress = FALSE
            )
          }
        },
        error = function(e){
          
          cli::cli_alert_warning(
            paste0(
              "Could not read CSV ",
              resource_catalogue$downloaded_file[i],
              ": ",
              conditionMessage(e)
            )
          )
          
          NULL
        }
      )
      
      
      if(is.null(resource_data)){
        
        resource_catalogue$processing_status[i] <- "Read failed"
        next
      }
      
      
      # Add source information
      
      resource_data <- resource_data |>
        dplyr::mutate(
          source_file = basename(
            resource_catalogue$downloaded_file[i]
          ),
          source_folder = basename(
            dirname(
              resource_catalogue$downloaded_file[i]
            )
          ),
          .before = 1
        )
      
      
      data_list[[i]] <- resource_data
      resource_catalogue$processing_status[i] <- "Processed"
    }
  }
  
  
  # Combine successfully processed data
  
  combined_data <- dplyr::bind_rows(
    data_list
  )
  
  
  # Count successfully processed resources
  
  processed_resources <- sum(
    resource_catalogue$processing_status == "Processed",
    na.rm = TRUE
  )
  
  
  # Identify resources that could not be processed
  
  failed_resources <- resource_catalogue |>
    dplyr::filter(
      .data$processing_status != "Processed"
    )
  
  
  # Report summary
  
  cli::cli_alert_success(
    paste0(
      "Publication extraction completed with ",
      nrow(combined_data),
      " row(s) retrieved from ",
      processed_resources,
      " successfully processed resource(s)."
    )
  )
  
  
  # Report failed resources
  
  if(nrow(failed_resources) > 0L){
    
    cli::cli_alert_warning(
      "{nrow(failed_resources)} publication resource(s) could not be successfully processed:"
    )
    
    
    for(i in seq_len(nrow(failed_resources))){
      
      cli::cli_bullets(
        c(
          "!" = paste0(
            failed_resources$publication_url[i],
            " - ",
            failed_resources$processing_status[i]
          )
        )
      )
    }
  }
  
  
  # Return outputs
  
  list(
    data = combined_data,
    catalogue = resource_catalogue,
    failed_resources = failed_resources,
    publication_links = publication_links
  )
}