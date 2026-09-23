#' Get Fingertips Indicators
#'
#' Retrieves data for one or more indicator IDs from the Fingertips API.
#'
#' @param indicator_ids Vector of Fingertips indicator IDs to retrieve.
#' @param delay_seconds Number of seconds to wait between API requests.
#'   Defaults to `0.5`.
#'
#' @return A data frame containing the combined data for all successfully
#'   retrieved indicators. Returns an empty data frame if all requests fail.
#'
#' @details
#' Indicators are requested one at a time. A short delay is applied between
#' requests to reduce the chance of API errors when retrieving multiple
#' indicators.
#'
#' If an individual request fails, a warning message is printed and the
#' function continues with the remaining indicators.
#'
#' @export
get_fingertips_indicators <- function(
    indicator_ids,
    delay_seconds = 0.5
) {
  
  # Validate inputs
  
  if(
    length(indicator_ids) == 0L ||
    any(is.na(indicator_ids))
  ){
    stop(
      "`indicator_ids` must contain at least one non-missing indicator ID.",
      call. = FALSE
    )
  }
  
  if(
    !is.numeric(delay_seconds) ||
    length(delay_seconds) != 1L ||
    is.na(delay_seconds) ||
    delay_seconds < 0
  ){
    stop(
      "`delay_seconds` must be a single number greater than or equal to 0.",
      call. = FALSE
    )
  }
  
  
  # Prepare indicator IDs
  
  indicator_ids <- unique(
    as.character(indicator_ids)
  )
  
  
  # Fingertips API URL
  
  base_url <- paste0(
    "https://fingertips.phe.org.uk/api/all_data/csv/",
    "by_indicator_id?indicator_ids="
  )
  
  
  # Store results
  
  results_list <- vector(
    "list",
    length(indicator_ids)
  )
  
  names(results_list) <- indicator_ids
  
  
  # Report start of extraction
  
  cli::cli_alert_info(
    "Retrieving {length(indicator_ids)} Fingertips indicator(s)."
  )
  
  
  # Create progress bar
  
  progress_id <- cli::cli_progress_bar(
    name = "Retrieving Fingertips indicators",
    total = length(indicator_ids)
  )
  
  
  # Retrieve each indicator
  
  for(i in seq_along(indicator_ids)){
    
    id <- indicator_ids[i]
    
    url <- paste0(
      base_url,
      id
    )
    
    result <- tryCatch(
      
      {
        
        response <- httr::GET(
          url
        )
        
        httr::stop_for_status(
          response
        )
        
        content_text <- httr::content(
          response,
          as = "text",
          encoding = "UTF-8"
        )
        
        data <- utils::read.csv(
          text = content_text,
          stringsAsFactors = FALSE
        )
        
        data
      },
      
      error = function(e){
        
        cli::cli_alert_warning(
          "Could not retrieve indicator {id}: {conditionMessage(e)}"
        )
        
        NULL
      }
    )
    
    
    # Store result
    
    results_list[[i]] <- result
    
    
    # Update progress bar
    
    cli::cli_progress_update(
      id = progress_id,
      inc = 1
    )
    
    
    # Pause before the next API request
    
    if(i < length(indicator_ids)){
      
      Sys.sleep(
        delay_seconds
      )
    }
  }
  
  
  # Complete progress bar
  
  cli::cli_progress_done(
    id = progress_id
  )
  
  
  # Remove failed requests
  
  successful_results <- results_list[
    !vapply(
      results_list,
      is.null,
      logical(1)
    )
  ]
  
  
  # Calculate retrieval summary
  
  successful_count <- length(
    successful_results
  )
  
  failed_count <- length(indicator_ids) -
    successful_count
  
  
  # Return empty data frame if all requests failed
  
  if(successful_count == 0L){
    
    cli::cli_alert_warning(
      "No Fingertips indicators were successfully retrieved."
    )
    
    return(
      data.frame()
    )
  }
  
  
  # Report successful extraction
  
  cli::cli_alert_success(
    "Successfully retrieved {successful_count} of {length(indicator_ids)} Fingertips indicator(s)."
  )
  
  if(failed_count > 0L){
    
    cli::cli_alert_warning(
      "{failed_count} indicator request(s) failed."
    )
  }
  
  
  # Combine successful results
  
  dplyr::bind_rows(
    successful_results
  )
}