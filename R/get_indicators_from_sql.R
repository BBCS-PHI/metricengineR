#' Extract indicators from a SQL table
#'
#' Extracts indicator data from a SQL database table using an existing
#' database connection. The function can return all indicators or filter
#' the data to a specified set of indicator IDs. All columns can be returned,
#' or extraction can be restricted to a specified set of columns.
#'
#' @param conn An active database connection, typically created using
#'   `DBI::dbConnect()`.
#'
#' @param schema_name A character scalar specifying the database schema
#'   containing the table.
#'
#' @param table_name A character scalar specifying the name of the SQL table
#'   from which indicator data should be extracted.
#'
#' @param database_name An optional character scalar specifying the database
#'   containing the table. If `NULL`, the database associated with the
#'   existing connection is used. Defaults to `NULL`.
#'
#' @param indicator_ids An optional numeric or character vector containing
#'   valid indicator IDs to extract. Accepted values include:
#'
#'   \itemize{
#'     \item `NULL` to extract all indicators.
#'     \item `"All"` to extract all indicators.
#'     \item A numeric vector such as `c(1, 2, 3)`.
#'     \item A character vector such as `c("1", "2", "3")`.
#'     \item A single comma-separated character string, such as
#'       `"1, 2, 3"`.
#'   }
#'
#'   `"All"` must be supplied on its own. Defaults to `NULL`.
#'
#' @param columns An optional character vector containing the names of columns
#'   to extract from the SQL table. If `NULL`, all columns are extracted.
#'   For example, `c("indicator_id", "start_date", "numerator")`.
#'   Defaults to `NULL`.
#'
#' @return A data frame containing the records and columns returned by the SQL
#'   query. On successful execution, messages are printed confirming that the
#'   extraction completed and reporting the total number of rows extracted.
#'
#' @details
#' The function first normalises `indicator_ids` using
#' `normalise_indicator_ids()`.
#'
#' If `columns` is supplied, it must be a non-empty character vector without
#' missing values. Duplicate column names are removed before the SQL query is
#' constructed.
#'
#' A SQL table identifier is created using `DBI::Id()`. When `database_name`
#' is supplied, the identifier contains the database, schema, and table. When
#' `database_name` is `NULL`, only the schema and table are used.
#'
#' The table identifier is safely quoted using `DBI::dbQuoteIdentifier()`.
#'
#' If `columns` is `NULL`, the SQL query uses `SELECT *` and all columns are
#' returned. Otherwise, the supplied column names are safely quoted using
#' `DBI::dbQuoteIdentifier()` and only those columns are included in the
#' `SELECT` clause.
#'
#' If `indicator_ids` is `NULL` or equal to `"All"`, all indicators from the
#' specified table are returned. Otherwise, each indicator ID is safely
#' quoted using `DBI::dbQuoteLiteral()` and added to a
#' `WHERE indicator_id IN (...)` clause.
#'
#' The resulting SQL query is executed using `DBI::dbGetQuery()`.
#'
#' Errors occurring while constructing or executing the SQL query are caught
#' using `tryCatch()`. If an error occurs, a descriptive error is raised using
#' `stop()` and execution of the current pipeline is stopped.
#'
#' @section Dependencies:
#' This function uses:
#'
#' \itemize{
#'   \item `DBI::Id()` - creates a database table identifier.
#'   \item `DBI::dbQuoteIdentifier()` - safely quotes the database table
#'     identifier and selected column names.
#'   \item `DBI::dbQuoteLiteral()` - safely quotes indicator ID values.
#'   \item `DBI::dbGetQuery()` - executes the SQL query and returns the result.
#'   \item `tryCatch()` - handles errors during SQL query construction and
#'     execution.
#' }
#'
#' @section Internal dependencies:
#' This function depends on:
#'
#' \itemize{
#'   \item `normalise_indicator_ids()` - normalises the supplied indicator IDs
#'     and identifies requests to extract all indicators.
#' }
#'
#' @export
get_indicators_from_sql <- function(
    conn,
    schema_name,
    table_name,
    database_name = NULL,
    indicator_ids = NULL,
    columns = NULL
) {
  
  # Normalise indicator IDs
  
  ids <- normalise_indicator_ids(indicator_ids)
  
  # Validate column names
  
  if(!is.null(columns)){
    
    if(
      !is.character(columns) ||
      length(columns) == 0L ||
      anyNA(columns)
    ){
      stop(
        "`columns` must be NULL or a character vector of column names.",
        call. = FALSE
      )
    }
    
    columns <- unique(columns)
  }
  
  tryCatch(
    {
      
      # Create table identifier
      
      if(is.null(database_name)){
        
        table_id <- DBI::Id(
          schema = schema_name,
          table = table_name
        )
        
      } else {
        
        table_id <- DBI::Id(
          catalog = database_name,
          schema = schema_name,
          table = table_name
        )
      }
      
      
      quoted_table <- DBI::dbQuoteIdentifier(
        conn,
        table_id
      )
      
      
      # Create SELECT clause
      
      if(is.null(columns)){
        
        select_clause <- "*"
        
      } else{
        
        quoted_columns <- DBI::dbQuoteIdentifier(
          conn,
          columns
        )
        
        select_clause <- paste(
          quoted_columns,
          collapse = ", "
        )
      }
      
      # Create base query
      
      sql_query <- paste0(
        "SELECT ",
        select_clause,
        " FROM ",
        quoted_table
      )
      
      
      # Build WHERE clause when indicator IDs are provided
      
      if(!is.null(ids) && length(ids) > 0L){
        
        quoted_ids <- vapply(
          ids,
          function(x){
            as.character(
              DBI::dbQuoteLiteral(
                conn,
                x
              )
            )
          },
          character(1)
        )
        
        sql_query <- paste0(
          sql_query,
          " WHERE indicator_id IN (",
          paste(
            quoted_ids,
            collapse = ", "
          ),
          ")"
        )
        
        cli::cli_alert_info(
          "Extracting {length(ids)} indicator(s) from SQL."
        )
        
      } else {
        
        cli::cli_alert_info(
          "Extracting all indicators from SQL."
        )
      }
      
      
      # Run query
      
      result <- DBI::dbGetQuery(
        conn,
        sql_query
      )
      
      
      # Report result
      
      cli::cli_alert_success(
        "Indicators successfully extracted from SQL."
      )
      
      cli::cli_alert_info(
        "Total rows extracted: {nrow(result)}"
      )
      
      
      result
    },
    
    error = function(e){
      
      stop(
        paste0(
          "FAIL: Failed to extract indicators from SQL table ",
          table_name,
          " - ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}