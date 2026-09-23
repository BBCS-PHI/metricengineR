#' Run a SQL script from a file
#'
#' Reads a SQL script from a file and executes it against an existing
#' database connection.
#'
#' @param conn An active database connection, typically created using
#'   `DBI::dbConnect()`.
#'
#' @param path A character scalar specifying the path to the SQL file
#'   to be executed.
#'
#' @return `NULL` invisibly when the SQL script runs successfully.
#'   A success message is printed to the console.
#'
#' @details
#' The SQL file is read line by line using `readLines()` and collapsed into
#' a single SQL string. The resulting SQL script is then executed using
#' `DBI::dbExecute()`.
#'
#' If the SQL script is executed successfully, a confirmation message is
#' printed containing the name of the SQL file.
#'
#' If an error occurs while reading or executing the SQL script, the error
#' is caught using `tryCatch()`. A descriptive error message is raised using
#' `stop()`, including the SQL filename and the original database or file
#' error message. Execution of the current pipeline is stopped.
#'
#' @section Dependencies:
#' This function uses:
#'
#' \itemize{
#'   \item `readLines()` - reads the contents of the SQL file.
#'   \item `basename()` - extracts the SQL filename from the supplied path.
#'   \item `DBI::dbExecute()` - executes the SQL script against the database.
#'   \item `tryCatch()` - handles errors encountered during execution.
#' }
#'
#' @export
run_sql_file <- function(conn, path) {
  
  if(!file.exists(path)){
    stop(
      "FAIL: SQL file does not exist: ",
      path,
      call. = FALSE
    )
  }
  
  tryCatch(
    {
      
      sql_text <- paste(
        readLines(
          path,
          warn = FALSE
        ),
        collapse = "\n"
      )
      
      DBI::dbExecute(
        conn,
        sql_text
      )
      
      cli::cli_alert_success(
        "SQL script successfully run: {basename(path)}"
        )
    },
    
    error = function(e){
      
      stop(
        paste0(
          "FAIL: SQL script failed: ",
          basename(path),
          " - ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}


