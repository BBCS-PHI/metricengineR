
test_that("run_sql_file runs a valid SQL script successfully", {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn))
  
  sql_file <- tempfile(fileext = ".sql")
  
  writeLines(
    "CREATE TABLE test_table (id INTEGER);",
    sql_file
  )
  
  on.exit(unlink(sql_file), add = TRUE)
  
  expect_message(
    run_sql_file(conn, sql_file),
    "SQL script successfully run"
  )
  
  expect_true(
    DBI::dbExistsTable(conn, "test_table")
  )
})

test_that("run_sql_file stops when the SQL script fails", {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn))
  
  sql_file <- tempfile(fileext = ".sql")
  
  writeLines(
    "THIS IS NOT VALID SQL;",
    sql_file
  )
  
  on.exit(unlink(sql_file), add = TRUE)
  
  expect_error(
    run_sql_file(conn, sql_file),
    "SQL script failed"
  )
})

test_that("run_sql_file stops when the SQL file does not exist", {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn))
  
  testthat::expect_error(
    run_sql_file(
      conn,
      "file_that_does_not_exist.sql"
    ),
    "SQL file does not exist"
  )
  
})