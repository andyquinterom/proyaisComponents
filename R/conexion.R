#' @export
conexion <- R6::R6Class(
  classname = "conexion",
  public = list(
    initialize = function() {
      self$conn <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = Sys.getenv("DATABASE_NAME"),
        user = Sys.getenv("DATABASE_USER"),
        password = Sys.getenv("DATABASE_PW"),
        host = Sys.getenv("DATABASE_HOST"),
        port = Sys.getenv("DATABASE_PORT"),
        bigint = "integer64",
        sslmode = "allow"
      )
    },
    conn = NULL,
    execute = function(...) DBI::dbExecute(conn = self$conn, ...),
    send_query = function(...) DBI::dbGetQuery(conn = self$conn, ...),
    list_tables = function() DBI::dbListTables(conn = self$conn),
    write_table = function(...) DBI::dbWriteTable(conn = self$conn, ...),
    escape = function(...) DBI::sqlInterpolate(conn = self$conn, ...),
    tbl = function(...) dplyr::tbl(src = self$conn, ...)
  )
)
