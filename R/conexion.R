#' R6 Class encapsula las necesidades basicas de una conexion a Postgres
#'
#' @import bit64
#' @export
conexion <- R6::R6Class(
  classname = "conexion",
  public = list(
    #' @description
    #' Crea nuevo objeto de conexión
    #' @details
    #' Esta función requiere unas variables de ambiente cargadas para poder
    #' functionar. DATABASE_NAME para el nombre de la base de datos;
    #' DATABASE_USER para el usuario de autenticación;
    #' DATABASE_PW para la contraseña de dicho usuario;
    #' DATABASE_HOST para la dirección IP de la base de datos;
    #' DATABASE_PORT para el puerto de la base de datos.
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
    #' @field conn Conexión DBI para pasar a funciones
    conn = NULL,
    #' @description Método para DBI::dbExecute
    #' @param ... Argumentos de DBI::dbExecute
    execute = function(...) DBI::dbExecute(conn = self$conn, ...),
    #' @description Método para DBI::dbGetQuery
    #' @param ... Argumentos de DBI::dbGetQuery
    send_query = function(...) DBI::dbGetQuery(conn = self$conn, ...),
    #' @description Método para DBI::dbListTables
    #' @param ... Argumentos de DBI::dbListTables
    list_tables = function() DBI::dbListTables(conn = self$conn),
    #' @description Método para DBI::dbWriteTable
    #' @param ... Argumentos de DBI::dbWriteTable
    write_table = function(...) DBI::dbWriteTable(conn = self$conn, ...),
    #' @description Método para DBI::dbRemoveTable
    #' @param ... Argumentos de DBI::dbRemoveTable
    remove_table = function(...) DBI::dbRemoveTable(conn = self$conn, ...),
    #' @description Método para DBI::sqlInterpolate
    #' @param ... Argumentos de DBI::sqlInterpolate
    escape = function(...) DBI::sqlInterpolate(conn = self$conn, ...),
    #' @description Método para dplyr::tbl
    #' @param ... Argumentos de dplyr::tbl
    tbl = function(...) dplyr::tbl(src = self$conn, ...)
  )
)
