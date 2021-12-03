#' R6 Class encapsula los datos y métodos para una tabla mutable
#'
#' @description
#' Un objeto creado con esta clase podrá ser mutado de manera sencilla
#' agregando modificaciones y aplicandolas. Modificaciones especificas
#' se pueden agregar, modificar o eliminar con los métodos encapsulados aqui.
#' Las modificaciones se pasan con una función de un solo argumento que
#' modifique los datos. Cada modificación lleva una prioridad, entre más alta
#' esta prioridad, se aplicará antes.
#' @examples
#' example_data <- data.frame(n = 1:100)
#' number_table <- tabla$new(example_data)
#' number_table$mod_add(
#'   id = "x2",
#'   priority = 1,
#'   .f = function(.data) {
#'     .data$n <- .data$n * 2
#'     return(.data)
#'   }
#' )
#' number_table$mod_aplicar()
#' print(number_table$table)
#' number_table$reset()
#' @importFrom magrittr %>%
#' @export
tabla <- R6::R6Class(
  classname = "tabla_mutable",
  public = list(
    #' @description
    #' Crea una tabla mutable
    #' @param .data Tibble, Data.table, Tbl SQL, etc...
    initialize = function(.data) {
      self$base <- .data
      self$tabla <- .data
      self$mod_list <- tibble::tibble(
        id = character(0),
        priority = integer(0),
        f = list()
      )
      self$colnames <- colnames(.data)
      self$colnames_num <- colnames(
        dplyr::select_if(
          dplyr::collect(head(.data, 0)),
          is.numeric
        )
      )
    },
    #' @description
    #' Devuelve la tabla a su estado original
    reset = function() {
      self$tabla <- self$base
    },
    #' @description
    #' Elimina todas las modificaciones a la tabla
    mod_reset = function() {
      self$mod_list <- tibble::tibble(
        id = character(0),
        priority = integer(0),
        f = list()
      )
    },
    #' @field mod_list Lista de modificaciones para hacer a la tabla
    mod_list = NULL,
    #' @description
    #' Agrega una modificación para hacerse a la tabla
    #' @param id Un ID único para la modificación
    #' @param priority Nivel de prioridad de la modificación (define el orden)
    #' @param .f función con un solo argumento para aplicar a la tabla
    mod_add = function(id, priority, .f) {
      new_row <- tibble::tibble(
        id = id,
        priority = priority,
        f = c(.f)
      )
      self$mod_list <- self$mod_list %>%
        dplyr::rows_upsert(new_row, by = "id")
    },
    #' @description
    #' Remueve alguna modificación de la lista
    #' @param id El ID único de la modificación
    mod_rm = function(id) {
      if (id %in% self$mod_list$id) {
        delete_row <- tibble::tibble(id = id)
        self$mod_list <- self$mod_list %>%
          dplyr::rows_delete(delete_row, by = "id")
      }
    },
    #' @description
    #' Aplica todas las modificaciones en la lista en orden según la prioridad.
    mod_aplicar = function() {
      temp <- self$base
      mod_list <- self$mod_list %>%
        dplyr::arrange(dplyt::desc(priority)) %>%
        dplyr::pull(f)

      for (curr_func in mod_list) {
          temp <- curr_func(temp)
      }

      self$tabla <- temp
    },
    #' @description
    #' Extrae el SQL query o un hash que identifique el estado actual
    #' de la tabla.
    query = function() {
      if ("tbl_sql" %in% class(self$tabla)) {
        return(dbplyr::sql_render(self$tabla))
      }
      return(digest::digest(self$tabla, algo = "sha256"))
    },
    #' @field base Los datos base (originales) del objeto.
    base = NULL,
    #' @field tabla La tabla en estado mutable.
    tabla = NULL,
    #' @field colnames Las columnas de los datos base
    colnames = NULL,
    #' @field colnames_num Las columnas de tipo númerico de los datos base
    colnames_num = NULL
  )
)
