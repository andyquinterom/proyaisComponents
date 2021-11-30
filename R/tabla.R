#' @export
tabla <- R6::R6Class(
  classname = "tabla mutable",
  public = list(
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
    reset = function() {
      self$tabla <- self$base
    },
    mod_reset = function() {
      self$mod_list <- tibble::tibble(
        id = character(0),
        priority = integer(0),
        f = list()
      )
    },
    mod_list = NULL,
    mod_add = function(id, priority, .f) {
      new_row <- tibble::tibble(
        id = id,
        priority = priority,
        f = c(.f)
      )
      self$mod_list <- self$mod_list %>%
        dplyr::rows_upsert(new_row, by = "id")
    },
    mod_rm = function(id) {
      if (id %in% self$mod_list$id) {
        delete_row <- tibble::tibble(id = id)
        self$mod_list <- self$mod_list %>%
          dplyr::rows_delete(delete_row, by = "id")
      }
    },
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
    query = function() {
      if ("tbl_sql" %in% class(self$tabla)) {
        return(dbplyr::sql_render(self$tabla))
      }
      return(digest::digest(self$tabla, algo = "sha256"))
    },
    base = NULL,
    tabla = NULL,
    colnames = NULL,
    colnames_num = NULL
  )
)
