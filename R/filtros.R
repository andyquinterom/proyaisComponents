#' UI para filtros discretos
#'
#' @param id ID del módulo
#' @return Objeto tags para UI de una aplicación Shiny
#' @export
filtros_discretos_ui <- function(id) {
  ns <- shiny::NS(id)

  tags$div(
    class = "filtros",
    shinyWidgets::actionGroupButtons(
      inputIds = ns(c("filtros_char_add", "filtros_char_rm")),
      labels = c("+", "-"),
      size = "sm"
    ),
    tags$br(),
    tags$br(),
    tags$div(
      id = ns("filtros_char")
    ),
  )

}

#' Servidor para filtros discretos
#'
#' @param id ID del módulo
#' @param datos Objecto de class "tabla mutable"
#' @param cache ReactiveValues para el cache de shinyCache
#' @param max_char Número de filtros máximo
#' @description Filtros se aplican con un trigger con el ID del módulo.
#' @export
filtros_discretos_server <- function(id, datos, cache, max_char = 20) {

  ns <- shiny::NS(id)

  moduleServer(
    id = id,
    module = function(input, output, session) {
      aplicar_count <- counter()
      # cantidad de filtros numericos
      # cantidad de filtros de variables caracteres

      filtros <- reactiveValues(
        n_char = 0,
        selected_char = lapply(1:max_char, function(x) "Ninguno"),
        selected_num = lapply(1:max_char, function(x) "Ninguno"),
        colnames = colnames(datos$base)
      )

      observe({
        if (filtros$n_char < max_char) {
          insertUI(
            selector = paste0("#", ns("filtros_char")),
            where = "beforeEnd",
            ui = tags$div(
              id = ns(paste0("filtro_char_", filtros$n_char + 1)),
              fluidRow(
                column(
                  width = 5,
                  selectizeInput(
                    inputId = ns(paste(
                      "filtro_char_columna",
                      filtros$n_char + 1,
                      sep = "_"
                    )),
                    label = NULL,
                    choices = c("Ninguno", filtros$colnames),
                    selected = "Ninguno",
                    multiple = FALSE
                  )
                ),
                column(
                  width = 2,
                  shinyWidgets::switchInput(
                    inputId = ns(paste(
                      "filtro_char_incluir",
                      filtros$n_char + 1,
                      sep = "_"
                    )),
                    onLabel = "Incluir",
                    offLabel = "Excluir",
                    width = "100%",
                    value = TRUE
                  )
                ),
                column(
                  width = 5,
                  addPreserveSearch(selectizeInput(
                    inputId = ns(paste(
                      "filtro_char_valor",
                      filtros$n_char + 1,
                      sep = "_"
                    )),
                    label = NULL,
                    choices = "Ninguno",
                    selected = "Ninguno",
                    options = list(plugins = list("preserve_search")),
                    multiple = TRUE
                  ))
                )
              )
            )
          )
          filtros$n_char <- filtros$n_char + 1
        } else {
          showNotification(
            "No se pueden insertar más filtros",
            type = "error"
          )
        }
      }) %>%
        bindEvent(input$filtros_char_add, TRUE)

      observe({
        if (filtros$n_char > 1) {
          updateSelectizeInput(
            inputId = paste0("filtro_char_columna_", filtros$n_char),
            selected = "Ninguno"
          )
          removeUI(
            selector = glue::glue(
              "#{ ns(paste0('filtro_char_', filtros$n_char)) }"
            )
          )
          filtros$n_char <- filtros$n_char - 1
        } else {
          showNotification(
            "Se requiere por lo menos un filtro",
            type = "error"
          )
        }
      }) %>%
        bindEvent(input$filtros_char_rm)

      lapply(
        X = 1:max_char,
        FUN = function(i) {
          observe({
            updateSelectizeInput(
              session = session,
              inputId = paste0("filtro_char_valor_", i),
              server = TRUE,
              selected = filtros$selected_char[[i]],
              choices = {
                columna_seleccionada <-
                  input[[paste0("filtro_char_columna_", i)]]
                if (!columna_seleccionada %in% c("Ninguno", "")) {
                  shinyCache::cache_call(
                    fn = pull_distinct,
                    cache = cache,
                    cache_depends = {
                      list(datos$query())
                    },
                    cache_params = list(col = columna_seleccionada),
                    non_cache_params = list(
                      data = datos$base
                    ),
                  )
                } else {
                  "Ninguno"
                }
              }
            )
          }) %>%
            bindEvent(input[[paste0("filtro_char_columna_", i)]])
        }
      )

      lapply(
        X = 1:max_char,
        FUN = function(i) {
          observe({
            columna <- input[[paste0("filtro_char_columna_", i)]]
            filtro_id <- paste0(id, "-", i)
            if (is.null(columna)) columna <- "Ninguno"
            if (columna %in% c("Ninguno", "")) {
              datos$mod_rm(id = filtro_id)
            } else {
              incluir <- input[[paste0("filtro_char_incluir_", i)]]
              valor <- input[[paste0("filtro_char_valor_", i)]]
              filtro_function <- function(.data) {
                if (incluir) {
                  return(dplyr::filter(.data, !!as.name(columna) %in% !!valor))
                }
                return(dplyr::filter(.data, !(!!as.name(columna) %in% !!valor)))
              }
              datos$mod_add(
                id = filtro_id,
                priority = 10,
                .f = c(filtro_function)
              )
            }
          }) %>%
            bindEvent(gargoyle::watch(id))
        }
      )

    }
  )
}

addPreserveSearch <- function(x) {
  preserve_search <- htmltools::htmlDependency(
    name = "preserve_search",
    version = "1.0",
    src = system.file("deps", package = "proyaisComponents"),
    script = "preserve_search-1.0.js",
    stylesheet = "filtros.css"
  )
  htmltools::attachDependencies(
    x,
    c(htmltools::htmlDependencies(x), list(preserve_search))
  )
}

counter <- function() {
  x <- 0
  function() {
    x <<- x + 1
    return(x)
  }
}

pull_distinct <- function(data, col) {
  data %>%
    dplyr::select(!!rlang::sym(col)) %>%
    dplyr::distinct() %>%
    dplyr::pull(!!rlang::sym(col))
}
