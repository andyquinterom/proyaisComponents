#' UI para filtros discretos
#'
#' @param id ID del módulo
#' @return Objeto tags para UI de una aplicación Shiny
#' @export
filtros_discretos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    class = "filtros",
    shinyWidgets::actionGroupButtons(
      inputIds = ns(c("filtros_char_add", "filtros_char_rm")),
      labels = c("+", "-"),
      size = "sm"
    ),
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::tags$div(
      id = ns("filtros_char"),
      # Este es un div que inicializa un switchInput para evitar errores
      # con el UI. El error parece ser con bootstrap, este es un work around
      # que funciona
      shiny::tags$div(
        style = "display: none;",
        shinyWidgets::switchInput(inputId = ns("garbage"))
      )
    ),
  )

}

#' Servidor para filtros discretos
#'
#' @param id ID del módulo
#' @param datos Objecto de class "tabla_mutable"
#' @param cache ReactiveValues para el cache de shinyCache
#' @param max_char Número de filtros máximo
#' @importFrom magrittr %>%
#' @description Filtros se aplican con un trigger con el ID del módulo.
#' @export
filtros_discretos_server <- function(id, datos, cache, max_char = 20) {

  ns <- shiny::NS(id)

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # cantidad de filtros numericos
      # cantidad de filtros de variables caracteres

      gargoyle::init(id, ns("actualizar"))

      filtros <- shiny::reactiveValues(n_char = 0)

      shiny::observe({
        filtros$n_char <- 0
        for (i in seq_len(max_char)) {
          shiny::updateSelectizeInput(
            inputId = paste0("filtro_char_columna_", i),
            selected = "Ninguno",
            choices = c("Ninguno", datos$colnames)
          )
        }
      }) %>%
        shiny::bindEvent(gargoyle::watch(ns("actualizar")))

      shiny::observe({
        if (filtros$n_char < max_char) {
          shiny::insertUI(
            selector = paste0("#", ns("filtros_char")),
            where = "beforeEnd",
            ui = shiny::tags$div(
              id = ns(paste0("filtro_char_", filtros$n_char + 1)),
              shiny::fluidRow(
                shiny::column(
                  width = 5,
                  shiny::selectizeInput(
                    inputId = ns(paste(
                      "filtro_char_columna",
                      filtros$n_char + 1,
                      sep = "_"
                    )),
                    label = NULL,
                    choices = c("Ninguno", datos$colnames),
                    width = "100%",
                    selected = "Ninguno",
                    multiple = FALSE
                  )
                ),
                shiny::column(
                  width = 2,
                  shinyWidgets::switchInput(
                    inputId = ns(paste(
                      "filtro_char_incluir",
                      filtros$n_char + 1,
                      sep = "_"
                    )),
                    onLabel = "Incluir",
                    offLabel = "Excluir",
                    inline = TRUE,
                    value = TRUE
                  )
                ),
                shiny::column(
                  width = 5,
                  addPreserveSearch(shiny::selectizeInput(
                    inputId = ns(paste(
                      "filtro_char_valor",
                      filtros$n_char + 1,
                      sep = "_"
                    )),
                    label = NULL,
                    width = "100%",
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
          shiny::showNotification(
            "No se pueden insertar más filtros",
            type = "error"
          )
        }
      }) %>%
        shiny::bindEvent(input$filtros_char_add, TRUE)

      shiny::observe({
        if (filtros$n_char > 0) {
          shiny::updateSelectizeInput(
            inputId = paste0("filtro_char_columna_", filtros$n_char),
            selected = "Ninguno"
          )
          shiny::removeUI(
            selector = glue::glue(
              "#{ ns(paste0('filtro_char_', filtros$n_char)) }"
            )
          )
          filtros$n_char <- filtros$n_char - 1
        } else {
          shiny::showNotification(
            "No hay filtros.",
            type = "error"
          )
        }
      }) %>%
        shiny::bindEvent(input$filtros_char_rm)

     lapply(
       X = 1:max_char,
       FUN = function(i) {
         shiny::observe({
           shiny::updateSelectizeInput(
             session = session,
             inputId = paste0("filtro_char_valor_", i),
             server = TRUE,
             selected = NULL,
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
           shiny::bindEvent(input[[paste0("filtro_char_columna_", i)]])
       }
     )

      lapply(
        X = 1:max_char,
        FUN = function(i) {
          shiny::observe({
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
            shiny::bindEvent(gargoyle::watch(id))
        }
      )

    }
  )
}

pull_distinct <- function(data, col) {
  data %>%
    dplyr::select(!!rlang::sym(col)) %>%
    dplyr::distinct() %>%
    dplyr::pull(!!rlang::sym(col))
}
