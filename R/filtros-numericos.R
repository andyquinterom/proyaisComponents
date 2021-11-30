#' UI para filtros numéricos
#'
#' @param id ID del módulo
#' @return Objeto tags para UI de una aplicación Shiny
#' @export
filtros_numericos_ui <- function(id) {
  ns <- shiny::NS(id)

  tags$div(
    class = "filtros",
    shinyWidgets::actionGroupButtons(
      inputIds = ns(c("filtros_num_add", "filtros_num_rm")),
      labels = c("+", "-"),
      size = "sm"
    ),
    tags$br(),
    tags$br(),
    tags$div(
      id = ns("filtros_num")
    ),
  )

}

#' Servidor para filtros numéricos
#'
#' @param id ID del módulo
#' @param datos Objecto de class "tabla mutable"
#' @param cache ReactiveValues para el cache de shinyCache
#' @param max_num Número de filtros máximo
#' @description Filtros se aplican con un trigger con el ID del módulo.
#' @export
filtros_numericos_server <- function(id, datos, cache, max_num = 20) {

  ns <- shiny::NS(id)

  moduleServer(
    id = id,
    module = function(input, output, session) {
      # cantidad de filtros numericos
      # cantidad de filtros de variables caracteres

      gargoyle::init(id, ns("actualizar"))

      filtros <- reactiveValues(
        n_num = 0
      )

      observe({
        filtros$n_char <- 0
        for (i in seq_len(max_num)) {
          updateSelectizeInput(
            inputId = paste0("filtro_num_columna_", i),
            selected = "Ninguno",
            choices = c("Ninguno", datos$colnames_num)
          )
          updateNumericInput(
            inputId = paste0("filtro_num_min_", i),
            value = NA
          )
          updateNumericInput(
            inputId = paste0("filtro_num_max_", i),
            value = NA
          )
        }
      }) %>%
        bindEvent(watch(ns("actualizar")))

      observe({
        if (filtros$n_num < max_num) {
          insertUI(
            selector = paste0("#", ns("filtros_num")),
            where = "beforeEnd",
            ui = tags$div(
              id = ns(paste0("filtro_num_", filtros$n_num + 1)),
              fluidRow(
                column(
                  width = 5,
                  selectizeInput(
                    inputId = ns(paste(
                      "filtro_num_columna",
                      filtros$n_num + 1,
                      sep = "_"
                    )),
                    label = NULL,
                    choices = c("Ninguno", datos$colnames_num),
                    selected = "Ninguno",
                    multiple = FALSE
                  )
                ),
                column(width = 2),
                column(
                  width = 5,
                  fluidRow(
                    column(
                      width = 6,
                      numericInput(
                        inputId = ns(
                          paste0("filtro_num_min_", filtros$n_num + 1)
                        ),
                        label = NULL,
                        value = NA,
                        min = 0,
                        max = 0,
                        width = "100%")),
                    column(
                      width = 6,
                      numericInput(
                        inputId = ns(
                          paste0("filtro_num_max_", filtros$n_num + 1)
                        ),
                        label = NULL,
                        value = NA,
                        min = 0,
                        max = 0,
                        width = "100%"
                      )
                    )
                  )
                )
              )
            )
          )
          filtros$n_num <- filtros$n_num + 1
        } else {
          showNotification(
            "No se pueden insertar más filtros",
            type = "error"
          )
        }
      }) %>%
        bindEvent(input$filtros_num_add, TRUE)

      observe({
        if (filtros$n_num > 0) {
          updateSelectizeInput(
            inputId = paste0("filtro_num_columna_", filtros$n_num),
            selected = "Ninguno"
          )
          removeUI(
            selector = glue::glue(
              "#{ ns(paste0('filtro_num_', filtros$n_num)) }"
            )
          )
          filtros$n_num <- filtros$n_num - 1
        } else {
          showNotification(
            "Se requiere por lo menos un filtro",
            type = "error"
          )
        }
      }) %>%
        bindEvent(input$filtros_num_rm)

      lapply(
        X = 1:max_num,
        FUN = function(i) {
          observe({
            columna <- input[[paste0("filtro_num_columna_", i)]]
            filtro_id <- paste0(id, "-", i)
            if (is.null(columna)) columna <- "Ninguno"
            if (columna %in% c("Ninguno", "")) {
              datos$mod_rm(id = filtro_id)
            } else {
              minimo <- input[[paste0("filtro_num_min_", i)]]
              maximo <- input[[paste0("filtro_num_max_", i)]]
              filtro_expresion <- paste(
                c(if (!is.na(minimo)) glue::glue("{columna} >= {minimo}"),
                  if (!is.na(maximo)) glue::glue("{columna} <= {maximo}")),
                collapse = " & "
              )
              filtro_function <- function(.data) {
                if (filtro_expresion == "") {
                  return(.data)
                }
                return(
                  dplyr::filter(.data, !!rlang::parse_expr(filtro_expresion))
                )
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
