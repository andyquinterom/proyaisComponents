#' UI para filtros numéricos
#'
#' @param id ID del módulo
#' @return Objeto tags para UI de una aplicación Shiny
#' @export
filtros_numericos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    class = "filtros",
    shinyWidgets::actionGroupButtons(
      inputIds = ns(c("filtros_num_add", "filtros_num_rm")),
      labels = c("+", "-"),
      size = "sm"
    ),
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::tags$div(
      id = ns("filtros_num")
    ),
  )

}

#' Servidor para filtros numéricos
#'
#' @param id ID del módulo
#' @param datos Objecto de class "tabla_mutable"
#' @param max_num Número de filtros máximo
#' @details
#' Para poder aplicar los filtros debe llamar a un trigger con el ID del módulo.
#' En caso de que se actualice el objeto de tabla mutable, se trendra que llamar
#' un trigger con el siguiente formato id-actualizar. Este reseteará el
#' UI del módulo para adaptarse a la nueva tabla.
#' @examples
#' # Para actualizar el módulo a una nueva tabla mutable
#' gargoyle::trigger("id-actualizar")
#' # Para aplicar los filtros y meterlos en la lista de modificaciones
#' gargoyle::trigger("id")
#' @description Filtros se aplican con un trigger con el ID del módulo.
#' @importFrom magrittr %>%
#' @export
filtros_numericos_server <- function(id, datos, max_num = 20) {

  ns <- shiny::NS(id)

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # cantidad de filtros numericos
      # cantidad de filtros de variables caracteres

      gargoyle::init(id, ns("actualizar"))

      filtros <- shiny::reactiveValues(
        n_num = 0
      )

      shiny::observe({
        filtros$n_char <- 0
        for (i in seq_len(max_num)) {
          shiny::updateSelectizeInput(
            inputId = paste0("filtro_num_columna_", i),
            selected = "Ninguno",
            choices = c("Ninguno", datos$colnames_num)
          )
          shiny::updateNumericInput(
            inputId = paste0("filtro_num_min_", i),
            value = NA
          )
          shiny::updateNumericInput(
            inputId = paste0("filtro_num_max_", i),
            value = NA
          )
        }
      }) %>%
        shiny::bindEvent(gargoyle::watch(ns("actualizar")))

      shiny::observe({
        if (filtros$n_num < max_num) {
          shiny::insertUI(
            selector = paste0("#", ns("filtros_num")),
            where = "beforeEnd",
            ui = shiny::tags$div(
              id = ns(paste0("filtro_num_", filtros$n_num + 1)),
              shiny::fluidRow(
                shiny::column(
                  width = 5,
                  shiny::selectizeInput(
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
                shiny::column(width = 2),
                shiny::column(
                  width = 5,
                  shiny::fluidRow(
                    shiny::column(
                      width = 6,
                      shiny::numericInput(
                        inputId = ns(
                          paste0("filtro_num_min_", filtros$n_num + 1)
                        ),
                        label = NULL,
                        value = NA,
                        min = 0,
                        max = 0,
                        width = "100%")),
                    shiny::column(
                      width = 6,
                      shiny::numericInput(
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
          shiny::showNotification(
            "No se pueden insertar más filtros",
            type = "error"
          )
        }
      }) %>%
        shiny::bindEvent(input$filtros_num_add, TRUE)

      shiny::observe({
        if (filtros$n_num > 0) {
          shiny::updateSelectizeInput(
            inputId = paste0("filtro_num_columna_", filtros$n_num),
            selected = "Ninguno"
          )
          shiny::removeUI(
            selector = glue::glue(
              "#{ ns(paste0('filtro_num_', filtros$n_num)) }"
            )
          )
          filtros$n_num <- filtros$n_num - 1
        } else {
          shiny::showNotification(
            "Se requiere por lo menos un filtro",
            type = "error"
          )
        }
      }) %>%
        shiny::bindEvent(input$filtros_num_rm)

      lapply(
        X = 1:max_num,
        FUN = function(i) {
          shiny::observe({
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
            shiny::bindEvent(gargoyle::watch(id))
        }
      )

    }
  )
}
