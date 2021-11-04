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
#' @param tbl_reactive Reactive con un tibble o lazy tibble
#' @param tbl_name Nombre de tabla en base de datos
#' @param id ID del módulo
#' @param cache ReactiveValues para el cache de shinyCache
#' @param conn Conexión a DBI en caso de pasar tbl_name
#' @param max_char Número de filtros máximo
#' @return Objeto tags para UI de una aplicación Shiny
#' @export
filtros_discretos_server <- function(tbl_reactive, tbl_name, id, cache,
  conn = NULL, max_char = 20) {

  if (missing(tbl_name)) {
    tbl_input <- tbl_reactive
  } else {
    tbl_input <- reactive({
      dplyr::tbl(conn, tbl_name)
    })
  }

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
        colnames = colnames(tbl_input())
      )

      observe({
        if (filtros$n_char < max_char) {
          insertUI(
            selector = paste0("#", ns("filtros_char")),
            where = "beforeEnd",
            ui = tags$div(
              id = paste0("filtro_char_", filtros$n_char + 1),
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
          removeUI(
            selector = paste0("#filtro_char_", filtros$n_char)
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
          observeEvent(input[[paste0("filtro_char_columna_", i)]], {
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
                      if ("tbl_lazy" %in% class(tbl_input())) {
                        list(dbplyr::sql_render(tbl_input()))
                      } else {
                        list(tbl_input())
                      }
                    },
                    cache_params = list(col = columna_seleccionada),
                    non_cache_params = list(
                      data = tbl_input()
                    ),
                  )
                } else {
                  "Ninguno"
                }
              }
            )
          })
        }
      )

      tbl_return <- reactive({
        tabla <- tbl_input()
        inputs_filtros_char <- c()
        inputs_filtros_char <- unlist(
          lapply(
            X = 1:filtros$n_char,
            FUN = function(i) {
              return(!input[[paste0("filtro_char_columna_", i)]] %in%
                c("Ninguno", ""))
            }
          )
        )

        n_filtros_char <- sum(inputs_filtros_char)

        lapply(
          X = (1:filtros$n_char)[inputs_filtros_char],
          FUN = function(i) {
            valores_filtro <- input[[paste0("filtro_char_valor_", i)]]
            columna <- input[[paste0("filtro_char_columna_", i)]]
            if (input[[paste0("filtro_char_incluir_", i)]]) {
              tabla <<- tabla %>%
                dplyr::filter(!!as.name(columna) %in% valores_filtro)
            } else {
              tabla <<- tabla %>%
                dplyr::filter(!(!!as.name(columna) %in% valores_filtro))
            }
          }
        )

        n_filtros_total <- n_filtros_char

        showNotification(
          ui = paste("Se aplicaron", n_filtros_total, "filtros."),
          duration = 4
        )

        return(tabla)

      })

      return(tbl_return)

    }
  )
}

addPreserveSearch <- function(x) {
  preserve_search <- htmltools::htmlDependency(
    "preserve_search", "1.0", "deps",
    script = "preserve_search.js",
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
