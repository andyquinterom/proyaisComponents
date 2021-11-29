conn <- conexion$new()
numero_filtros <- 20

# Datos didacticos para el test, es practicamente un matriz identidad con
# todos los valores debajo de la identidad identicos.
datos_compound_test <- purrr::map_dfc(
  .x = purrr::set_names(
    x = seq_len(numero_filtros),
    nm = paste0("COL", seq_len(numero_filtros))
  ),
  max_n = numero_filtros,
  .f = function(n, max_n) {
    n <- n - 1
    sequencia <- c()
    sequencia <- rep("NO", n)
    sequencia <- c(sequencia, rep("SI", max_n - n))
    return(sequencia)
  }
)

conn$write_table("filtros_test", datos_compound_test, temporary = TRUE)

counter <- function() {
  x <- 0
  function() {
    x <<- x + 1
    return(x)
  }
}

aplicar <- counter()
# Función para reset todos los filtros
reset_filtros <- function(session) {
  purrr::map(
    .x = 1:numero_filtros,
    .f = function(i) {
      run <- glue::glue(
        'session$setInputs(
          filtro_char_columna_{i} = "",
          filtro_char_valor_{i} = NULL,
          filtro_char_incluir_{i} = TRUE
        )
        session$setInputs(aplicar_filtros = {aplicar()})'
      )
      eval(parse(text = run))
    }
  )
}

datos_db <- tabla$new(dplyr::tbl(conn$conn, "filtros_test"))
datos_local <- tabla$new(dplyr::collect(datos_db$base))

# Validar que el módulo corra cuando se utiliza tbl_name y no tbl_reactive
shiny::testServer(
  app = proyaisComponents::filtros_discretos_server,
  args = list(
    cache = reactiveValues(),
    datos = datos_db
  ),
  expr = {
    # Simple test para validar que corra el módulo
    expect_equal(1, 1)
  }
)

 # Test completo con lazy loading
shiny::testServer(
  app = proyaisComponents::filtros_discretos_server,
  args = list(
    cache = reactiveValues(),
    datos = datos_db
  ),
  expr = {
    # Habilito todos los filtros
    purrr::map(
      .x = 1:numero_filtros,
      .f = function(i) {
        session$setInputs(filtros_char_add = i)
      }
    )

    # Para evitar posibles error (improbable pero uno no sabe) reseteo los
    # filtros previo a correr los tests
    reset_filtros(session)

    # Primer test, voy filtro por filtro, aplicando los valores de cada columna
    # tanto excluyendo como incluyendo

    # La primera iteración (x) indica la fila de filtro que sera aplicado
    # La segunda iteración (incluir) dice si incluir o excluir
    # La tercera iteración (i) inclica sobre cual columna aplicar el filtro
    purrr::map(
      .x = 1:numero_filtros,
      .f = function(x) {
        purrr::map(
          .x = c(TRUE, FALSE),
          .f = function(incluir) {
            purrr::map(
              .x = 1:numero_filtros,
              .f = function(i) {
                run <- glue::glue(
                  'session$setInputs(
                    filtro_char_columna_{x} = paste0("COL", i),
                    filtro_char_valor_{x} = "SI",
                    filtro_char_incluir_{x} = {incluir}
                  )
                  session$setInputs(aplicar_filtros = {aplicar()})'
                )
                eval(parse(text = run))
                datos$mod_aplicar()
                expect_equal(
                  datos_db$tabla %>% dplyr::collect(),
                  if (incluir) {
                    datos_compound_test %>% tail(numero_filtros - i + 1)
                  } else {
                    datos_compound_test %>% head(i - 1)
                  }
                )
                reset_filtros(session)
              }
            )
          }
        )
      }
    )

#    # Filtros con multiples valores
#    # Se aplica a cada fila y cada columna un filtro incluyendo tanto
#    # "SI" como "NO", la tabla siempre debe estar completa.
#    purrr::map(
#      .x = 1:numero_filtros,
#      .f = function(i) {
#        run <- glue::glue(
#          'session$setInputs(
#            filtro_char_columna_{i} = paste0("COL", i),
#            filtro_char_valor_{i} = c("SI", "NO"),
#            filtro_char_incluir_{i} = TRUE
#          )
#          session$setInputs(aplicar_filtros = {aplicar()})'
#        )
#        eval(parse(text = run))
#        expect_equal(
#          datos_db$tabla %>% dplyr::collect(),
#          datos_compound_test
#        )
#        reset_filtros(session)
#      }
#    )
#
#    # Filtros con multiples valores
#    # Se aplica a cada fila y cada columna un filtro excluyendo tanto
#    # "SI" como "NO", la tabla siempre debe estar vacia
#    purrr::map(
#      .x = 1:numero_filtros,
#      .f = function(i) {
#        run <- glue::glue(
#          'session$setInputs(
#            filtro_char_columna_{i} = paste0("COL", i),
#            filtro_char_valor_{i} = c("SI", "NO"),
#            filtro_char_incluir_{i} = FALSE
#          )
#          session$setInputs(aplicar_filtros = {aplicar()})'
#        )
#        eval(parse(text = run))
#        expect_equal(
#          datos$tabla %>% dplyr::collect(),
#          datos_compound_test %>% tail(0)
#        )
#        reset_filtros(session)
#      }
#    )
#
#    # Si el usuario no tiene ningun valor seleccionado en el filtro
#    # la tabla debe aparecer vacia
#    purrr::map(
#      .x = 1:numero_filtros,
#      .f = function(i) {
#        run <- glue::glue(
#          'session$setInputs(
#            filtro_char_columna_{i} = paste0("COL", i),
#            filtro_char_valor_{i} = NULL,
#            filtro_char_incluir_{i} = TRUE
#          )
#          session$setInputs(aplicar_filtros = {aplicar()})'
#        )
#        eval(parse(text = run))
#        expect_equal(
#          datos$tabla %>% dplyr::collect(),
#          datos_compound_test %>% tail(0)
#        )
#        reset_filtros(session)
#      }
#    )
#
#    # Filtros compound
#    # Aplica los filtros a cada fila y cada columna de forma compuesta.
#    # No se elimina el filtro anterior.
#    purrr::map(
#      .x = 1:numero_filtros,
#      .f = function(i) {
#        run <- glue::glue(
#          'session$setInputs(
#            filtro_char_columna_{i} = paste0("COL", i),
#            filtro_char_valor_{i} = "SI",
#            filtro_char_incluir_{i} = TRUE
#          )
#          session$setInputs(aplicar_filtros = {aplicar()})'
#        )
#        eval(parse(text = run))
#        expect_equal(
#          datos$tabla %>% dplyr::collect(),
#          datos_compound_test %>% tail(numero_filtros - i + 1)
#        )
#      }
#    )
#
#    reset_filtros(session)
#
#    # Filtros compuestos con exclusion
#    purrr::map(
#      .x = numero_filtros:1,
#      .f = function(i) {
#        run <- glue::glue(
#          'session$setInputs(
#            filtro_char_columna_{i} = paste0("COL", i),
#            filtro_char_valor_{i} = "SI",
#            filtro_char_incluir_{i} = FALSE
#          )
#          session$setInputs(aplicar_filtros = {aplicar()})'
#        )
#        eval(parse(text = run))
#        expect_equal(
#          datos_db$tabla %>% dplyr::collect(),
#          datos_compound_test %>% head(i - 1)
#        )
#      }
#    )
#    # Se quitan los filtros y se testea si la tabla vuelve a su forma original
#    reset_filtros(session)
#    expect_equal(
#      datos_db$tabla %>% dplyr::collect(),
#      datos_compound_test
#    )
  }
)
