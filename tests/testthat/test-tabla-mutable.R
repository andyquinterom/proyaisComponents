library(testthat)

iris_mutable <- tabla$new(iris)

test_that("Nueva tabla mutable", {

  expect_equal(
    iris_mutable$base,
    iris
  )

  expect_equal(
    iris_mutable$tabla,
    iris
  )

  iris_mutable$mod_aplicar()

  expect_equal(
    iris_mutable$tabla,
    iris
  )

  expect_equal(
    iris_mutable$colnames,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    iris_mutable$colnames_num,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

})

test_that("Aplicar modificaciones", {

  iris_mutable$mod_add(
    id = "MOD1",
    priority = 1,
    .f = function(.data) {
      dplyr::filter(.data, Species == "setosa")
    }
  )

  iris_mutable$mod_aplicar()

  expect_equal(
    as.character(unique(iris_mutable$tabla$Species)),
    "setosa"
  )

  iris_mutable$mod_add(
    id = "MOD2",
    priority = 2,
    .f = function(.data) {
      dplyr::mutate(.data, Species = "not setosa")
    }
  )

  iris_mutable$mod_aplicar()

  expect_equal(
    nrow(iris_mutable$tabla),
    0
  )

  iris_mutable$mod_rm("MOD1")

  iris_mutable$mod_aplicar()

  expect_equal(
    as.character(unique(iris_mutable$tabla$Species)),
    "not setosa"
  )

  iris_mutable$reset()

  expect_equal(
    iris_mutable$tabla,
    iris
  )

  iris_mutable$mod_reset()

  expect_equal(
    nrow(iris_mutable$mod_list),
    0
  )

})

