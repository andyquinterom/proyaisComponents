#' @title addPreserveSearch
#' @description Permite a selectizeInput mantener la busqueda
#' @param x selectizeInput to add preserve search to
#' @export
addPreserveSearch <- function(x) {
  preserve_search <- htmltools::htmlDependency(
    name = "preserve_search",
    version = "1.0",
    src = system.file("deps", package = "proyaisComponents"),
    script = "preserve_search-1.0.js"
  )
  htmltools::attachDependencies(
    x,
    c(htmltools::htmlDependencies(x), list(preserve_search))
  )
}
