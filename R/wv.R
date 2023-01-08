#' @encoding UTF-8
#' @family extensions
#' @family values
#' @title Which \link[=av]{atomized} values are `TRUE`?
#' @description Calls `which(av(x))`.
#' @return An integer vector.
#' @export
wv <- function(...) {
  x <- uj::av(...)
  uj::f0(uj::ilgl(x), base::which(x), stop(uj:::.errs("[...] does not resolve to a logical object.")))
}
