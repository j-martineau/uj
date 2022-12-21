#' @encoding UTF-8
#' @family extensions
#' @family values
#' @title Which \link[=av]{atomized} values are `TRUE`?
#' @description Calls `which(av(x))`.
#' @export
wv <- function(...) {
  x <- av(...)
  f0(ilgl(x), which(x), stop(.errs("[...] does not resolve to a logical object.")))
}
