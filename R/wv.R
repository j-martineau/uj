#' @title Which \link[=av]{atomized} values are `TRUE`?
wv <- function(...) {
  x <- av(...)
  f0(ilgl(x), which(x), stop(.errs("[...] does not resolve to a logical object.")))
}
