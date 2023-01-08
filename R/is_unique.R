#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Evaluate whether an object has only unique atomic values
#' @description Evaluates whether `x` contains only only unique atomic values, with options for for atomizing before evaluation and for handling `NA` values.
#' @param x An atomic object.
#' @param a A non-`NA` logical scalar indicating whether to reduce `x` to an atomic vector containing all of its atomic values. When `FALSE` and `x` is not atomic, throws an error.
#' @param na A non-`NA` logical scalar indicating whether `NA` values are allowed.
#' @return Scalar `TRUE` or scalar `FALSE`.
#' @examples
#' is_unq(letters)
#' is_unq(sample(letters, 27, replace = T))
#' @export
is_unique <- function(x, a = T, na = F) {
  udtf <- function(D) {base::nrow(base::unique(D) == base::nrow(D))}
  ugen <- function(G) {base::length(base::unique(G)) == base::length(G)}
  errs <- base::c(uj::f0(isTRUE(a ) | isFALSE(a ), NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(isTRUE(na) | isFALSE(na), NULL, "[na] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x.av <- base::unlist(x, T, F)
  if (a) {x <- x.av}
  if (base::length(x.av) > 0) {
    if (na) {if (base::any(base::is.na(x.av))) {stop(uj:::.errs("[na = FALSE] but [x] contains NA values."))}}
    uj::f0(base::is.data.frame(x), udtf(x), uj::f0(base::is.list(x), ugen(x), ugen(x.av)))
  } else {F}
}
