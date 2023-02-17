#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Does an atomic object only contain unique values?
#' @description Evaluates whether `x` contains only only unique atomic values, with options for for atomizing before evaluation and for handling `NA` values.
#' @param x An atomic object.
#' @param a `TRUE` or `FALSE` indicating whether to reduce `x` to an atomic vector containing all of its atomic values. When `FALSE` and `x` is not atomic, throws an error.
#' @param na `TRUE` or `FALSE` indicating whether `NA` values are allowed.
#' @return Scalar `TRUE` or scalar `FALSE`.
#' @examples
#' is_unique(letters)
#' is_unq(sample(letters, 27, replace = T))
#' @export
is_unique <- function(x, a = T, nas = F) {
  uDTF <- function(D) {uj::NR(uj::UV(D)) == uj::NR(D)}
  uGEN <- function(G) {uj::NU(G) == uj::N(G)}
  uj::errs_if_nots(uj::isTF1(a ) , "[a] must be TRUE or FALSE."  ,
                   uj::isTF1(nas), "[nas] must be TRUE or FALSE.", PKG = "uj")
  x.av <- uj::av(x)
  if (a) {x <- x.av}
  if (uj::N1P(x.av)) {
    uj::err_if_not(uj::f0(nas, T, uj::anyNAS(x.av)), "[na = FALSE] but [x] contains NA values.", PKG = "uj")
    uj::f0(uj::isDTF(x), uDTF(x), uj::f0(uj::isLST(x), uGEN(x), uGEN(x.av)))
  } else {F}
}

#' @rdname is_unique
#' @export
is_unq <- is_unique

#' @rdname is_unique
#' @export
isUNQ <- is_unique
