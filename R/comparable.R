#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Are objects comparable?
#' @description Determines whether modes of all `...` arguments are comparable (i.e., sortable and compatible with each other), meaning that the are either all character, all logical, all numeric, or all ordered factor with the same set of levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability with each other.
#' @param rec. `TRUE` or `FALSE` indicating whether `...` arguments must be recyclable to be comparable.
#' @return A logical scalar.
#' @examples
#' az. <- letters
#' comparable(1:10, pi, exp(1))
#' comparable(az., az., "", "!")
#' comparable(az., factor(az.))
#' comparable(1:10, 1:3, rec. = TRUE)
#' comparable(list(az., 1:10), az., 1:10)
#' comparable(factor(az., az., ordered = T), factor("q", az., ordered = T))
#' @export
comparable <- function(..., REC = FALSE) {
  x <- base::list(...)
  n <- uj::N(x)
  uj::errs_if_nots(n >= 2        , "[...] must contain multiple arguments.",
                   uj::isTF1(REC), "[REC] must be TRUE or FALSE."          , PKG = "uj")
  if (REC) {
    unq.ns <- uj::U(uj::NS(x))
    n.reps <- base::max(unq.ns) / unq.ns
    if (base::any(!uj::rounded(n.reps))) {return(F)}
  }
  is.chr <- base::all(base::sapply(x, uj::isCHR))
  is.lgl <- base::all(base::sapply(x, uj::isLGL))
  is.num <- base::all(base::sapply(x, uj::isNUM))
  is.ord <- base::all(base::sapply(x, uj::isORD))
  if (!is.chr & !is.lgl & !is.num) {
    if (is.ord) {
      fac.levs <- base::lapply(x, levels)
      for (i in 2:n) {if (uj::isDIF1(fac.levs[[i]], fac.levs[[i - 1]])) {return(F)}}
      T
    } else {F}
  } else {T}
}
