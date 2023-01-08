#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Are objects comparable?
#' @description Determines whether modes of all `...` arguments are comparable (i.e., sortable and compatible with each other), meaning that the are either all character, all logical, all numeric, or all ordered factor with the same set of levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability with each other.
#' @param rec. A non-`NA` logical scalar indicating whether `...` arguments must be recyclable to be comparable.
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
comparable <- function(..., rec. = FALSE) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- base::c(uj::f0(n < 2          , "[...] must contain multiple arguments.", NULL),
                  uj::f0(!uj::isTF(rec.), "[rec.] must be TRUE or FALSE."         , NULL))
  if (!base::is.null(errs)) {stop(.uj::errs(errs))}
  if (rec.) {
    unq.ns <- base::unique(base::lengths(x))
    n.reps <- base::max(unq.ns) / unq.ns
    if (base::any(n.reps != base::round(n.reps))) {return(F)}
  }
  is.chr <- base::all(base::sapply(x, base::is.character))
  is.lgl <- base::all(base::sapply(x, base::is.logical))
  is.num <- base::all(base::sapply(x, base::is.numeric))
  is.ord <- base::all(base::sapply(x, base::is.ordered))
  if (is.chr | is.lgl | is.num) {return(T)}
  if (!is.ord) {return(F)}
  fac.levs <- base::lapply(x, levels)
  for (i in 2:n) {if (!base::identical(fac.levs[[i]], fac.levs[[i - 1]])) {return(F)}}
  T
}
