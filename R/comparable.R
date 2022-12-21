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
  x <- list(...)
  n <- length(x)
  errs <- c(f0(n < 2      , "[...] must contain multiple arguments.", NULL),
            f0(!isTF(rec.), "[rec.] must be TRUE or FALSE."         , NULL))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (rec.) {
    unq.ns <- unique(lengths(x))
    n.reps <- max(unq.ns) / unq.ns
    if (any(n.reps != round(n.reps))) {return(F)}
  }
  is.chr <- all(sapply(x, is.character))
  is.lgl <- all(sapply(x, is.logical))
  is.num <- all(sapply(x, is.numeric))
  is.ord <- all(sapply(x, is.ordered))
  if (is.chr | is.lgl | is.num) {return(T)}
  if (!is.ord) {return(F)}
  fac.levs <- lapply(x, levels)
  for (i in 2:n) {if (!identical(fac.levs[[i]], fac.levs[[i - 1]])) {return(F)}}
  T
}
