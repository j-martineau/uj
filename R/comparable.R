#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Are objects comparable?
#' @description Determines whether modes of all `...` arguments are comparable (i.e., sortable and compatible with each other), meaning that the are either all character, all logical, all numeric, or all ordered factor with the same set of levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability with each other.
#' @param .rec `TRUE` or `FALSE` indicating whether `...` arguments must be recyclable to be comparable.
#' @param x,y Arguments to be checked for comparability.
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
comparable <- function(..., .rec = FALSE) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- NULL
  if (n < 2) {errs <- base::c(errs, "[...] must contain multiple arguments.")}
  if (!ppp::.cmp_lgl_scl(.rec)) {errs <- base::c(errs, "[.rec] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  if (.rec) {
    unqNs <- base::unique(base::lengths(x))
    nReps <- base::max(unqNs) / unqNs
    if (base::any(nReps != base::round(nReps))) {return(F)}
  }
  chr <- base::all(base::sapply(x, base::is.character(x)))
  lgl <- base::all(base::sapply(x, base::is.logical(x)))
  num <- base::all(base::sapply(x, base::is.numeric(x)))
  ord <- base::all(base::sapply(x, base::is.ordered(x)))
  if (!chr & !lgl & !num) {
    if (ord) {
      facLevs <- base::lapply(x, base::levels)
      for (i in 2:n) {
        currLevs <- facLevs[[i]]
        prevLevs <- facLevs[[i - 1]]
        if (base::length(currLevs) != base::length(prevLevs)) {return(F)}
        else if (!base::all(currLevs == prevLevs)) {return(F)}
      }
      T
    } else {F}
  } else {T}
}

#' @rdname comparable
#' @export
comparable_xy <- function(x, y, .rec = FALSE) {uj::comparable(x, y, .rec = .rec)}
