#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Are objects comparable?
#' @description Determines whether modes of all `...` arguments are comparable (i.e., sortable and compatible with each other), meaning that the are either all character, all logical, all numeric, or all ordered factor with the same set of levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability with each other.
#' @param rec. `TRUE` or `FALSE` indicating whether `...` arguments must be recyclable to be comparable.
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
comparable <- function(..., REC = FALSE) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- NULL
  if (n < 2) {errs <- base::c(errs, "[...] must contain multiple arguments.")}
  if (!uj:::.cmp_lgl_scl(REC)) {errs <- base::c(errs, "[REC] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (REC) {
    unq.ns <- base::unique(base::lengths(x))
    n.reps <- base::max(unq.ns) / unq.ns
    if (base::any(n.reps != base::round(n.reps))) {return(F)}
  }
  is.chr <- base::all(base::sapply(x, base::is.character(x)))
  is.lgl <- base::all(base::sapply(x, base::is.logical(x)))
  is.num <- base::all(base::sapply(x, base::is.numeric(x)))
  is.ord <- base::all(base::sapply(x, base::is.ordered(x)))
  if (!is.chr & !is.lgl & !is.num) {
    if (is.ord) {
      fac.levs <- base::lapply(x, base::levels)
      for (i in 2:n) {
        levs.curr <- fac.levs[[i]]
        levs.prev <- fac.levs[[i - 1]]
        if (base::length(levs.curr) != base::length(levs.prev)) {return(F)}
        else if (!base::all(levs.curr == levs.prev)) {return(F)}
      }
      T
    } else {F}
  } else {T}
}

#' @rdname comparable
#' @export
comparable_xy <- function(x, y, REC = FALSE) {uj::comparable(x, y, REC = REC)}
