#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Are objects comparable?
#' @description Determines whether modes of all `...` arguments are comparable (i.e., sortable and compatible with each other), meaning that the are either all character, all logical, all numeric, or all ordered factor with the same set of levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability with each other.
#' @param .REC `TRUE` or `FALSE` indicating whether `...` arguments must be recyclable to be comparable.
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
comparable <- function(..., .REC = FALSE) {
  x <- base::list(...)
  N <- base::length(x)
  Errors <- NULL
  if (N < 2) {Errors <- base::c(Errors, "[...] must contain multiple arguments.")}
  if (!uj:::.cmp_lgl_scl(.REC)) {Errors <- base::c(Errors, "[.REC] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (.REC) {
    UnqNs <- base::unique(base::lengths(x))
    nReps <- base::max(UnqNs) / UnqNs
    if (base::any(nReps != base::round(nReps))) {return(F)}
  }
  Chr <- base::all(base::sapply(x, base::is.character(x)))
  Lgl <- base::all(base::sapply(x, base::is.logical(x)))
  Num <- base::all(base::sapply(x, base::is.numeric(x)))
  Ord <- base::all(base::sapply(x, base::is.ordered(x)))
  if (!Chr & !Lgl & !Num) {
    if (Ord) {
      FacLevs <- base::lapply(x, base::levels)
      for (i in 2:N) {
        CurrLevs <- FacLevs[[i]]
        PrevLevs <- FacLevs[[i - 1]]
        if (base::length(CurrLevs) != base::length(PrevLevs)) {return(F)}
        else if (!base::all(CurrLevs == PrevLevs)) {return(F)}
      }
      T
    } else {F}
  } else {T}
}

#' @rdname comparable
#' @export
comparable_xy <- function(x, y, .REC = FALSE) {uj::comparable(x, y, .REC = .REC)}
