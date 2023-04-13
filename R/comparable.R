#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Are objects comparable?
#' @description Determines whether modes of all `...` arguments are comparable (i.e., sortable and compatible with each other), meaning that the are either all character, all logical, all numeric, or all ordered factor with the same set of levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability with each other.
#' @param rec. `TRUE` or `FALSE` indicating whether `...` arguments must be recyclable to be comparable.
#' @param X,y Arguments to be checked for comparability.
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
  X <- base::list(...)
  N <- base::length(X)
  Errors <- NULL
  if (N < 2) {Errors <- base::c(Errors, "[...] must contain multiple arguments.")}
  if (!uj:::.cmp_lgl_scl(REC)) {Errors <- base::c(Errors, "[REC] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (REC) {
    UnqNs <- base::unique(base::lengths(X))
    nReps <- base::max(UnqNs) / UnqNs
    if (base::any(nReps != base::round(nReps))) {return(F)}
  }
  Chr <- base::all(base::sapply(X, base::is.character(X)))
  Lgl <- base::all(base::sapply(X, base::is.logical(X)))
  Num <- base::all(base::sapply(X, base::is.numeric(X)))
  Ord <- base::all(base::sapply(X, base::is.ordered(X)))
  if (!Chr & !Lgl & !Num) {
    if (Ord) {
      FacLevs <- base::lapply(X, base::levels)
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
comparable_xy <- function(X, y, REC = FALSE) {uj::comparable(X, y, REC = REC)}
