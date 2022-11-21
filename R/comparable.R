#' @family props
#' @title Are objects comparable?
#' @description Determines whether modes of all arguments in \code{...} are
#'   comparable (i.e., sortable and compatible with each other), meaning that
#'   the are either all character, all logical, all numeric, or all ordered
#'   factor with the same set of levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability
#'   with each other.
#' @param recycle. \link[=cmp_lgl_scl]{Complete logical scalar} indicating
#'   whether arguments in \code{...} must be recyclable to be comparable.
#' @return A logical scalar.
#' @export
comparable <- function(..., recycle. = T) {
  x <- list(...)
  n <- length(x)
  errs <- c(f0(n < 2          , "\n \u2022 [...] must contain multiple arguments.", NULL),
            f0(!isTF(recycle.), "\n \u2022 [recycle.] must be TRUE or FALSE."     , NULL))
  if (idef(errs)) {stop(errs)}
  if (recycle.) {
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
  fac.levs <- sapply(x, levels)
  for (i in 2:n) {if (!identical(fac.levs[[i]], fac.levs[[i - 1]])) {return(F)}}
  T
}
