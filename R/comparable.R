#' @name comparable.
#' @family props
#' @title Are objects comparable?
#' @param ... An arbitrary number of arguments to be checked for comparability
#'   with each other.
#' @param recycle. \link[cmp_lgl_scl]{Complete logical scalar} indicating
#'   whether arguments in \code{...} must be recyclable to be comparable.
#' @return A logical scalar.
#' @export
comparable. <- function() {help("comparable.")}

#' @describeIn comparable. Determines whether modes of all arguments in
#'   \code{...} are comparable (i.e., sortable and compatible with each other),
#'   meaning that all are character, logical, numeric, or ordered factor with
#'   the same set of levels (in the same order).
comparable <- function(..., recycle. = T) {
  x <- list(...)
  n <- length(x.)
  err <- NULL
  if (n < 2) {err <- c(err, "\n  * [...] must contain multiple arguments.")}
  if (!isTF(recycle.)) {err <- c(err, "\n  * [recycle.] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  if (recycle.) {
    nrep <- unique(lengths(x.))
    nrep <- max(nrep) / nrep
    if (any(nrep != round(nrep))) {return(F)}
  }
  chr <- all(sapply(x., is.character))
  lgl <- all(sapply(x., is.logical))
  num <- all(sapply(x., is.numeric))
  ord <- all(sapply(x., is.ordered))
  if (chr | lgl | num) {return(T)}
  if (!ord) {return(F)}
  levs <- sapply(x, levels)
  for (i in 2:n) {if (!identical(levs[[i]], levs[[i - 1]])) {return(F)}}
  T
}
