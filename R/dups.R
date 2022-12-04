#' @title Duplicate values.
#' @description Extended functionality for \code{\link[base]{duplicated}}.
#' \itemize{
#'   \item **`dups`**: \link[=av]{atomizes} `...`, then reduce the resulting vector to contain one copy of each duplicated element.
#'   \item **`idups`**: indexes all elements of `x` that are duplicates of its earlier element, either as a logical vector of the same length as `x` (when `int = FALSE`) or as an integer vector whose length is the same as the number of elements in `x` that are duplicates of earlier elements (when `int = TRUE`).
#' }
#' @param ... One or more atomic vectors to be evaluated for duplicated values.
#' @param x An \link[=atm_vec]{atomic vec}.
#' @param int A non-`NA` logical scalar indicating whether to return a logical vector (`int = FALSE`) or an integer vector (`int = TRUE`).
#' @return \itemize{
#'   \item **`dups`**: an atomic vector.
#'   \item **`idups`**: a logical vector (when `int = FALSE`) or an integer vector (when `int = TRUE`).
#' }
#' @examples
#' dups(0:5, 5:10, 10:15, 15:20)
#' idups(c(0:5, 5:10, 10:15, 15:20))
#' idups(c(0:5, 5:10, 10:15, 15:20), int = T)
#' @export
dups <- function(...) {x <- av(...); unique(x[duplicated(x)])}

#' @rdname dups
#' @export
idups <- function(x, int = F) {
  errs <- c(f0(atm_vec(x), NULL, "\n \u2022 [x] must be an atomic vec (?atm_vec)."),
            f0(isTF(int), NULL, "\n \u2022 [int] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  f0(int, which(duplicated(x)), duplicated(x))
}
