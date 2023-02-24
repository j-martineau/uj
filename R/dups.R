#' @encoding UTF-8
#' @family extensions
#' @title Duplicate values.
#' @description Extensions of \code{\link[base]{duplicated}}. Logically and numerically index duplicate values.
#' @details
#' \tabular{ll}{  `idups`   \tab Indexes all elements of `x` that are duplicates of its earlier element, either as a logical vector of the same length as `x` (when `int = FALSE`)
#'                               or as an integer vector whose length is the same as the number of elements in `x` that are duplicates of earlier elements (when `int = TRUE`).    \cr   \tab   \cr
#'                `dups`    \tab \link[=av]{Atomizes} `...`, then reduce the resulting vector to contain one copy of each duplicated element.                                                     }
#' @param ... One or more atomic vectors to be evaluated for duplicated values.
#' @param x An \link[=atm_vec]{atomic vec}.
#' @param int `TRUE` or `FALSE` indicating whether to return a logical vector (`int = FALSE`) or an integer vector (`int = TRUE`).
#' @return **A logical vector** (when `int = FALSE)` \cr\cr `idups`
#' \cr\cr  **An integer vector** (when `int = TRUE`) \cr\cr `idups`
#' \cr\cr  **An atomic vector**                      \cr\cr `dups`
#' @examples
#' dups(0:5, 5:10, 10:15, 15:20)
#' idups(c(0:5, 5:10, 10:15, 15:20))
#' idups(c(0:5, 5:10, 10:15, 15:20), int = T)
#' @export
dups <- function(...) {
  x <- uj::av(...)
  uj::U(x[base::duplicated(x)])
}

#' @rdname dups
#' @export
idups <- function(x, int = F) {
  uj::errs_if_nots(uj::atm_vec(x), "[x] must be an atomic vec (?atm_vec).", uj::isTF1(int), "[int] must be TRUE or FALSE.", PKG = "uj")
  uj::f0(int, uj::WV(base::duplicated(x)), base::duplicated(x))
}
