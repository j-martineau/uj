#' @encoding UTF-8
#' @family extensions
#' @title Duplicate values.
#' @description Extensions of \code{\link[base]{duplicated}}. Logically and numerically index duplicate values.
#' @details
#' \tabular{ll}{  `idups`   \tab Indexes all elements of `x` that are duplicates of its earlier element, either as a logical vector of the same length as `x` (when `.INT = FALSE`)
#'                               or as an integer vector whose length is the same as the number of elements in `x` that are duplicates of earlier elements (when `.INT = TRUE`).    \cr   \tab   \cr
#'                `dups`    \tab \link[=av]{Atomizes} `...`, then reduce the resulting vector to contain one copy of each duplicated element.                                                     }
#' @param ... One or more atomic vectors to be evaluated for duplicated values.
#' @param x An \link[=atm_vec]{atomic vec}.
#' @param .INT `TRUE` or `FALSE` indicating whether to return a logical vector (`.INT = FALSE`) or an integer vector (`.INT = TRUE`).
#' @return **A logical vector** (when `.INT = FALSE)` \cr\cr `idups`
#' \cr\cr  **An integer vector** (when `.INT = TRUE`) \cr\cr `idups`
#' \cr\cr  **An atomic vector**                      \cr\cr `dups`
#' @examples
#' dups(0:5, 5:10, 10:15, 15:20)
#' idups(c(0:5, 5:10, 10:15, 15:20))
#' idups(c(0:5, 5:10, 10:15, 15:20), .INT = T)
#' @export
dups <- function(...) {
  x <- uj::av(...)
  base::unique(x[base::duplicated(x)])
}

#' @rdname dups
#' @export
idups <- function(x, .INT = F) {
  Errors <- NULL
  if (!uj:::.atm_vec(x)) {Errors <- base::c(Errors, "[x] must be an atomic vec (?atm_vec).")}
  if (!uj:::.cmp_lgl_scl(.INT)) {Errors <- base::c(Errors, "[.INT] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  uj::f0(.INT, base::which(base::duplicated(x)), base::duplicated(x))
}
