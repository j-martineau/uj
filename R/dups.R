#' @encoding UTF-8
#' @family extensions
#' @title Duplicate values.
#' @description Extensions of \code{\link[base]{duplicated}}. Logically and numerically index duplicate values.
#' @details
#' \tabular{ll}{  `idups`   \tab Indexes all elements of `X` that are duplicates of its earlier element, either as a logical vector of the same length as `X` (when `Int = FALSE`)
#'                               or as an integer vector whose length is the same as the number of elements in `X` that are duplicates of earlier elements (when `Int = TRUE`).    \cr   \tab   \cr
#'                `dups`    \tab \link[=av]{Atomizes} `...`, then reduce the resulting vector to contain one copy of each duplicated element.                                                     }
#' @param ... One or more atomic vectors to be evaluated for duplicated values.
#' @param X An \link[=atm_vec]{atomic vec}.
#' @param Int `TRUE` or `FALSE` indicating whether to return a logical vector (`Int = FALSE`) or an integer vector (`Int = TRUE`).
#' @return **A logical vector** (when `Int = FALSE)` \cr\cr `idups`
#' \cr\cr  **An integer vector** (when `Int = TRUE`) \cr\cr `idups`
#' \cr\cr  **An atomic vector**                      \cr\cr `dups`
#' @examples
#' dups(0:5, 5:10, 10:15, 15:20)
#' idups(c(0:5, 5:10, 10:15, 15:20))
#' idups(c(0:5, 5:10, 10:15, 15:20), Int = T)
#' @export
dups <- function(...) {
  X <- uj::av(...)
  base::unique(X[base::duplicated(X)])
}

#' @rdname dups
#' @export
idups <- function(X, Int = F) {
  Errors <- NULL
  if (!uj:::.atm_vec(X)) {Errors <- base::c(Errors, "[X] must be an atomic vec (?atm_vec).")}
  if (!uj:::.cmp_lgl_scl(Int)) {Errors <- base::c(Errors, "[Int] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  uj::f0(Int, base::which(base::duplicated(X)), base::duplicated(X))
}
