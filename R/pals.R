#' @name pals
#' @encoding UTF-8
#' @family plots
#' @title Plotting palettes
#' @description This family of functions uses `'dots'` to subsume plotting to `'points'`, `'shapes'`, and `'pch'`.
#' @details The functions in this family create palettes of various types from the first `n`  \eqn{^{(1)}} values of source vectors as defined below:
#' \tabular{lll}{  *Function*     \tab *Type*                    \tab *Source*    \cr
#'                 `pal_colors`   \tab colors                    \tab `v(colors)` \cr
#'                 `pal_lines`    \tab line types                \tab `v(lines)`  \cr
#'                 `pal_azdots`   \tab alpha dots                \tab `v(azdots)` \cr
#'                 `pal_AZdots`   \tab ALPHA dots                \tab `v(AZdots)` \cr
#'                 `pal_ndots`    \tab numeral dots              \tab `v(ndots)`  \cr
#'                 `pal_pdots`    \tab punctuation dots          \tab `v(pdots)`  \cr
#'                 `pal_ldots`    \tab line dots\eqn{^{(2)}}     \tab `v(ldots)`  \cr
#'                 `pal_edots`    \tab empty dots\eqn{^{(3)}}    \tab `v(edots)`  \cr
#'                 `pal_fdots`    \tab filled dots\eqn{^{(4)}}   \tab `v(fdots)`  \cr
#'                 `pal_sdots`    \tab solid dots                \tab `v(sdots)`    }
#'   \tabular{l}{  \eqn{^{(1)}} Recycled if `n > length(<source>)`.  \cr
#'                 \eqn{^{(2)}} Non-enclosing shape.                 \cr
#'                 \eqn{^{(3)}} Enclosing shape with empty interior. \cr
#'                 \eqn{^{(4)}} Enclosing shape with filled interior.  }
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of unique values to return.
#' @return **A positive whole number vector** \cr\cr `pal_edots, pal_fdots`  \cr `pal_ldots, pal_sdots`
#' \cr\cr  **A character vector**             \cr\cr `pal_colors, pal_lines` \cr `pal_azdots, pal_AZdots` \cr `pal_ndots, pal_pdots`
#' @export
pal_colors <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("colors", n)
}

#' @rdname pals
#' @export
pal_lines <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("lines", n)
}

#' @rdname pals
#' @export
pal_azdots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("azdots", n)
}

#' @rdname pals
#' @export
pal_AZdots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("AZdots", n)
}

#' @rdname pals
#' @export
pal_fdots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("fdots", n)
}

#' @rdname pals
#' @export
pal_edots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("edots", n)
}

#' @rdname pals
#' @export
pal_ldots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("ldots", n)
}

#' @rdname pals
#' @export
pal_ndots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("ndots", n)
}

#' @rdname pals
#' @export
pal_pdots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("pdots", n)
}

#' @rdname pals
#' @export
pal_sdots <- function(n) {
  uj:::.pal_errs(n, uj::callers())
  uj:::.pal_vals("edots", n)
}
