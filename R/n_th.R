.n_th_errs <- function(x, n, n.scl) {
  errs <- c(f0(pop_vec(x)             , NULL, "\n \u2022 [x] is not a populated vector (?pop_vec)."),
            f0(!n.scl & cmp_psw_vec(n), NULL, "\n \u2022 [n] must be a complete positive whole-number vector (?cmp_psw_vec)."),
            f0( n.scl & cmp_psw_scl(n), NULL, "\n \u2022 [n] must be a complete positive whole-number scalar (?cmp_psw_scl)."))
  if (idef(errs)) {return(errs)}
  if (any(n > length(x))) {return(paste0("\n \u2022", f0(n.scl, "", "The largest value in")," [n] is greater than the number of elements in [x]."))}
  NULL
}

#' @name n_th
#' @family extensions
#' @title Extract Elements by Index Position
#' @description \tabular{ll}{
#'   FUNCTION      \tab WHAT IT DOES                                         \cr
#'   `n_th`        \tab Gets the `n`-th element(s) of a vector.              \cr
#'   `first_n`     \tab Gets the first `n`` elements of a vector.            \cr
#'   `last_n`      \tab Gets the last `n` elements of a vector.              \cr
#'   `n_th_last`   \tab Gets the `n`-th from last element(s) of a vector.      }
#' @param x A \link[=pop_vec]{populated vector} to extract elements from.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @return A vector or \link[=ivls]{vlist} of length `1+`.
#' @examples
#' first_n(letters, 5)
#' last_n(letters, 5)
#' n_th(letters, 5)
#' n_th(letters, )
#' @export
n_th <- function(x, n) {
  errs <- .n_th_errs(x, n, F)
  if (!is.null(errs)) {stop(errs)}
  x[n]
}

#' @rdname n_th
#' @export
first_n <- function(x, n) {
  errs <- .n_th_errs(x, n, T)
  if (!is.null(errs)) {stop(errs)}
  x[1:n]
}

#' @rdname n_th
#' @export
last_n <- function(x, n) {
  errs <- .n_th_errs(x, n, F)
  if (!is.null(errs)) {stop(errs)}
  n <- 1:n; x[1 + length(x) - n]
}

#' @rdname n_th
#' @export
n_th_last <- function(x, n) {
  errs <- .n_th_errs(x, n, T)
  if (!is.null(errs)) {stop(errs)}
  x[length(x) - n + 1]
}
