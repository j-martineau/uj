.n_th_errs <- function(x, n, n.scl) {
  errs <- c(f0(pop_vec(x)             , NULL, "\n \u2022 [x] is not a populated vector (?pop_vec)."),
            f0(!n.scl & cmp_psw_vec(n), NULL, "\n \u2022 [n] must be a complete positive whole-number vector (?cmp_psw_vec)."),
            f0( n.scl & cmp_psw_scl(n), NULL, "\n \u2022 [n] must be a complete positive whole-number scalar (?cmp_psw_scl)."))
  if (idef(errs)) {return(errs)}
  if (any(n > length(x))) {return(paste0("\n \u2022", f0(n.scl, "", "The largest value in")," [n] is greater than the number of elements in [x]."))}
  NULL
}

#' @family extensions
#' @title Extract elements by numeric position
#' @description \itemize{
#'   \item **`n_th`**: gets the `n`-th element(s) of a vector.
#'   \item **`first_n`**: gets the first `n`` elements of a vector.
#'   \item **`last_n`**: gets the last `n` elements of a vector.
#'   \item **`n_th_last`**: gets the `n`-th-from-last element(s) of a vector.
#' }
#' @param x A \link[=pop_vec]{populated vector} to extract elements from.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @return A scalar, vector, or \link[=ivls]{vlist}.
#' @export
#' @examples
#' first_n(letters, 5)
#' last_n(letters, 5)
#' n_th(letters, 5)
#' n_th(letters, )
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
