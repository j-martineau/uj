# internals

.r_errs <- function(fun, stack, ..., r = NULL, e = NULL) {
  errs <- NULL
  if (!base::all(base::sapply(base::list(...), uj::.atm_vec))) {errs <- base::c(errs, "Arguments in [...] must be atomic vecs (?atm_vec).")}
  if (!base::ifelse(base::is.null(r), T, uj::.cmp_psw_scl(r))) {errs <- base::c(errs, "[r] must be a positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::ifelse(base::is.null(e), T, uj::.cmp_psw_scl(e))) {errs <- base::c(errs, "[e] must be a positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, fun = fun, stack = stack)}
}

# exported

#' @encoding UTF-8
#' @family extensions
#' @title Error-checked wrappers for `base::rep`
#' @description These functions \link[=av]{atomize} `...` arguments, converting them to an atomic vector (represented here as `dots`) before replication.
#' @details
#' \tabular{ll}{  `re, er`   \tab Calls `rep(dots, times = R, each = E)`. \cr   \tab   \cr
#'                `e`        \tab Calls `rep.int(dots, each = E)`.        \cr   \tab   \cr
#'                `r`        \tab Calls `rep.int(dots, R)`.                              }
#' @param ... One or more objects containing atomic values to be replicated (\link[=a]{atomized} before replication).
#' @param r A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of replications of entire vectors.
#' @param e A complete positive whole-number scalar giving the number of times each value is to be replicated in place.
#' @return An atomic vector.
#' @examples
#' r(2, 0:2, 3)
#' e(3, 0:2, 3)
#' re(2, 3, 0:2, 3)
#' @export
r <- function(r, ...) {
  uj:::.r_errs("r", uj::callers(), ..., r = r)
  base::rep.int(uj::av(...), r)
}

#' @rdname r
#' @export
e <- function(e, ...) {
  uj:::.r_errs("e", uj::callers(), ..., e = e)
  base::rep(uj::av(...), each = e)
}

#' @rdname r
#' @export
re <- function(r, e, ...) {
  uj:::.r_errs("re", uj::callers(), ..., r = r, e = e)
  base::rep(base::rep(uj::av(...), times = r), each = e)
}

#' @rdname r
#' @export
er <- function(e, r, ...) {
  uj:::.r_errs("er", uj::callers(), ..., r = r, e = e)
  base::rep(base::rep(uj::av(...), times = r), each = e)
}
