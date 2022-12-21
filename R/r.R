.r.errs <- function(fun, ..., r = NULL, e = NULL, R = NULL) {
  c(f0(all(sapply(list(...), ivec))        , NULL, "Arguments in [...] must be atomic vecs (?atm_vec)."),
    f0(fun == "e" , NULL, f0(cmp_psw_scl(r), NULL, "[r] must be a positive whole-number scalar (?cmp_psw_scl).")),
    f0(fun == "r" , NULL, f0(cmp_psw_scl(e), NULL, "[e] must be a positive whole-number scalar (?cmp_psw_scl).")))
}

#' @encoding UTF-8
#' @family extensions
#' @title Error-checked wrappers for `base::rep`
#' @description These functions \link[=av]{atomize} `...` arguments, converting them to an atomic vector (represented here as `dots`) before replication.
#' \tabular{rl}{
#'     `re, er`   \tab Calls `rep(dots, times = r, each = e)`.
#'   \cr    `e`   \tab Calls `rep.int(dots, each = e)`.
#'   \cr    `r`   \tab Calls `rep.int(dots, r)`.
#' }
#' @param ... One or more atomic vectors to be replicated (\link[=a]{atomized} before replication).
#' @param r A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of replications of entire vectors.
#' @param e A complete positive whole-number scalar giving the number of times each value is to be replicated in place.
#' @return An atomic vector.
#' @examples
#' r(2, 0:2, 3)
#' e(3, 0:2, 3)
#' re(2, 3, 0:2, 3)
#' @export
r <- function(r, ...) {
  errs <- .r.errs("r", ..., r = r)
  if (!is.null(errs)) {stop(.errs(errs))}
  rep.int(av(...), r)
}

#' @rdname r
#' @export
e <- function(e, ...) {
  errs <- .r.errs("e", ..., e = e)
  if (!is.null(errs)) {stop(.errs(errs))}
  rep(av(...), each = e)
}

#' @rdname r
#' @export
re <- function(r, e, ...) {
  errs <- .r.errs("re", ..., r = r, e = e)
  if (!is.null(errs)) {stop(.errs(errs))}
  rep(rep(av(...), times = r), each = e)
}

#' @rdname r
#' @export
er <- function(e, r, ...) {
  errs <- .r.errs("er", ..., r = r, e = e)
  if (!is.null(errs)) {stop(.errs(errs))}
  rep(rep(av(...), times = r), each = e)
}
