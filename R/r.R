#' @encoding UTF-8
#' @family extensions
#' @title Error-checked wrappers for `base::rep`
#' @description These functions \link[=av]{atomize} `...` arguments, converting them to an atomic vector (represented here as `dots`) before replication.
#' @details
#' \tabular{ll}{  `re, er`   \tab Calls `rep(dots, times = R, each = E)`. \cr   \tab   \cr
#'                `e`        \tab Calls `rep.int(dots, each = E)`.        \cr   \tab   \cr
#'                `r`        \tab Calls `rep.int(dots, R)`.                              }
#' @param ... One or more objects containing atomic values to be replicated (\link[=a]{atomized} before replication).
#' @param R A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of replications of entire vectors.
#' @param E A complete positive whole-number scalar giving the number of times each value is to be replicated in place.
#' @return An atomic vector.
#' @examples
#' r(2, 0:2, 3)
#' e(3, 0:2, 3)
#' re(2, 3, 0:2, 3)
#' @export
r <- function(R, ...) {
  uj:::.r_errs("r", uj::callers(), ..., R = R)
  base::rep.int(uj::av(...), R)
}

#' @rdname r
#' @export
e <- function(E, ...) {
  uj:::.r_errs("e", uj::callers(), ..., E = E)
  base::rep(uj::av(...), each = E)
}

#' @rdname r
#' @export
re <- function(R, E, ...) {
  uj:::.r_errs("re", uj::callers(), ..., R = R, E = E)
  base::rep(base::rep(uj::av(...), times = R), each = E)
}

#' @rdname r
#' @export
er <- function(E, R, ...) {
  uj:::.r_errs("er", uj::callers(), ..., R = R, E = E)
  base::rep(base::rep(uj::av(...), times = R), each = E)
}
