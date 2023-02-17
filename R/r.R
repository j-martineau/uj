.r_errs <- function(fun, ..., r = NULL, e = NULL) {
  uj::errs_if_nots(base::all(base::sapply(base::list(...), uj::atm_vec)), "Arguments in [...] must be atomic vecs (?atm_vec)."        ,
                   uj::nllOR(r, "cmp_psw_scl")                          , "[r] must be a positive whole-number scalar (?cmp_psw_scl).",
                   uj::nllOR(e ,"cmp_psw_scl")                          , "[e] must be a positive whole-number scalar (?cmp_psw_scl).", PKG = "uj")
}

#' @encoding UTF-8
#' @family extensions
#' @title Error-checked wrappers for `base::rep`
#' @description These functions \link[=av]{atomize} `...` arguments, converting them to an atomic vector (represented here as `dots`) before replication.
#' @details
#' \tabular{ll}{  `re, er`   \tab Calls `rep(dots, times = r, each = e)`. \cr   \tab  }
#' \tabular{ll}{  `e`        \tab Calls `rep.int(dots, each = e)`.        \cr
#'                `r`        \tab Calls `rep.int(dots, r)`.                           }
#' @param ... One or more objects containin aatomic values to be replicated (\link[=a]{atomized} before replication).
#' @param r A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of replications of entire vectors.
#' @param e A complete positive whole-number scalar giving the number of times each value is to be replicated in place.
#' @return An atomic vector.
#' @examples
#' r(2, 0:2, 3)
#' e(3, 0:2, 3)
#' re(2, 3, 0:2, 3)
#' @export
r <- function(r, ...) {
  uj:::.r_errs("r", ..., r = r)
  base::rep.int(uj::av(...), r)
}

#' @rdname r
#' @export
e <- function(e, ...) {
  uj:::.r_errs("e", ..., e = e)
  base::rep(uj::av(...), each = e)
}

#' @rdname r
#' @export
re <- function(r, e, ...) {
  uj:::.r_errs("re", ..., r = r, e = e)
  base::rep(base::rep(uj::av(...), times = r), each = e)
}

#' @rdname r
#' @export
er <- function(e, r, ...) {
  uj:::.r_errs("er", ..., r = r, e = e)
  base::rep(base::rep(uj::av(...), times = r), each = e)
}
