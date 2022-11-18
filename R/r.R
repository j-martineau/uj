.r.errs <- function(fun, ..., r = NULL, e = NULL, R = NULL) {
  c(f0(all(sapply(list(...), ivec))       , NULL, "\n \u2022 Arguments in [...] must be atomic vecs (?atm_vec)."),
    f0(fun == "e" , NULL, f0(cmp_psw_scl(r), NULL, "\n \u2022 [r] must be a positive whole-number scalar (?cmp_psw_scl).")),
    f0(fun == "r" , NULL, f0(cmp_psw_scl(e), NULL, "\n \u2022 [e] must be a positive whole-number scalar (?cmp_psw_scl).")))
}

#' @name r.
#' @family extended
#' @title Error-Checked Wraps of \code{\link[=base:rep]{base::rep}}.
#' @description \tabular{ll}{
#' \code{r}            \tab   \tab Atomizes \code{...} and, with the resulting
#'                                 vector \code{dots}, calls
#'                                 \code{base::rep.int(dots, r)}.            \cr
#' \code{e}            \tab   \tab Atomizes \code{...} and, with the resulting
#'                                 vector \code{dots}, calls
#'                                 \code{base::rep(dots, each = e)}.         \cr
#' \code{er}, \code{re}\tab   \tab Both functions atomize \code{...}, and, with
#'                                 the resulting vector, calls
#'                                 \code{base::rep(dots, times = r, each = e)}.}
#' \tabular{ll}{
#' NOTE \tab Although \code{er} and \code{re} produce identical results when
#'           their arguments are the same, both are included for convenience.  }
#' @section Comparing Function Output: The following console excerpt
#'   demonstrates how each function performs.
#'   ```
#'   > list(r  =  r(2,    0:1, 9),
#'   +      e  =  e(3,    0:1, 9),
#'   +      re = re(2, 3, 0:1, 9),
#'   +      er = er(3, 2, 0:1, 9))
#'   $r
#'   [1] 0 1 9 0 1 9
#'
#'   $e
#'   [1] 0 0 0 1 1 1 9 9 9
#'
#'   $re
#'    [1] 0 0 0 1 1 1 9 9 9 0 0 0 1 1 1 9 9 9
#'
#'   $er
#'    [1] 0 0 1 1 9 9 0 0 1 1 9 9 0 0 1 1 9 9
#'   ```
#' @param ... One or more atomic vectors to be replicated, reduced to unique
#'   values, or reduced to unique duplicated values.
#' @param r A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving
#'   the number of replications of entire vectors.
#' @param e A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving
#'   the number of times each value is to be replicated in place before
#'   replication of the entire vector.
#' @return An atomic vector.
#' @examples
#' r(2, 0:4, 5:9)
#' e(3, 0:4, 5:9)
#' re(2, 3, 0:4, 5:9)
#' er(3, 2, 0:4, 5:9)
#' @export
r. <- function() {help("r.", package = "uj")}

#' @rdname r.
#' @export
r <- function(r, ...) {
  errs <- .r.errs("r", ..., r = r)
  if (idef(errs)) {stop(errs)}
  rep.int(av(...), r)
}

#' @rdname r.
#' @export
e <- function(e, ...) {
  errs <- .r.errs("e", ..., e = e)
  if (idef(errs)) {stop(errs)}
  rep(av(...), each = e)
}

#' @rdname r.
#' @export
re <- function(r, e, ...) {
  errs <- .r.errs("re", ..., r = r, e = e)
  if (idef(errs)) {stop(errs)}
  rep(rep(av(...), times = r), each = e)
}

#' @rdname r.
#' @export
re <- function(e, r, ...) {
  errs <- .r.errs("er", ..., r = r, e = e)
  if (idef(errs)) {stop(errs)}
  rep(rep(av(...), times = r), each = e)
}
