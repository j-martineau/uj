#' @name r.
#' @family extended
#' @title Extension of \code{base::rep}.
#' @param ... One or more atomic vectors to be replicated, reduced to unique
#'   values, or reduced to unique duplicated values.
#' @param r  giving the number of replications of entire vectors.
#' @param e \link[cmp_psw_scl]{Complete positive whole-number scalar} giving the
#'   number of times each value is to be replicated in place before replication
#'   of the entire vector.
#' @return An atomic vector.
#' @examples
#' r(2, 0:4, 5:9)
#' e(3, 0:4, 5:9)
#' re(2, 3, 0:4, 5:9)
#' er(3, 2, 0:4, 5:9)
#' @export
r. <- function() {help("r.", package = "uj")}

#' @describeIn r. Atomizes \code{...} and concatenates \code{r} copies of the
#'   resulting vector For example, \code{r(2, 0:1, 9)} returns \code{c(0, 1, 9,
#'   0, 1, 9)}.
#' @export
r <- function(r, ...) {
  errs <- c(f0(all(sapply(list(...), ivec)), NULL, "\n \u2022 Arguments in [...] must be atomic vecs (?ivec)."),
            f0(cmp_psw_scl(r)              , NULL, "\n \u2022 [r] must be a positive whole-number scalar (?cmp_psw_scl)."))
  if (idef(errs)) {stop(errs)}
  rep.int(av(...), r)
}

#' @describeIn r. Atomizes \code{...} and replaces each element of the resulting
#'   vector with \code{e.} copies of itself. For example, \code{e(3, 0:1, 9)}
#'   returns \code{c(0, 0, 0, 1, 1, 1, 9, 9, 9)}.
#' @export
e <- function(e, ...) {
  errs <- c(f0(all(sapply(list(...), ivec)), NULL, "\n \u2022 Arguments in [...] must be atomic vecs (?ivec)."),
            f0(cmp_psw_scl(e)              , NULL, "\n \u2022 [e] must be a positive whole-number scalar (?cmp_psw_scl)."))
  if (idef(errs)) {stop(errs)}
  rep(av(...), each = e)
}

#' @describeIn r. Atomizes \code{...}, creates \code{r} copies of the resulting
#'   vector, then replaces each element of that resulting vector with \code{e.}
#'   copies of itself. For example, \code{re(2, 3, 0:1, 9)} returns \code{c(0,
#'   0, 0, 1, 1, 1, 9, 9, 9, 0, 0, 0, 1, 1, 1, 9, 9, 9)}. NOTE: There is no
#'   \strong{\code{er}} function to reverse the order because it produces the
#'   same result.
#' @export
re <- function(r, e, ...) {
  errs <- c(f0(all(sapply(list(...), ivec)), NULL, "\n \u2022 Arguments in [...] must be atomic vecs (?ivec)."),
            f0(cmp_psw_scl(r)              , NULL, "\n \u2022 [r] must be a positive whole-number scalar (?cmp_psw_scl)."),
            f0(cmp_psw_scl(e)              , NULL, "\n \u2022 [e] must be a positive whole-number scalar (?cmp_psw_scl)."))
  if (idef(errs)) {stop(errs)}
  rep(av(...), times = r, each = e)
}
