#' @name reps
#' @family values
#' @title Extended functionality for \code{\link[base:rep]{base::rep}}.
#' @description \code{\link[=a]{atomize}} \code{...} and concatenate \code{r}
#'   copies of the resulting vector. For example, \code{reps(0:1, 9, r = 2)}
#'   gives the result \code{c(0, 1, 9, 0, 1, 9)}.
#' @param ... One or more atomic vectors to be replicated, reduced to unique
#'   values, or reduced to unique duplicated values.
#' @param r. An integer scalar giving the number of replications of entire
#'   vectors.
#' @param e. An integer scalar giving the number of times each value is to be
#'   replicated in place before replication of the entire vector.
#' @return An atomic vector.
#' @examples
#' reps(0:4, 5:9, r. = 2)
#' each(0:4, 5:9, r. = 2)
#' reps_each(0:4, 5:9, r. = 2, e. = 2)
#' each_reps(0:4, 5:9, e. = 2, r. = 2)
#' @export
reps <- function(..., r. = 1) {
  vd. <- all(sapply(list(...), ivec))
  vr. <- cmp_psw_scl(r.)
  err. <- NULL
  if (!vd.) {err. <- c(err., "\n • Arguments in [...] must be atomic vectors.")}
  if (!vr.) {err. <- c(err., "\n • [r.] must be a positive, whole-number scalar.")}
  if (idef(err.)) {stop(err.)}
  rep.int(av(...), r.)
}

#' @describeIn reps \code{\link[=a.]{atomize}} \code{...} and create \code{e}
#'   copies of each element of the resulting vector in place. For example,
#'   \code{each(0:1, 9, e = 2)} gives the result \code{c(0, 0, 1, 1, 9, 9)}.
#' @export
each <- function(..., e. = 1) {
  vd. <- all(sapply(list(...), ivec))
  ve. <- cmp_psw_scl(e.)
  err. <- NULL
  if (!vd.) {err. <- c(err., "\n • Arguments in [...] must be atomic vectors.")}
  if (!ve.) {err. <- c(err., "\n • [e.] must be a positive, whole-number scalar.")}
  if (idef(err.)) {stop(err.)}
  rep(av(...), each = e.)
}

#' @describeIn reps Call \code{reps} then \code{each}.
#' @export
reps_each <- function(..., r. = 1, e. = 1) {
  vd. <- all(sapply(list(...), ivec))
  ve. <- cmp_psw_scl(e.)
  vr. <- cmp_psw_scl(r.)
  err. <- NULL
  if (!vd.) {err. <- c(err., "\n • Arguments in [...] must be atomic vectors.")}
  if (!ve.) {err. <- c(err., "\n • [e.] must be a positive, whole-number scalar.")}
  if (!vr.) {err. <- c(err., "\n • [r.] must be a positive, whole-number scalar.")}
  if (idef(err.)) {stop(err.)}
  rep(rep.int(av(...), r.), each = e.)
}

#' @describeIn reps Call \code{each} then \code{reps}.
#' @export
each_reps <- function(..., e. = 1, r. = 1) {
  vd. <- all(sapply(list(...), ivec))
  ve. <- cmp_psw_scl(e.)
  vr. <- cmp_psw_scl(r.)
  err. <- NULL
  if (!vd.) {err. <- c(err., "\n • Arguments in [...] must be atomic vectors.")}
  if (!ve.) {err. <- c(err., "\n • [e.] must be a positive, whole-number scalar.")}
  if (!vr.) {err. <- c(err., "\n • [r.] must be a positive, whole-number scalar.")}
  if (idef(err.)) {stop(err.)}
  rep.int(rep(av(...), each = e.), r.)
}
