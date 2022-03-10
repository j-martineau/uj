#' @name reps
#' @family values
#' @title Replication
#' @description Extended functionality for \code{\link[base:rep]{base::rep}}.
#' @details \strong{\code{reps}}
#'   \cr \code{\link[=a]{atomizes}} \code{...} and concatenates \code{r} copies
#'   of the resulting vector. For example, \code{reps(0:1, 9, r = 2)} gives the
#'   result \code{c(0, 1, 9, 0, 1, 9)}.
#'   \cr\cr
#'   \strong{\code{each}}
#'   \cr \code{\link[=a.]{atomizes}} \code{...} and creates \code{e} copies of
#'   each element of the resulting vector in place. For example, \code{each(0:1,
#'   9, e = 2)} gives the result \code{c(0, 0, 1, 1, 9, 9)}.
#'   \cr\cr
#'   \strong{\code{reps_each} and \code{each_reps}}
#'   \cr Call \code{reps} then \code{each} and vice versa, respectively.
#' @param ... One or more atomic vectors to be replicated, reduced to unique
#'   values, or reduced to unique duplicated values.
#' @param r An integer scalar giving the number of replications of entire
#'   vectors.
#' @param e An integer scalar giving the number of times each value is to be
#'   replicated in place before replication of the entire vector.
#' @return An atomic vector.
#' @examples
#' reps(0:4, 5:9, r = 2)
#' each(0:4, 5:9, r = 2)
#' reps_each(0:4, 5:9, r = 2, e = 2)
#' each_reps(0:4, 5:9, e = 2, r = 2)
#' @export
reps <- function(..., r = 1) {
  VX <- all(sapply(list(...), xvec))
  VR <- cmp_psw_scl(r)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be atomic vectors.")}
  if (!VR) {E <- c(E, "\n  * [r] must be a positive, whole-number scalar.")}
  if (xdef(E)) {stop(E)}
  rep.int(av(...), r)
}

#' @rdname reps
#' @export
each <- function(..., e = 1) {
  VX <- all(sapply(list(...), xvec))
  VE <- cmp_psw_scl(e)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be atomic vectors.")}
  if (!VE) {E <- c(E, "\n  * [e] must be a positive, whole-number scalar.")}
  if (xdef(E)) {stop(E)}
  rep(av(...), each = e)
}

#' @rdname reps
#' @export
reps_each <- function(..., r = 1, e = 1) {
  VX <- all(sapply(list(...), xvec))
  VR <- cmp_psw_scl(r)
  VE <- cmp_psw_scl(e)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be atomic vectors.")}
  if (!VR) {E <- c(E, "\n  * [r] must be a positive, whole-number scalar.")}
  if (!VE) {E <- c(E, "\n  * [e] must be a positive, whole-number scalar.")}
  if (xdef(E)) {stop(E)}
  rep(rep.int(av(...), r), each = e)
}

#' @rdname reps
#' @export
each_reps <- function(..., e = 1, r = 1) {
  VX <- all(sapply(list(...), xvec))
  VE <- cmp_psw_scl(e)
  VR <- cmp_psw_scl(r)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be atomic vectors.")}
  if (!VE) {E <- c(E, "\n  * [e] must be a positive, whole-number scalar.")}
  if (!VR) {E <- c(E, "\n  * [r] must be a positive, whole-number scalar.")}
  if (xdef(E)) {stop(E)}
  rep.int(rep(av(...), each = e), r)
}
