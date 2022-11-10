#' @name recycling.
#' @family extensions
#' @family environments
#' @title Recycling and recyclability
#' @param ... Named arguments to be recycled in the environment of the calling
#'   function.
#' @param lengths. \link[cmp_psw_vec]{Complete positive whole-number vec} of
#'   lengths to check for recyclability.
#' @param targ. \link[cmp_psw_scl]{Complete positive whole-number scalar} giving
#'   the target length of recycled arguments.
#' @param n. For \code{recyclable_n}, a \link[cmp_psw_vec]{complete positive
#'   whole-number vec} giving the lengths of arguments to be recycled; for
#'   \code{recyclable} and \code{recycle}, either \code{NULL} or a
#'   \link[cmp_psw_vec]{complete positive whole-number vec} giving the set of
#'   valid recycled argument lengths.
#' @param min. \code{NULL} or \link[cmp_psw_scl]{complete positive whole-number
#'   scalar} giving the minimum valid recycled argument length.
#' @param max. \code{NULL} or \link[cmp_psw_scl]{complete positive whole-number
#'   scalar} giving the maximum valid recycled argument length.
#' @param err. \link[cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   throw an error if the arguments in \code{...} are not recyclable.
#' @return \code{recyclable_n} and \code{recyclable} return \code{TRUE} or
#'   \code{FALSE}. \code{recycle} returns \code{NULL} as it is called for the
#'   side effect of recycling arguments in the environment of a parent function.
#' @export
recycling. <- function() {help("recycling.", package = "uj")}

#' @describeIn recycling. Check whether the vector of lengths in \code{n.}
#'   represents recyclable arguments subject to the setting in \code{targ.}.
#' @export
recyclable_n <- function(n., targ. = max(n.)) {
  vn. <- cmp_psw_vec(n.)
  vt. <- cmp_psw_scl(targ.)
  err. <- NULL
  if (!vn.) {err. <- c(err., "\n • [n.] must be a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!vt.) {err. <- c(err., "\n • [targ.] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (idef(err.)) {stop(err.)}
  out. <- targ. / n.
  all(out. == round(out.))
}

#' @describeIn recycling. Evaluate whether arguments in \code{...} are
#'   recyclable subject to settings in the arguments \code{n.}, \code{min.},
#'   \code{max.}, and \code{targ.}.
#' @export
recyclable <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL, err. = F) {
  vd. <- all(sapply(list(...), ivec))
  vn. <- f0(inll(n.), T, cmp_psw_vec(n.))
  vt. <- f0(inll(targ.), T, cmp_psw_scl(targ.))
  ve. <- isTF(err.)
  vmn. <- cmp_psw_scl(min.)
  vmx. <- f0(inll(max.), T, cmp_psw_scl(max.))
  vnd. <- ...length() > 0
  errs. <- NULL
  if (!vnd.) {errs. <- c(errs., "\n • [...] is empty.")}
  if (!vd. ) {errs. <- c(errs., "\n • Arguments in [...] must be atomic vecs (?ivec).")}
  if (!vn. ) {errs. <- c(errs., "\n • [n.] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!vmn.) {errs. <- c(errs., "\n • [min.] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!vmx.) {errs. <- c(errs., "\n • [max.] must be null or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!vt. ) {errs. <- c(errs., "\n • [targ.] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!ve. ) {errs. <- c(errs., "\n • [err.] must be TRUE or FALSE.")}
  if (idef(errs.)) {stop(errs.)}
  x. <- list(...)
  ns. <- lengths(x.)
  vns. <- f0(inll(n.  ), T, all(ns. %in% length(x.)))
  vmn. <- f0(inll(min.), T, all(ns. >= min.))
  vmx. <- f0(inll(max.), T, all(ns. <= max.))
  err. <- NULL
  if (!vns.) {err. <- c(err., "\n • Arguments in [...] must have length in [n.]." )}
  if (!vmn.) {err. <- c(err., "\n • Arguments in [...] must have length ≥ [min.].")}
  if (!vmx.) {err. <- c(err., "\n • Arguments in [...] must have length ≤ [max.].")}
  if (idef(err.)) {stop(err.)}
  if (inll(targ.)) {targ. <- max(n.)}
  out. <- recyclable_n(n., targ.)
  if (err. & !out.) {stop("\n • Arguments in [...] are not recyclable.")}
  out.
}

#' @describeIn recycling. Recycle arguments in \code{...} in the environment of
#'   the calling function subject to settings in the arguments \code{n.},
#'   \code{min.}, \code{max.}, and \code{targ.}.
#' @export
recycle <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL) {
  recyclable(..., n. = n., min. = min., max. = max., targ. = targ., err. = T)
  x. <- list(...)
  out. <- max(lengths(x.) / lengths(x.))
  names. <- ...names()
  for (i. in 1:length(x.)) {if (out.[i.] > 1) {vset(names.[i.], rep(...elt(i.), out.[i.]))}}
  NULL
}
