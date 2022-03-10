#' @name recycling
#' @family meta
#' @title Recycling
#' @description Check for recyclability and recycle arguments.
#' @details \strong{\code{recyclable_n}}
#'   \cr Checks whether a vector of lengths represents recyclable arguments.
#'   \cr\cr
#'   \strong{\code{recyclable}}
#'   \cr Checks arguments in \code{...} for recyclability.
#'   \cr\cr
#'   \strong{\code{recycle}}
#'   \cr Recycles arguments in \code{...} in the environment of the function
#'   \code{gens.} generations back in the function call stack.
#' @param ... named arguments to be recycled in the calling function.
#' @param lengths. vector of lengths to check for recyclability.
#' @param targ. target length of recycled arguments.
#' @param n. for \code{recyclable_n}, the lengths of arguments to be recycled;
#'   for \code{recyclable} and \code{recycle}, \code{NULL} or the set of valid
#'   recycled argument lengths.
#' @param min. \code{NULL} or the minimum valid recycled argument length.
#' @param max. \code{NULL} or the maximum valid recycled argument length.
#' @param err. \code{TRUE} or \code{FALSE} indicating whether to throw an error
#'   if the arguments in \code{...} are not recyclable.
#' @return \code{recyclable_n} and \code{recyclable} return \code{TRUE} or
#'   \code{FALSE}. \code{recycle} returns \code{NULL} as it is called for the
#'   side effect of recycling arguments in the environment of a parent function.
#' @export
recyclable_n <- function(n., targ. = max(n.)) {
  VN <- cmp_psw_vec(n.)
  VT <- cmp_psw_scl(targ.)
  E <- NULL
  if (!VN) {E <- c(E, "\n  * [n.] must be a non-NA, positive, whole-number vector.")}
  if (!VT) {E <- c(E, "\n  * [targ.] must be a non-NA, positive, whole-number scalar.")}
  if (xdef(E)) {stop(E)}
  R <- targ. / n.
  all(R == round(R))
}

#' @rdname recycling
#' @export
recyclable <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL, err. = F) {
  VL  <- ...length() > 0
  VX  <- all(sapply(list(...), xvec))
  VN  <- f0(xnll(n.), T, cmp_psw_vec(n.))
  VMN <- cmp_psw_scl(min.)
  VMX <- f0(xnll(max.), T, cmp_psw_scl(max.))
  VT  <- f0(xnll(targ.), T, cmp_psw_scl(targ.))
  VE  <- isTF(err.)
  E <- NULL
  if (!VL ) {E <- c(E, "\n  * [...] is empty.")}
  if (!VX ) {E <- c(E, "\n  * Arguments in [...] must be atomic vects.")}
  if (!VN ) {E <- c(E, "\n  * [n.] must be NULL or a non-NA, positive, whole-number vector.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be a non-NA, positive, whole-number scalar")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be null or a non-NA, positive, whole-number scalar")}
  if (!VT ) {E <- c(E, "\n  * [targ.] must be NULL or a non-NA, positive, whole-number scalar.")}
  if (!VE ) {E <- c(E, "\n  * [err.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  x   <- list(...)
  L   <- lengths(x)
  VL  <- f0(xnll(n.  ), T, all(L %in% length(x)))
  VI  <- f0(xnll(min.), T, all(L >= min.))
  VA  <- f0(xnll(max.), T, all(L <= max.))
  E <- NULL
  if (!VL) {E <- c(E, "\n  * Arguments in [...] must have length in [n.]." )}
  if (!VI) {E <- c(E, "\n  * Arguments in [...] must have length ≥ [min.].")}
  if (!VA) {E <- c(E, "\n  * Arguments in [...] must have length ≤ [max.].")}
  if (xdef(E)) {stop(E)}
  if (xnll(targ.)) {targ. <- max(n.)}
  R <- recyclable_n(n., targ.)
  if (err. & !R) {stop("\n  * Arguments in [...] are not recyclable.")}
  R
}

#' @rdname recycling
#' @export
recycle <- function(..., n. = NULL, min. = 1, max. = NULL, targ. = NULL) {
  recyclable(..., n. = n., min. = min., max. = max., targ. = targ., err. = T)
  Names <- ...names()
  x <- list(...)
  R <- max(lengths(x) / lengths(x))
  for (i in 1:length(x)) {if (R[i] > 1) {vset(Names[i], rep(...elt(i), R[i]))}}
  NULL
}
