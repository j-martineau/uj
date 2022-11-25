#' @name wraps_base
#' @family wraps
#' @title Thin Wraps of Functions from Package `base`
#' @description \tabular{ll}{
#'   WRAPPER   \tab BASE FUNCTION                                            \cr
#'   `u` (A)   \tab \code{\link[base]{unique}}                               \cr
#'   `up`      \tab \code{\link[base]{ceiling}}                              \cr
#'   `dn`      \tab \code{\link[base]{floor}}                                \cr
#'   `mid`     \tab \code{\link[base]{substr}}                               \cr
#'   `spf`     \tab \code{\link[base]{sprintf}}                              \cr
#'   `levs`    \tab \code{\link[base]{levels}}                                 }
#' (A) `u` differs from \code{\link{uv}} in that `uv` \link[=a]{atomizes} an
#'  arbitrary number of `...` arguments before calling `unique`.
#' @inherit base::unique
#' @export
u <- function(x) {base::unique(x)}

#' @rdname wraps_base
#' @inherit base::floor
#' @export
dn <- function(x) {base::floor(x)}

#' @rdname wraps_base
#' @inherit base::ceiling
#' @export
up <- function(x) {base::ceiling(x)}

#' @rdname wraps_base
#' @inherit base::substr
#' @export
mid <- function(x, start, stop) {base::substr(x, start, stop)}

#' @rdname wraps_base
#' @inherit base::sprintf
#' @export
spf <- function(fmt, ...) {base::sprintf(fmt, ...)}

#' @rdname wraps_base
#' @inherit base::levels
#' @export
levs <- function(x) {base::levels(x)}
