#' @name bs.
#' @family wraps
#' @title Thin Wraps of Base Package Functions
#' @description \tabular{ll}{
#'   \code{u}      \tab \code{\link[base]{unique}}                           \cr
#'   \code{up}     \tab \code{\link[base]{ceiling}}                          \cr
#'   \code{dn}     \tab \code{\link[base]{floor}}                            \cr
#'   \code{mid}    \tab \code{\link[base]{substr}}                           \cr
#'   \code{spf}    \tab \code{\link[base]{sprintf}}                          \cr
#'   \code{levs}   \tab \code{\link[base]{levels}}                             }
#' @export
bs. <- function() {help("bs.", package = "uj")}

#' @describeIn bs. Thin wrapper for \code{\link[base]{unique}}. Differs from
#'   \code{\link{uv}} in that \code{uv} takes an arbitrary number of parameters
#'   and \link[a]{atomizes} them before getting unique atomic values.
#' @inherit base::ceiling
#' @export
u <- function(x) {base::unique(x)}

#' @describeIn bs. Round down to nearest integer.
#' @inherit base::floor
#' @export
dn <- function(x) {base::floor(x)}

#' @describeIn bs. Round up to nearest integer.
#' @inherit base::ceiling
#' @export
up <- function(x) {base::ceiling(x)}

#' @describeIn bs. Extract substring(s).
#' @inherit base::substr
#' @export
mid <- function(x, start, stop) {base::substr(x, start, stop)}

#' @describeIn bs. Format inlays into strings.
#' @inherit base::sprintf
#' @export
spf <- function(fmt, ...) {base::sprintf(fmt, ...)}

#' @describeIn bs. Get levels of an unordered factor or ordered factor object.
#' @inherit base::levels
#' @export
levs <- function(x) {base::levels(x)}
