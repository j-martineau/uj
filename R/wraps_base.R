#' @name wraps_base
#' @family wraps
#' @title Thin wrappers for `base` functions
#' @description \itemize{
#'   \item **`u`**: thinly wraps code{\link[base]{unique}}.
#'   \item **`up`**: thinly wraps \code{\link[base]{ceiling}}.
#'   \item **`dn`**: thinly wraps \code{\link[base]{floor}}.
#'   \item **`mid`**: thinly wraps \code{\link[base]{substr}}.
#'   \item **`spf`**: thinly wraps \code{\link[base]{sprintf}}.
#'   \item **`levs`**: thinly wraps \code{\link[base]{levels}}.
#' }
#' NOTE: `u` differs from \code{\link{uv}} in that `uv` \link[=a]{atomizes} an arbitrary number of `...` arguments before calling `unique`.
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
