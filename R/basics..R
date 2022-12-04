#' @name basics
#' @family wraps
#' @title Thin wrappers for `base` functions
#' @description \tabular{rl}{
#'      `levs`   \tab Thinly wraps \code{\link[base]{levels}}.
#'   \cr `spf`   \tab Thinly wraps \code{\link[base]{sprintf}}.
#'   \cr `mid`   \tab Thinly wraps \code{\link[base]{substr}}.
#'   \cr `len`   \tab Thinly wraps \code{\link[base]{nchar}}.
#'   \cr  `up`   \tab Thinly wraps \code{\link[base]{ceiling}}.
#'   \cr  `dn`   \tab Thinly wraps \code{\link[base]{floor}}.
#'   \cr   `u`   \tab Thinly wraps \code{\link[base]{unique}}\eqn{^1}.
#' }
#'     \eqn{^1} Differs from \code{\link{uv}} because `uv` calls `unique(av(...))`.
#' @examples
#' vals <- c(1:3, 2:4) / 3
#' vars <- c("a", "bb", "ccc", "dddd", "ccc", "bb")
#' text <- "%s = %0.2f and %s = %0.0f"
#'
#' vals
#' vars
#' text
#'
#' u(vals)
#' u(vars)
#' u(data.frame(var = vars, val = vals))
#'
#' dn(vals)
#' up(vals)
#'
#' len(vars)
#' mid(vars, 1, 3)
#'
#' spf(text, vars[1:3], vals[1:3], vars[4:6], vals[4:6])
#' @inherit base::unique
#' @export
u <- function(x) {base::unique(x)}

#' @rdname basics
#' @inherit base::floor
#' @export
dn <- function(x) {base::floor(x)}

#' @rdname basics
#' @inherit base::ceiling
#' @export
up <- function(x) {base::ceiling(x)}

#' @rdname basics
#' @inherit base::nchar
#' @export
len <- function(x, type = "chars", allowNA = FALSE, keepNA = NA) {base::nchar(x, type = type, allowNA = allowNA, keepNA = keepNA)}

#' @rdname basics
#' @inherit base::substr
#' @export
mid <- function(x, start, stop) {base::substr(x, start, stop)}

#' @rdname basics
#' @inherit base::sprintf
#' @export
spf <- function(fmt, ...) {base::sprintf(fmt, ...)}

#' @rdname basics
#' @inherit base::levels
#' @export
levs <- function(x) {base::levels(x)}
