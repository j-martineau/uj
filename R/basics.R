#' @name basics
#' @family wraps
#' @title Wrappers for `base` functions
#' @description \tabular{rl}{
#'      `levs`   \tab Thinly wraps \code{\link[base]{levels}}.
#'   \cr         \tab   
#'   \cr `spf`   \tab Thinly wraps \code{\link[base]{sprintf}}.
#'   \cr `mid`   \tab Thinly wraps \code{\link[base]{substr}}.
#'   \cr `len`   \tab Thinly wraps \code{\link[base]{nchar}}.
#'   \cr         \tab   
#'   \cr  `up`   \tab Thinly wraps \code{\link[base]{ceiling}}.
#'   \cr  `dn`   \tab Thinly wraps \code{\link[base]{floor}}.
#'   \cr         \tab   
#'   \cr   `u`   \tab Thinly wraps \code{\link[base]{unique}}\eqn{^1}.
#'   \cr   `w`   \tab Thinly Wraps \code{\link[base]{which}}\eqn{^1}.
#'   \cr         \tab   
#'   \cr  `g1`   \tab Calls \code{\link[base:paste0]{paste0(av(...), collapse = " ")}}.
#'   \cr  `g0`   \tab Calls \code{\link[base:paste0]{paste0(av(...), collapse = "")}}.
#'   \cr   `g`   \tab Calls \code{\link[base:paste0]{paste0(av(...), collapse = g)}}
#'   \cr         \tab   
#'   \cr  `p1`   \tab Calls \code{\link[base:paste]{paste(..., sep = " ")}}.
#'   \cr  `p0`   \tab Calls \code{\link[base:paste]{paste(..., sep = "")}}.
#'   \cr   `p`   \tab Calls \code{\link[base:paste]{paste(..., sep = p)}}
#' }
#'    \eqn{^{1.}} Calls `paste0(av(...), `
#' \cr    \eqn{^{2.}} Differs from \code{\link{uv}}, which calls `unique(av(...))`.
#' \cr    \eqn{^{3.}} Differs from \code{\link{wv}}, which calls `which(av(...))`.
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
#' @inherit base::which
#' @export
w <- function(x) {base::which(x)}

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

#' @rdname basics
#' @inherit base::paste0
#' @export
g <- function(g, ...) {paste(av(...), collapse = g)}

#' @rdname basics
#' @export
g0 <- function(...) {paste(av(...), collapse = "")}

#' @rdname basics
#' @export
g1 <- function(...) {paste(av(...), collapse = " ")}

#' @rdname basics
#' @inherit base::paste
#' @export
p <- function(p, ...) {paste(..., sep = p)}

#' @rdname basics
#' @export
p0 <- function(...) {paste(..., sep = "")}

#' @rdname basics
#' @export
p1 <- function(...) {paste(..., sep = " ")}
