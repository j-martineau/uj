#' @name gr
#' @family wraps
#' @family strings
#' @family plotting
#' @title Thin wrappers of `graphics` functions.
#' @description \tabular{rl}{
#'       `strh` \tab   Thinly wraps \code{\link[graphics]{strheight}}.
#'   \cr `strw` \tab   Thinly wraps \code{\link[graphics]{strwidth}}.
#' }
#' @inherit graphics::strheight
#' @export
strh <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strheight(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}

#' @rdname gr
#' @inherit graphics::strwidth
#' @export
strw <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strwidth(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}
