#' @name wraps_graphics
#' @title Wraps for Functions in Package \code{graphics}
#' @description Thin wrappers for \code{\link[graphics]{strheight}} and
#'   \code{\link[graphics]{strwidth}}.
#' @inherit graphics::strheight
#' @export
strh <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strheight(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}

#' @rdname wraps_graphics
#' @export
strw <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strwidth(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}
