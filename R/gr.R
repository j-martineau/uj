#' @name gr.
#' @title Wraps for functions in package \code{graphics}
#' @export
gr. <- function() {help("gr.", package = "uj")}

#' @describeIn gr. Thin wrapper for \code{\link[graphics]{strheight}}
#' @inherit graphics::strheight
#' @export
strh <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strheight(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}

#' @describeIn gr. Thin wrapper for \code{\link[graphics]{strheight}}
#' @inherit graphics::strwidth
#' @export
strw <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strwidth(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}
