#' @name wraps_graphics
#' @title Tnin Wraps for Functions in Package `graphics`
#' @description \tabular{ll}{
#'   WRAPPER   \tab GRAPHICS FUNCTION                                        \cr
#'   `strh`    \tab \code{\link[graphics]{strheight}}.                       \cr
#'   `strw`    \tab \code{\link[graphics]{strwidth}}.                          }
#' @inherit graphics::strheight
#' @export
strh <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strheight(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}

#' @rdname wraps_graphics
#' @inherit graphics::strwidth
#' @export
strw <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strwidth(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}
