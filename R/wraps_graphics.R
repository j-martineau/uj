#' @name wraps_graphics
#' @family wraps
#' @title Thin wrappers of `graphics` functions.
#' @description \itemize{
#'   \item **`strh`**: thinly wraps \code{\link[graphics]{strheight}}.
#'   \item **`strw`**: thinly wraps \code{\link[graphics]{strwidth}}.
#' }
#' @inherit graphics::strheight
#' @export
strh <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strheight(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}

#' @rdname wraps_graphics
#' @inherit graphics::strwidth
#' @export
strw <- function(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...) {graphics::strwidth(s = s, units = units, cex = cex, font = font, vfont = vfont, ...)}
