#' @encoding UTF-8
#' @family properties
#' @title Manage classes
#' @description \tabular{rl}{
#'       `has_cls`   \tab Evaluate whether class(es) in `...` are (all) classes of `x`.
#'   \cr             \tab  
#'   \cr `add_cls`   \tab Add class(es) in `...` to classes of `x`.
#'   \cr             \tab  
#'   \cr `set_cls`   \tab Set class of `x` to class(es) in `...`.
#'   \cr             \tab  
#'   \cr    `xcls`   \tab Removes class(es) in `...` from classes of `x`.
#'   \cr             \tab  
#'   \cr     `cls`   \tab Gets class(es) of `x`.
#' }
#' @param x An R object.
#' @param ... Unquoted class names and/or names of variables whose contents are \link[=av]{atomized} and converted to character. When `...` args are names of variables in the calling environment, they are atomized, otherwise they are treated as unquoted class names.
#' @return *A character vector*
#'   \cr   `cls`
#'   \cr
#'   \cr *A logical scalar*
#'   \cr   `has_cls`
#'   \cr
#'   \cr *An R object*
#'   \cr   `add_cls`
#'   \cr   `set_cls`
#'   \cr   `xcls`
#' @examples
#' obj1. <- tb(letters = letters, numbers = 1:26)
#' obj2. <- set_cls(obj1., c("new", cls(obj1.)))
#' obj3. <- set_cls(obj1., "new")
#' cls(obj1.)
#' cls(obj2.)
#' cls(obj3.)
#' @export
cls <- function(x) {base::class(x)}

#' @rdname cls
#' @export
xcls <- function(x, ...) {
  rm.cls <- uj::flex_dots(...)
  all.cls <- base::class(x)
  keep.cls <- all.cls[uj::not_in(all.cls, rm.cls)]
  if (base::length(keep.cls) == 0) {stop(uj:::.errs("There are no classes remaining."))}
  base::class <- keep.cls
  x
}

#' @rdname cls
#' @export
set_cls <- function(x, ...) {base::class(x) <- uj::flex_dots(...)}

#' @rdname cls
#' @export
add_cls <- function(x, ...) {base::class(x) <- uj::uv(base::c(base::class(x), uj::flex_dots(...)))}

#' @rdname cls
#' @export
has_cls <- function(x, ...) {uj::allIN(uj::flex_dots(...), base::class(x))}
