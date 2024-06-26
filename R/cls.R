#' @encoding UTF-8
#' @family properties
#' @title Manage Classes
#' @details
#' \tabular{ll}{  `rm_cls, xcls`   \tab Removes class(es) in `...` from classes of `x`.               \cr   \tab   \cr
#'                `has_cls`        \tab Evaluate whether class(es) in `...` are (all) classes of `x`. \cr   \tab   \cr
#'                `add_cls`        \tab Add class(es) in `...` to classes of `x`.                     \cr   \tab   \cr
#'                `set_cls`        \tab Set class of `x` to class(es) in `...`.                       \cr   \tab   \cr
#'                `cls`            \tab Gets class(es) of `x`.                                                       }
#' @param x An R object.
#' @param ... Unquoted class names and/or names of variables whose contents are \link[=av]{atomized} and converted to character. When `...` args are names of variables in the calling environment, they are atomized, otherwise they are treated as unquoted class names.
#' @return **A character vector** \cr\cr `cls`
#' \cr\cr  **A logical scalar**   \cr\cr `has_cls`
#' \cr\cr  **An object**          \cr\cr `add_cls, set_cls, rm_cls, xcls`
#' @examples
#' egObj1 <- data.frame(letters = letters, numbers = 1:26, stringsAsFactors = F)
#' egObj2 <- set_cls(egObj1, c("new", cls(egObj1)))
#' egObj3 <- set_cls(egObj1, "new")
#' cls(egObj1)
#' cls(egObj2)
#' cls(egObj3)
#' @export
cls <- function(x) {base::class(x)}

#' @rdname cls
#' @export
rm_cls <- function(x, ...) {
  rmClass <- base::as.character(uj::uv(...))
  allClass <- base::class(x)
  keepClass <- allClass[!(allClass %in% rmClass)]
  if (base::length(keepClass) == 0) {uj::stopperr("There are no classes remaining.")}
  base::class(x) <- keepClass
  x
}

#' @rdname cls
#' @export
xcls <- rm_cls

#' @rdname cls
#' @export
set_cls <- function(x, ...) {
  newClass <- base::as.character(uj::uv(...))
  base::class(x) <- newClass
  x
}

#' @rdname cls
#' @export
add_cls <- function(x, ...) {
  currClass <- base::class(x)
  newClass <- base::as.character(uj::uv(...))
  allClass <- base::unique(base::c(currClass, newClass))
  base::class(x) <- allClass
  x
}

#' @rdname cls
#' @export
has_cls <- function(x, ...) {
  currClass <- base::class(x)
  checkClass <- base::as.character(uj::uv(...))
  base::all(currClass %in% checkClass)
  x
}
