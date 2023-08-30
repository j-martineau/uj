#' @encoding UTF-8
#' @family properties
#' @title Manage classes
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
#' egObj2 <- setCLS(egObj1, c("new", cls(egObj1)))
#' egObj3 <- setCLS(egObj1, "new")
#' cls(egObj1)
#' cls(egObj2)
#' cls(egObj3)
#' @export
cls <- function(x) {base::class(x)}

#' @rdname cls
#' @export
rm_cls <- function(x, ...) {
  RmClass <- base::as.character(uj::uv(...))
  AllClass <- base::class(x)
  KeepClass <- AllClass[!(AllClass %in% RmClass)]
  if (base::length(KeepClass) == 0) {uj::stopperr("There are no classes remaining.", .PKG = "uj")}
  base::class(x) <- KeepClass
  x
}

#' @rdname cls
#' @export
xcls <- rm_cls

#' @rdname cls
#' @export
set_cls <- function(x, ...) {
  NewClass <- base::as.character(uj::uv(...))
  base::class(x) <- NewClass
  x
}

#' @rdname cls
#' @export
add_cls <- function(x, ...) {
  CurrClass <- base::class(x)
  NewClass <- base::as.character(uj::uv(...))
  AllClass <- base::unique(base::c(CurrClass, NewClass))
  base::class(x) <- AllClass
  x
}

#' @rdname cls
#' @export
has_cls <- function(x, ...) {
  CurrClass <- base::class(x)
  CheckClass <- base::as.character(uj::uv(...))
  base::all(CurrClass %in% CheckClass)
  x
}
