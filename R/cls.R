#' @encoding UTF-8
#' @family properties
#' @title Manage classes
#' @details
#' \tabular{ll}{  `rm_cls, xcls`   \tab Removes class(es) in `...` from classes of `X`.               \cr   \tab   \cr
#'                `has_cls`        \tab Evaluate whether class(es) in `...` are (all) classes of `X`. \cr   \tab   \cr
#'                `add_cls`        \tab Add class(es) in `...` to classes of `X`.                     \cr   \tab   \cr
#'                `set_cls`        \tab Set class of `X` to class(es) in `...`.                       \cr   \tab   \cr
#'                `cls`            \tab Gets class(es) of `X`.                                                       }
#' @param X An R object.
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
cls <- function(X) {base::class(X)}

#' @rdname cls
#' @export
rm_cls <- function(X, ...) {
  RmClass <- base::as.character(uj::uv(...))
  AllClass <- base::class(X)
  KeepClass <- AllClass[!(AllClass %in% RmClass)]
  if (base::length(KeepClass) == 0) {uj::stopperr("There are no classes remaining.", PKG = "uj")}
  base::class(X) <- KeepClass
  X
}

#' @rdname cls
#' @export
xcls <- rm_cls

#' @rdname cls
#' @export
set_cls <- function(X, ...) {
  NewClass <- base::as.character(uj::uv(...))
  base::class(X) <- NewClass
  X
}

#' @rdname cls
#' @export
add_cls <- function(X, ...) {
  CurrClass <- base::class(X)
  NewClass <- base::as.character(uj::uv(...))
  AllClass <- base::unique(base::c(CurrClass, NewClass))
  base::class(X) <- AllClass
  X
}

#' @rdname cls
#' @export
has_cls <- function(X, ...) {
  CurrClass <- base::class(X)
  CheckClass <- base::as.character(uj::uv(...))
  base::all(CurrClass %in% CheckClass)
  X
}
