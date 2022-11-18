#' @name binfuns.
#' @family extensions
#' @family logicals
#' @family failsafe
#' @title Scalar Result Failsafe Binary Logical Functions
#' @description \tabular{ll}{
#'   \code{\%IS\%}          \tab Evaluates whether \code{x} and \code{y} are
#'                               \code{\link[base]{identical}}.            \cr
#'   \code{\%ISNT\%}        \tab Evaluates whether \code{x} and \code{y} are
#'                               NOT \code{\link[base]{identical}}.        \cr
#'   \code{\%EQ\%} (a)      \tab Evaluates whether \code{x} and \code{y} are
#'                               \code{\link[base]{setequal}}.             \cr
#'   \code{\%NEQ\%} (a)     \tab Evaluates whether \code{x} and \code{y} are
#'                               NOT \code{\link[base]{setequal}}.         \cr
#'   \code{\%AND\%}         \tab Evaluates whether \code{x} and \code{y} are
#'                               both scalar \code{TRUE}.                  \cr
#'   \code{\%OR\%}          \tab Evaluates whether \code{x} or \code{y} is
#'                               scalar \code{TRUE} or both.               \cr
#'   \code{\%NOR\%}         \tab Evaluates whether \code{x} and \code{y} are
#'                               both scalar \code{FALSE}.                 \cr
#'   \code{\%ONE\%}         \tab Evaluates whether
#'                               \link[=base:xor]{either \code{x} or \code{y}}
#'                               is scalar \code{TRUE}, but not both.      \cr
#'   \code{\%IN\%} (b)      \tab Evaluates whether atomic scalar \code{x} is
#'                               an element in atomic object \code{y}.     \cr
#'   \code{\%OUT\%} (b)     \tab Evaluates whether atomic scalar \code{x} is
#'                               NOT an element in atomic object \code{y}. \cr
#'   \code{\%HAS\%} (c)     \tab Evaluates whether atomic object \code{x}
#'                               contains atomic scalar \code{y}.          \cr
#'   \code{\%LACKS\%} (c)   \tab Evaluates whether atomic object \code{x}
#'                               lacks (does not contain) atomic scalar
#'                               \code{y}.                                   }
#' (a) If \code{x} and \code{y} are not \code{\link{compatible}}, \code{\%EQ\%}
#' and \code{\%NEQ\%} return \code{FALSE} and \code{TRUE}, respectively.
#' \cr\cr
#' (b) If \code{x} is not atomic, \code{y} is not atomic scalar, or \code{x} and
#' \code{y} are not \code{\link{compatible}}, \code{\%IN\%} and \code{\%OUT\%}
#' return \code{FALSE} and \code{TRUE}, respectively.
#' \cr\cr
#' (c) If \code{x} is not atomic scalar, \code{y} is not atomic, or \code{x} and
#' \code{y} are not \code{\link{compatible}}, \code{\%HAS\%} and
#' \code{\%LACKS\%} return \code{FALSE} and \code{TRUE}, respectively.
#' @param x,y Any objects.
#' @return A logical scalar.
#' @export
binfuns. <- function() {help("binfuns.", package = "uj")}

#' @rdname binfuns.
#' @export
`%IS%` <- function(x, y) {identical(x, y)}

#' @rdname binfuns.
#' @export
`%ISNT%` <- function(x, y) {!identical(x, y)}

#' @rdname binfuns.
#' @export
`%EQ%` <- function(x, y) {isEQ(x, y)}

#' @rdname binfuns.
#' @export
`%NEQ%` <- function(x, y) {!isEQ(x, y)}

#' @rdname binfuns.
#' @export
`%AND%` <- function(x, y) {isT(x) & isT(y)}

#' @rdname binfuns.
#' @export
`%OR%` <- function(x, y) {isT(x) | isT(y)}

#' @rdname binfuns.
#' @export
`%NOT%` <- function(x, y) {isF(x) & isF(y)}

#' @rdname binfuns.
#' @export
`%ONE%` <- function(x, y) {f0(isT(x), isF(y), isT(y))}

#' @rdname binfuns.
#' @export
`%IN%` <- function(x, y) {isIN(x, y)}

#' @rdname binfuns.
#' @export
`%OUT%` <- function(x, y) {notIN(x, y)}

#' @rdname binfuns.
#' @export
`%HAS%` <- function(x, y) {isIN(y, x)}

#' @rdname binfuns.
#' @export
`%LACKS%` <- function(x, y) {notIN(y, x)}
