#' @name binfuns
#' @family extensions
#' @family logicals
#' @family failsafe
#' @title Scalar Result Failsafe Binary Logical Functions
#' @section Functions in This Family:
#'   \strong{`%IS%`}
#'   \cr Evaluates whether `x` and `y` are identical.
#'   \cr\cr
#'   \strong{`%ISNT%`}
#'   \cr Evaluates whether `x` and `y` are NOT identical.
#'   \cr\cr
#'   \strong{`%EQ%`}
#'   \cr Evaluates whether `x` and `y` are setequal. (A)
#'   \cr\cr
#'   \strong{`%NEQ%`}
#'   \cr Evaluates whether `x` and `y` are NOT setequal. (A)
#'   \cr\cr
#'   \strong{`%AND%`}
#'   \cr Evaluates whether `x` and `y` are both scalar `TRUE`.
#'   \cr\cr
#'   \strong{`%OR%`}
#'   \cr Evaluates whether `x` and `y` is scalar `TRUE` or both.
#'   \cr\cr
#'   \strong{`%NOR%`}
#'   \cr Evaluates whether `x` and `y` are both scalar `FALSE`.
#'   \cr\cr
#'   \strong{`%ONE%`}
#'   \cr Evaluates whether either `x` or `y` is scalar `TRUE`, but not both.
#'   \cr\cr
#'   \strong{`%IN%`}
#'   \cr Evaluates whether atomic scalar `x` is in atomic object `y`. (B)
#'   \cr\cr
#'   \strong{`%OUT%`}
#'   \cr Evaluates whether atomic scalar `x` is NOT in atomic object `y`. (B)
#'   \cr\cr
#'   \strong{`%HAS%`}
#'   \cr Evaluates whether atomic object `x` contains atomic scalar `y` (C).
#'   \cr\cr
#'   \strong{`%LACKS%`}
#'   \cr Evaluates whether atomic object `x` lacks (does not contain) atomic
#'   scalar `y`. (C)
#'   \cr\cr
#'   (A) If `x` and `y` are not \code{\link{compatible}}, `%EQ%` and `%NEQ%`
#'   return `FALSE` and `TRUE`, respectively.
#'   \cr\cr
#'   (B) If `x` is not atomic, `y` is not atomic scalar, or `x` and `y` are not
#'   \code{\link{compatible}}, `%IN%` and `%OUT%` return `FALSE` and `TRUE`,
#'   respectively.
#'   \cr\cr
#'   (C) If `x` is not atomic scalar, `y` is not atomic, or `x` and `y` are not
#'   \code{\link{compatible}}, `%HAS%` and `%LACKS%` return `FALSE` and `TRUE`,
#'   respectively.
#' @param x,y Any objects.
#' @return A logical scalar.
#' @export
`%IS%` <- function(x, y) {identical(x, y)}

#' @rdname binfuns
#' @export
`%ISNT%` <- function(x, y) {!identical(x, y)}

#' @rdname binfuns
#' @export
`%EQ%` <- function(x, y) {isEQ(x, y)}

#' @rdname binfuns
#' @export
`%NEQ%` <- function(x, y) {!isEQ(x, y)}

#' @rdname binfuns
#' @export
`%AND%` <- function(x, y) {isT(x) & isT(y)}

#' @rdname binfuns
#' @export
`%OR%` <- function(x, y) {isT(x) | isT(y)}

#' @rdname binfuns
#' @export
`%NOT%` <- function(x, y) {isF(x) & isF(y)}

#' @rdname binfuns
#' @export
`%ONE%` <- function(x, y) {f0(isT(x), isF(y), isT(y))}

#' @rdname binfuns
#' @export
`%IN%` <- function(x, y) {isIN(x, y)}

#' @rdname binfuns
#' @export
`%OUT%` <- function(x, y) {notIN(x, y)}

#' @rdname binfuns
#' @export
`%HAS%` <- function(x, y) {isIN(y, x)}

#' @rdname binfuns
#' @export
`%LACKS%` <- function(x, y) {notIN(y, x)}
