#' @name binfuns
#' @family extensions
#' @family logicals
#' @family failsafe
#' @title Scalar Result Failsafe Binary Logical Functions
#' @description \tabular{ll}{
#'   FUNCTION    \tab WHAT IT DOES                                           \cr
#'   `%IS%`      \tab Evaluate whether `x` and `y` are identical.            \cr
#'   `%ISNT%`    \tab Evaluate whether `x` and `y` are NOT identical.        \cr
#'   `%EQ%`      \tab Evaluate whether `x` and `y` are setequal. (A)         \cr
#'   `%NEQ%`     \tab Evaluate whether `x` and `y` are NOT setequal. (A)     \cr
#'   `%AND%`     \tab Evaluate whether `x` and `y` are both scalar `TRUE`.   \cr
#'   `%OR%`      \tab Evaluate whether `x` and `y` is scalar `TRUE` or both. \cr
#'   `%NOR%`     \tab Evaluate whether `x` and `y` are both scalar `FALSE`.  \cr
#'   `%ONE%`     \tab Evaluate whether either `x` or `y` is scalar `TRUE`, but
#'                    not both.                                              \cr
#'   `%IN%`      \tab Evaluate whether atomic scalar `x` is contained in atomic
#'                    object `y`. (B)                                        \cr
#'   `%OUT%`     \tab Evaluate whether atomic scalar `x` is NOT contained in
#'                    atomic object `y`. (B)                                 \cr
#'   `%HAS%`     \tab Evaluate whether atomic object `x` contains atomic scalar
#'                    `y` (C).                                               \cr
#'   `%LACKS%`   \tab Evaluate whether atomic object `x` lacks (does not
#'                    contain) atomic scalar `y`. (C)                          }
#'   (A) If `x` and `y` are not \link[=compatible]{compatible}, `%EQ%` and
#'   `%NEQ%` return `FALSE` and `TRUE`, respectively.
#'   \cr\cr
#'   (B) If `x` is not atomic, `y` is not atomic scalar, or `x` and `y` are not
#'   compatible, `%IN%` and `%OUT%` return `FALSE` and `TRUE`, respectively.
#'   \cr\cr
#'   (C) If `x` is not atomic scalar, `y` is not atomic, or `x` and `y` are not
#'   compatible, `%HAS%` and `%LACKS%` return `FALSE` and `TRUE`, respectively.
#' @param x,y Any R objects.
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
