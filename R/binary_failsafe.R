#' @name binary_failsafe
#' @family failsafe
#' @family logicals
#' @title Failsafe binary logical functions
#' @description Binary functions that always produce either `TRUE` or `FALSE` unless calling `identity(x)` or `identity(y)` produces an error.
#' \itemize{
#'   \item **`%IS%`**: evaluates whether `x` and `y` are \code{\link[base]{identical}}.
#'   \item **`%ISNT%`**: evaluates whether `x` and `y` are NOT identical.
#'   \item **`%EQ%`**: evaluates whether `x` and `y` are \code{\link[base]{setequal}}.\eqn{^1}
#'   \item **`%NEQ%`**: evaluates whether `x` and `y` are NOT setequal.\eqn{^1}
#'   \item **`%IN%`**: evaluates whether atomic scalar `x` is contained in atomic object `y`.\eqn{^2}
#'   \item **`%OUT%`**: evaluates whether atomic scalar `x` is NOT contained in atomic object `y`.\eqn{^2}
#'   \item **`%OR%`**: evaluates whether `x` and `y` is scalar `TRUE` or both.
#'   \item **`%AND%`**: evaluates whether `x` and `y` are both scalar `TRUE`.
#'   \item **`%NOR%`**: evaluates whether `x` and `y` are both scalar `FALSE`.
#'   \item **`%ONE%`**: evaluates whether either `x` or `y` is scalar `TRUE`, but not both.
#'   \item **`%HAS%`**: evaluates whether atomic object `x` contains atomic scalar `y`.\eqn{^3}
#'   \item **`%LACKS%`**: evaluates whether atomic object `x` lacks (does not contain) atomic scalar `y`.\eqn{^3}
#' }
#' \eqn{^1} If `x` and `y` are not \code{\link{compatible}}, `%EQ%` and `%NEQ%` return `FALSE` and `TRUE`, respectively.
#' \cr\cr
#' \eqn{^2} If `x` is not atomic, `y` is not atomic scalar, or `x` and `y` are not compatible, `%IN%` and `%OUT%` return `FALSE` and `TRUE`, respectively.
#' \cr\cr
#' \eqn{^3} If `x` is not atomic scalar, `y` is not atomic, or `x` and `y` are not compatible, `%HAS%` and `%LACKS%` return `FALSE` and `TRUE`, respectively.
#' @param x,y Any R objects.
#' @return A logical scalar.
#' @export
`%IS%` <- function(x, y) {identical(x, y)}

#' @rdname binary_failsafe
#' @export
`%ISNT%` <- function(x, y) {!identical(x, y)}

#' @rdname binary_failsafe
#' @export
`%EQ%` <- function(x, y) {isEQ(x, y)}

#' @rdname binary_failsafe
#' @export
`%NEQ%` <- function(x, y) {!isEQ(x, y)}

#' @rdname binary_failsafe
#' @export
`%AND%` <- function(x, y) {isT(x) & isT(y)}

#' @rdname binary_failsafe
#' @export
`%OR%` <- function(x, y) {isT(x) | isT(y)}

#' @rdname binary_failsafe
#' @export
`%NOT%` <- function(x, y) {isF(x) & isF(y)}

#' @rdname binary_failsafe
#' @export
`%ONE%` <- function(x, y) {f0(isT(x), isF(y), isT(y))}

#' @rdname binary_failsafe
#' @export
`%IN%` <- function(x, y) {isIN(x, y)}

#' @rdname binary_failsafe
#' @export
`%OUT%` <- function(x, y) {notIN(x, y)}

#' @rdname binary_failsafe
#' @export
`%HAS%` <- function(x, y) {isIN(y, x)}

#' @rdname binary_failsafe
#' @export
`%LACKS%` <- function(x, y) {notIN(y, x)}
