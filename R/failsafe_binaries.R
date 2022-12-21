#' @name failsafe_binaries
#' @encoding UTF-8
#' @family extensions
#' @family failsafe
#' @family logicals
#' @title Failsafe binary logical functions
#' @description Binary functions that always produce either `TRUE` or `FALSE` unless calling `identity(x)` or `identity(y)` produces an error.
#' \cr\cr These functions evaluate whether two variables meet requirements as defined in the following table:
#' \tabular{rl}{
#'           `%HAS%`   \tab Atomic object `x` has (contains) atomic scalar `y`\eqn{^a}.
#'   \cr               \tab  
#'   \cr   `%LACKS%`   \tab Atomic object `x` lacks (does not contain) atomic scalar `y`\eqn{^a}.
#'   \cr               \tab  
#'   \cr      `%IN%`   \tab Atomic scalar `x` is contained in atomic object `y`\eqn{^b}.
#'   \cr               \tab  
#'   \cr     `%OUT%`   \tab Atomic scalar `x` is outside of (not contained in) atomic object `y`\eqn{^b}.
#'   \cr               \tab  
#'   \cr      `%IS%`   \tab `x` and `y` are \code{\link[base]{identical}}.
#'   \cr               \tab  
#'   \cr    `%ISNT%`   \tab `x` and `y` are *not* \code{\link[base]{identical}}.
#'   \cr               \tab  
#'   \cr      `%EQ%`   \tab `x` and `y` are \code{\link[base]{setequal}}\eqn{^c}.
#'   \cr               \tab  
#'   \cr     `%NEQ%`   \tab `x` and `y` are *not* \code{\link[base]{setequal}}\eqn{^c}.
#'   \cr               \tab  
#'   \cr     `%NOR%`   \tab Neither `x` nor `y` is scalar `TRUE`.
#'   \cr               \tab  
#'   \cr     `%AND%`   \tab Both `x` and `y` are scalar `TRUE`.
#'   \cr               \tab  
#'   \cr     `%ONE%`   \tab Either `x` is scalar `TRUE`, `y` is scalar `TRUE`, but not both.
#'   \cr               \tab  
#'   \cr      `%OR%`   \tab `x` is scalar `TRUE`, `y` is scalar `TRUE`, or both.
#' }
#' \eqn{^{a.}} If `x` is not atomic scalar, `y` is not atomic, or `x` and `y` are not compatible, `%HAS%` and `%LACKS%` return `FALSE` and `TRUE`, respectively.
#' \cr\cr \eqn{^{b.}} If `x` is not atomic, `y` is not atomic scalar, or `x` and `y` are not compatible, `%IN%` and `%OUT%` return `FALSE` and `TRUE`, respectively.
#' \cr\cr \eqn{^{c.}} If `x` and `y` are not \code{\link{compatible}}, `%EQ%` and `%NEQ%` return `FALSE` and `TRUE`, respectively.
#' @param x,y Any R objects.
#' @return A logical scalar.
#' @examples
#' abc <- c("a", "b", "c")
#' ABC <- abc
#' attr(ABC, "custom") <- "custom"
#'
#' abc %IS% letters[1:3]
#' abc %IS% ABC
#' abc %IS% NULL
#'
#' abc %EQ% letters[1:3]
#' abc %EQ% ABC
#' abc %EQ% NULL
#'
#' NULL %IN% abc
#' "a" %IN% abc
#' 1 %IN% abc
#'
#' NULL %OUT% abc
#' "a" %OUT% abc
#' 1 %OUT% abc
#'
#' abc %HAS% NULL
#' abc %HAS% "a"
#' abc %HAS% 1
#'
#' abc %LACKS% NULL
#' abc %LACKS% "a"
#' abc %LACKS% 1
#'
#' FALSE %OR% FALSE
#' TRUE %OR% TRUE
#' TRUE %OR% 42
#' "A" %OR% FALSE
#'
#' FALSE %AND% FALSE
#' TRUE %AND% TRUE
#' TRUE %AND% 42
#' "A" %AND% FALSE
#'
#' FALSE %NOR% FALSE
#' TRUE %NOR% TRUE
#' TRUE %NOR% 42
#' "A" %NOR% FALSE
#'
#' FALSE %ONE% FALSE
#' TRUE %ONE% TRUE
#' TRUE %ONE% 42
#' "A" %ONE% FALSE
#' @export
`%IS%` <- function(x, y) {identical(x, y)}

#' @rdname failsafe_binaries
#' @export
`%ISNT%` <- function(x, y) {!identical(x, y)}

#' @rdname failsafe_binaries
#' @export
`%EQ%` <- function(x, y) {isEQ(x, y)}

#' @rdname failsafe_binaries
#' @export
`%NEQ%` <- function(x, y) {!isEQ(x, y)}

#' @rdname failsafe_binaries
#' @export
`%NOR%` <- function(x, y) {!isTRUE(x) & !isTRUE(y)}

#' @rdname failsafe_binaries
#' @export
`%AND%` <- function(x, y) {isTRUE(x) & isTRUE(y)}

#' @rdname failsafe_binaries
#' @export
`%OR%` <- function(x, y) {isTRUE(x) | isTRUE(y)}

#' @rdname failsafe_binaries
#' @export
`%ONE%` <- function(x, y) {sum(isTRUE(x), isTRUE(y)) == 1}

#' @rdname failsafe_binaries
#' @export
`%IN%` <- function(x, y) {isIN(x, y)}

#' @rdname failsafe_binaries
#' @export
`%OUT%` <- function(x, y) {notIN(x, y)}

#' @rdname failsafe_binaries
#' @export
`%HAS%` <- function(x, y) {isIN(y, x)}

#' @rdname failsafe_binaries
#' @export
`%LACKS%` <- function(x, y) {notIN(y, x)}
