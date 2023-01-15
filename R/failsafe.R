#' @name failsafe
#' @encoding UTF-8
#' @family extensions
#' @family failsafe
#' @family errs
#' @title Evaluate objects and flag any errors
#' @description The function `makeErr` produces a `'simpleError'` object with a the error message `'Intentionally-generated error.'`.
#' \cr\cr Every other function in this family calls `identity(x)` with return value depending on whether an error is produced as follows:
#'   \tabular{rll}{
#'                               \tab *Value when* \tab `identity(x)` *produces*
#'   \cr   *Failsafe function*   \tab *No error*   \tab   *An error*
#'   \cr       `failsafeBLANK`   \tab `x`          \tab   `""` (a blank string)
#'   \cr        `failsafeNULL`   \tab `x`          \tab   `NULL`
#'   \cr         `failsafeNAC`   \tab `x`          \tab   `NA_character_`
#'   \cr         `failsafeNAI`   \tab `x`          \tab   `NA_integer_`
#'   \cr         `failsafeNAR`   \tab `x`          \tab   `NA_real_`
#'   \cr         `failsafeNAL`   \tab `x`          \tab   `NA `\eqn{^1}
#'   \cr          `failsafeNA`   \tab `x`          \tab   `NA `\eqn{^1}
#'   \cr          `failsafeC0`   \tab `x`          \tab   `character(0)`
#'   \cr          `failsafeI0`   \tab `x`          \tab   `integer(0)`
#'   \cr          `failsafeL0`   \tab `x`          \tab   `logical(0)`
#'   \cr          `failsafeN0`   \tab `x`          \tab   `numeric(0)`
#'   \cr          `failsafeOR`   \tab `x`          \tab   The `OR` arg
#'   \cr            `failsafe`   \tab `x`          \tab   An error object\eqn{^2}
#'   \cr              `msgERR`   \tab `NULL`       \tab   A character scalar\eqn{^3}
#'   \cr              `notERR`   \tab `TRUE`       \tab   `FALSE`
#'   \cr               `isERR`   \tab `FALSE`      \tab   `TRUE`
#' }
#'     ` `\eqn{^{1.}} `NA` is of mode `'logical'` but is harmlessly coerceable.
#' \cr ` `\eqn{^{2.}} Typically of class `'error'` and/or `'simpleErr'`
#' \cr ` `\eqn{^{3.}} The error message element of the error object produced.
#' @param x An object or a call to evaluate in the environment of a parent function where the initial call was made.
#' @param OR An object to return if evaluating `x` produces an error.
#' @return See table in *description*.
#' @examples
#' failsafe(pi)
#' notERR(pi)
#' msgERR(pi)
#' isERR(pi)
#'
#' failsafe(non.existent.variable)
#' notERR(non.existent.variable)
#' msgERR(non.existent.variable)
#' isERR(non.existent.variable)
#' @export
failsafe <- function(x) {tryCatch(base::identity(x), error = function(e) e, finally = NULL)}

#' @rdname failsafe
#' @export
isERR <- function(x) {assertthat::is.error(try(base::identity(x)))}

#' @rdname failsafe
#' @export
failsafeOR <- function(x, OR = NULL) {uj::f0(isERR(x), OR, x)}

#' @rdname failsafe
#' @export
failsafeBLANK <- function(x) {uj::failsafe(x, "")}

#' @rdname failsafe
#' @export
failsafeNULL <- function(x) {uj::failsafeOR(x)}

#' @rdname failsafe
#' @export
failsafeC0 <- function(x) {uhj::failsafe(x, base::character(0))}

#' @rdname failsafe
#' @export
failsafeI0 <- function(x) {uhj::failsafe(x, base::integer(0))}

#' @rdname failsafe
#' @export
failsafeL0 <- function(x) {uhj::failsafe(x, base::logical(0))}

#' @rdname failsafe
#' @export
failsafeN0 <- function(x) {uhj::failsafe(x, base::numeric(0))}

#' @rdname failsafe
#' @export
failsafeNAC <- function(x) {uj::failsafeOR(x, NA_character_)}

#' @rdname failsafe
#' @export
failsafeNAI <- function(x) {uj::failsafeOR(x, NA_integer_)}

#' @rdname failsafe
#' @export
failsafeNAL <- function(x) {uj::failsafeOR(x, NA)}

#' @rdname failsafe
#' @export
failsafeNAR <- function(x) {uj::failsafeOR(x, NA_real_)}

#' @rdname failsafe
#' @export
failsafeNA <- function(x) {uj::failsafeOR(x, NA)}

#' @rdname failsafe
#' @export
notERR <- function(x) {!isERR(x)}

#' @rdname failsafe
#' @export
msgERR <- function(x) {
  x <- uj::failsafe(x)
  uj::f0(assertthat::is.error(x), x$message, NULL)
}

#' @rdname failsafe
#' @export
makeERR <- function() {base::simpleError("Intentionally-generated error.")}
