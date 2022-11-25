#' @name failsafe
#' @family extensions
#' @family errs
#' @family failsafe
#' @title Evaluate Objects, Flag Any Errors
#' @description \tabular{ll}{
#'   FUNCTION     \tab WHAT IT DOES \cr
#'   `failsafe`   \tab Call `identity(x)`. If an error is produced in doing so,
#'                     returns an object of class `'error'` or of class
#'                     `'simpleError'`. If an error is not generated, returns
#'                     the safely evaluated value of `x`.                    \cr
#'   `isERR`      \tab Evaluate whether calling `identity(x)` produced an
#'                     error.                                                \cr
#'   `notERR`     \tab Evaluate whether calling `identity(x)` \emph{did not}
#'                     produce an error.                                     \cr
#'   `msgERR`     \tab Get any error message associated with calling
#'                     `identity(x)`. If there is none, returns `NULL`.        }
#' @param x An object or a call to evaluate in the environment of a parent
#'   function where the initial call was made.
#' @return \tabular{ll}{
#'   FUNCTIONS           \tab RETURN VALUE                                   \cr
#'   `failsafe`          \tab Either `x`, an object of class `'error'`, or an
#'                            object of class `'simpleError'`.               \cr
#'   `isERR`, `notERR`   \tab A logical scalar.                              \cr
#'   `msgERR`            \tab Either `NULL` or a character scalar.             }
#' @export
failsafe <- function(x) {tryCatch(identity(x), error = function(e) e, finally = NULL)}

#' @rdname failsafe
#' @export
isERR <- function(x) {any(class(failsafe(x)) %in% c("error", "simpleError"))}

#' @rdname failsafe
#' @export
notERR <- function(x) {!any(class(failsafe(x)) %in% c("error", "simpleError"))}

#' @rdname failsafe
#' @export
msgERR <- function(x) {f0(isERR(x), x$message, NULL)}
