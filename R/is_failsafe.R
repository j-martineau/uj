#' @name is_failsafe
#' @family extensions
#' @family failsafe
#' @family logicals
#' @title Failsafe `is` Functions
#' @description Functions that produce a `TRUE` or `FALSE` result as long as
#'   identity-evaluating their arguments (e.g., `identity(x)`) does not itself
#'   produce an error.
#' @param x,y Any R object.
#' @param ... Objects to check for containing `x`.
#' @return `TRUE` or `FALSE`.
#' @export
is_failsafe <- NULL

#' @describeIn is_failsafe Is `x` in the collective set of atomized values
#'   from \code{...}?
#' @export
isIN <- function(x, ...) {
  if (...length() == 0 | !is.atomic(x) | length(x) != 1) {return(F)}
  for (i in 1:...length()) {
    y <- ...elt(i)
    if (is.atomic(y)) {if (compatible(x, y)) {if (x %in% y) {return(T)}}}
  }
  return(F)
}

#' @describeIn is_failsafe Are `x` and `y` \link[base]{identical}?
#' @export
isID <- function(x, y) {identical(x, y)}

#' @describeIn is_failsafe Are `x` and `y` atomic and \link[base]{setequal}?
#' @export
isEQ <- function(x, y) {f0(!is.atomic(x) | !is.atomic(y), FALSE, tryCatch(setequal(x, y), error = function(e) F, finally = NULL))}

#' @describeIn is_failsafe Is `x` scalar `TRUE`?
#' @export
isT <- function(x) {isTRUE(x)}

#' @describeIn is_failsafe Is `x` scalar `FALSE`?
#' @export
isF <- function(x) {isFALSE(x)}

#' @describeIn is_failsafe Is `x` scalar `NA`?
#' @export
isNa <- function(x) {f0(length(x) != 1, F, f0(!is.atomic(x), F, is.na(x)))}

#' @describeIn is_failsafe Is `x` atomic, scalar, and non-`NA`?
#' @export
isOk <- function(x) {f0(length(x) != 1, F, f0(!is.atomic(x), F, !is.na(x)))}

#' @describeIn is_failsafe Is `x` either scalar `TRUE` or scalar
#'   `FALSE`?
#' @export
isTF <- function(x) {isTRUE(x) | isFALSE(x)}

#' @describeIn is_failsafe Is `x` scalar logical (`TRUE`,
#'   `FALSE` or `NA`)?
#' @export
isLG <- function(x) {isTF(x) | isNa(x)}

#' @describeIn is_failsafe Is `x` a scalar blank string?
#' @export
isBL <- function(x) {isEQ(x, "")}

#' @describeIn is_failsafe Is `x` atomic, of length `1`, and not contained
#'   in any argument in \code{...}?
#' @export
notIN <- function(x, ...) {
  if (!is.atomic(x) | length(x) != 1 | ...length() == 0) {return(T)}
  for (i. in 1:...length()) {
    y <- ...elt(i.)
    if (is.atomic(y)) {if (compatible(x, y)) {if (x %in% y) {return(F)}}}
  }
  return(T)
}

#' @describeIn is_failsafe Thin wrapper for `!identical`.
#' @export
notID <- function(x, y) {!identical(x, y)}

#' @describeIn is_failsafe Is `x` not atomic, `y` not atomic, or
#'   are they not setequal?
#' @export
notEQ <- function(x, y) {f0(!is.atomic(x) | !is.atomic(y), F, tryCatch(!setequal(x, y), error = function(e) T, finally = NULL))}

#' @describeIn is_failsafe Is `x` not a scalar blank string?
#' @export
notBL <- function(x) {notIN(x, "")}

#' @describeIn is_failsafe Are no elements of `x` in the collective
#'   atomized values from \code{...}?
#' @export
norIN <- function(x, ...) {!any(sapply(x, isIN, ...))}

#' @describeIn is_failsafe Are no elements of `x` identical to `y`?
#' @export
norID <- function(x, y) {!any(sapply(x, isID, y))}

#' @describeIn is_failsafe Are no elements of `x` identical to `y`?
#' @export
norEQ <- function(x, y) {!any(sapply(x, isEQ, y))}

#' @describeIn is_failsafe Are no elements of `x` scalar `TRUE`?
#' @export
norT <- function(x) {!any(sapply(x, isT))}

#' @describeIn is_failsafe Are no elements of `x` scalar `FALSE`?
#' @export
norF <- function(x) {!any(sapply(x, isF))}

#' @describeIn is_failsafe Are no elements of `x` scalar `NA`?
#' @export
norNa <- function(x) {!any(sapply(x, isNa))}

#' @describeIn is_failsafe Are no elements of `x` scalar non-`NA`?
#' @export
norOk <- function(x) {!any(sapply(x, isOk))}

#' @describeIn is_failsafe Are no elements of `x` either scalar
#'   `TRUE` or scalar `FALSE`?
#' @export
norTF <- function(x) {!any(sapply(x, isTF))}

#' @describeIn is_failsafe Are no elements of `x` scalar logical
#'   (`NA`, `TRUE`, or `FALSE`)?
#' @export
norLG <- function(x) {!any(sapply(x, isLG))}

#' @describeIn is_failsafe Are no elements of `x` blank string scalars?
#' @export
norBL <- function(x) {!any(sapply(x, isBL))}

#' @describeIn is_failsafe Are any elements of `x` in the collective
#'   atomized values from \code{...}?
#' @export
anyIN <- function(x, ...) {any(sapply(x, isIN, ...))}

#' @describeIn is_failsafe Are any elements of `x` identical to
#'   `y`?
#' @export
anyID <- function(x, y) {any(sapply(x, isID, y))}

#' @describeIn is_failsafe Are any elements of `x` set-equal to
#'   `y`?
#' @export
anyEQ <- function(x, y) {any(sapply(x, isEQ, y))}

#' @describeIn is_failsafe Are any elements of `x` scalar `TRUE`?
#' @export
anyT <- function(x) {any(sapply(x, isT))}

#' @describeIn is_failsafe Are any elements of `x` scalar `FALSE`?
#' @export
anyF <- function(x) {any(sapply(x, isF))}

#' @describeIn is_failsafe Are any elements of `x` scalar, atomic, and
#'   `NA`?
#' @export
anyNa <- function(x) {any(sapply(x, isNa))}

#' @describeIn is_failsafe Are any elements of `x` scalar, atomic, and
#'   non-`NA`?
#' @export
anyOk <- function(x) {any(sapply(x, isOk))}

#' @describeIn is_failsafe Are no elements of `x` scalar `TRUE` or
#'   scalar `FALSE`?
#' @export
anyTF <- function(x) {any(sapply(x, isTF))}

#' @describeIn is_failsafe Are any elements of `x` scalar logical
#'   (`TRUE`, `FALSE`, or `NA`)?
#' @export
anyLG <- function(x) {any(sapply(x, isLG))}

#' @describeIn is_failsafe Are any elements of `x` scalar blank strings?
#' @export
anyBL <- function(x) {any(sapply(x, isBL))}

#' @describeIn is_failsafe Are all elements of `x` in the collective set
#'   of atomized values from \code{...}?
#' @export
allIN <- function(x, ...) {all(sapply(x, isIN, ...))}

#' @describeIn is_failsafe Are all elements of `x` identical to
#'   `y`?
#' @export
allID <- function(x, y) {all(sapply(x, isID, y))}

#' @describeIn is_failsafe Are all elements of `x` set-equal to
#'   `y`?
#' @export
allEQ <- function(x, y) {all(sapply(x, isEQ, y))}

#' @describeIn is_failsafe Are all elements of `x` scalar `TRUE`?
#' @export
allT <- function(x) {all(sapply(x, isT))}

#' @describeIn is_failsafe Are all elements of `x` scalar `FALSE`?
#' @export
allF <- function(x) {all(sapply(x, isF))}

#' @describeIn is_failsafe Are all elements of `x` scalar `NA`?
#' @export
allNa <- function(x) {all(sapply(x, isNa))}

#' @describeIn is_failsafe Are all elements of `x` scalar, atomic, and
#'   non-`NA`?
#' @export
allOk <- function(x) {all(sapply(x, isOk))}

#' @describeIn is_failsafe Are all elements of `x` scalar `TRUE`
#'   or scalar `FALSE`?
#' @export
allTF <- function(x) {all(sapply(x, isTF))}

#' @describeIn is_failsafe Are all elements of `x` scalar logical
#'   (`TRUE`, `FALSE`, or `NA`)?
#' @export
allLG <- function(x) {all(sapply(x, isLG))}

#' @describeIn is_failsafe Are all elements of `x` scalar blank strings?
#' @export
allBL <- function(x) {all(sapply(x, isBL))}

#' @describeIn is_failsafe Is exactly one element of `x` in the
#'   collective set of atomized values from \code{...}?
#' @export
oneIN <- function(x, ...) {length(which(sapply(x, isIN, ...))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` identical to
#'   `y`?
#' @export
oneID <- function(x, y) {length(which(sapply(x, isID, y))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` set-equal to
#'   `y`?
#' @export
oneEQ <- function(x, y) {length(which(sapply(x, isEQ, y))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` scalar
#'   `TRUE`?
#' @export
oneT <- function(x) {length(which(sapply(x, isT))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` scalar
#'   `FALSE`?
#' @export
oneF <- function(x) {length(which(sapply(x, isF))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` scalar
#'   `NA`?
#' @export
oneNa <- function(x) {length(which(sapply(x, isNa))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` scalar, atomic,
#'   and non-`NA`?
#' @export
oneOk <- function(x) {length(which(sapply(x, isOk))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` scalar
#'   `TRUE` or scalar `FALSE`?
#' @export
oneTF <- function(x) {length(which(sapply(x, isTF))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` scalar logical
#'   (`TRUE`, `FALSE`, or `NA`)?
#' @export
oneLG <- function(x) {length(which(sapply(x, isLG))) == 1}

#' @describeIn is_failsafe Is exactly one element of `x` a scalar blank
#'   string?
#' @export
oneBL <- function(x) {length(which(sapply(x, isBL))) == 1}

#' @describeIn is_failsafe Are two or more elements of `x` in the
#'   collective set of atomized values from \code{...}?
#' @export
twoIN <- function(x, ...) {length(which(sapply(x, isIN, ...))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` identical to
#'   `y`?
#' @export
twoID <- function(x, y) {length(which(sapply(x, isID, y))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` set-equal to
#'   `y`?
#' @export
twoEQ <- function(x, y) {length(which(sapply(x, isEQ, y))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` scalar
#'   `TRUE`?
#' @export
twoT <- function(x) {all(sapply(x, isT))}

#' @describeIn is_failsafe Are two or more elements of `x` scalar
#'   `FALSE`?
#' @export
twoF <- function(x) {length(which(sapply(x, isF))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` scalar
#'   `NA`?
#' @export
twoNa <- function(x) {length(which(sapply(x, isNa))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` scalar,
#'   atomic, and non-`NA`?
#' @export
twoOk <- function(x) {length(which(sapply(x, isOk))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` scalar
#'   `TRUE` or scalar `FALSE`?
#' @export
twoTF <- function(x) {length(which(sapply(x, isTF))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` scalar logical
#'   (`TRUE`, `FALSE` or `NA`)?
#' @export
twoLG <- function(x) {length(which(sapply(x, isLG))) >= 2}

#' @describeIn is_failsafe Are two or more elements of `x` scalar blank
#'   strings?
#' @export
twoBL <- function(x) {length(which(sapply(x, isBL))) >= 2}
