#' @name is_failsafe
#' @family failsafe
#' @family is_functions
#' @title Failsafe \code{is} functions
#' @description Functions that produce a \code{TRUE} or \code{FALSE} result as
#'   long as identity-evaluating their arguments (i.e.,
#'   \code{identity(arg_name)}) does not itself produce an error.
#' @param x,y any object.
#' @param ... objects to check for containing \code{x}.
#' @details \strong{\code{isIN, notIN}}
#'   \cr The first determines whether \code{x} is an atomic scalar and if so,
#'   whether \code{y} is a non-empty atomic object, and if so, whether \code{x}
#'   is contained in any argument in \code{...}. Arguments in \code{...} are
#'   evaluated left to right, and does not evaluate returning \code{TRUE} as
#'   soon as it finds an argument containing \code{x}. Returns \code{FALSE} in
#'   the following special cases: \code{x} is not atomic, \code{x} is not of
#'   length \code{1}, a non-atomic argument is encountered in \code{...}, or an
#'   error is generated in evaluating \code{x} or any argument in \code{...}.
#'   The second returns the opposite of the first.
#'   \cr\cr
#'   \strong{\code{norIN, anyIN, allIN, oneIN, twoIN}}
#'   \cr These functions call \code{isIN(x, ...)} for every element of \code{x}
#'   and evaluate, respectively, whether \code{0}, \code{1} or more, all,
#'   exactly \code{1}, and \code{2} or more of the results were \code{TRUE}.
#'   \cr\cr
#'   \strong{\code{isID, notID}}
#'   \cr Thin wrappers for \code{identical} and \code{!identical}, respectively.
#'   \cr\cr
#'   \strong{\code{isEQ, notEQ}}
#'   \cr Thin wrappers for \code{setequal} and \code{!setequal}, respectively,
#'   but for atomic arguments only, meaning that if \code{x} and/or \code{y} are
#'   not atomic, they are considered not set equal.
#'   \cr\cr
#'   \strong{\code{isT, isF, isTF}}
#'   \cr Thin wrappers for \code{isTRUE}, \code{isFALSE}, and \code{(isTRUE |
#'   isFALSE)}, respectively, evaluating whether \code{x} is an atomic scalar
#'   \code{TRUE}, an atomic scalar \code{FALSE}, or either, respectively.
#'   \cr\cr
#'   \strong{\code{isNa, isOk}}
#'   \cr Evaluates, respectively, whether or not
#'   \code{x} is an atomic scalar \code{NA} (includes \code{NA_character_},
#'   \code{NA_complex_}, \code{NA_integer_}, and \code{NA_real_}).
#'   \cr\cr
#'   \strong{\code{isLG}}
#'   \cr Evaluates whether \code{x} is an effectively logical scalar (i.e.,
#'   \code{isT | isF | isNa}, because \code{NA} is coerceable to any
#'   atomic mode).
#'   \cr\cr
#'   \strong{\code{isBL, notBL}}
#'   \cr Evaluates whether \code{x} is a character scalar, and if so, whether
#'   it is a blank string, or \code{""}.
#'   \cr\cr
#'   \strong{\code{norX, norXX}}
#'   \cr Calls \code{isX(x)} or \code{isXX(x, y)} for each element of \code{x}
#'   and returns \code{TRUE} if the number of \code{TRUE} values returned
#'   is \code{0}.
#'   \cr\cr
#'   \strong{\code{anyX, anyXX}}
#'   \cr Calls \code{isX(x)} or \code{isXX(x, y)} for each element of \code{x}
#'   and returns \code{TRUE} if any \code{TRUE} values were returned.
#'   \cr\cr
#'   \strong{\code{allX, allXX}}
#'   \cr Calls \code{isX(x)} or \code{isXX(x, y)} for each element of \code{x}
#'   and returns \code{TRUE} if the number of \code{TRUE} values returned
#'   is positive and equal to the length of \code{x}.
#'   \cr\cr
#'   \strong{\code{oneX, oneXX}}
#'   \cr Calls \code{isX(x)} or \code{isXX(x, y)} for each element of \code{x}
#'   and returns \code{TRUE} if the number of \code{TRUE} values returned
#'   is \code{1}.
#'   \cr\cr
#'   \strong{\code{twoX, twoXX}}
#'   \cr Calls \code{isX(x)} or \code{isXX(x, y)} for each element of \code{x}
#'   and returns \code{TRUE} if the number of \code{TRUE} values returned
#'   is \code{2} or greater..
#' @return \code{TRUE} or \code{FALSE}.
#' @export
isIN <- function(x, ...) {
  if (...length() == 0 | !is.atomic(x) | length(x) != 1) {return(F)}
  for (i in 1:...length()) {
    y <- ...elt(i)
    if (is.atomic(y)) {if (compatible(x, y)) {if (x %in% y) {return(T)}}}
  }
  return(F)
}

#' @rdname is_failsafe
#' @export
isID <- function(x, y) {identical(x, y)}

#' @rdname is_failsafe
#' @export
isEQ <- function(x, y) {f0(!is.atomic(x) | !is.atomic(y), FALSE, tryCatch(setequal(x, y), error = function(e) F, finally = NULL))}

#' @rdname is_failsafe
#' @export
isT <- function(x) {isTRUE(x)}

#' @rdname is_failsafe
#' @export
isF <- function(x) {isFALSE(x)}

#' @rdname is_failsafe
#' @export
isNa <- function(x) {f0(length(x) != 1, F, f0(!is.atomic(x), F, is.na(x)))}

#' @rdname is_failsafe
#' @export
isOk <- function(x) {f0(length(x) != 1, F, f0(!is.atomic(x), F, !is.na(x)))}

#' @rdname is_failsafe
#' @export
isTF <- function(x) {isTRUE(x) | isFALSE(x)}

#' @rdname is_failsafe
#' @export
isLG <- function(x) {isTF(x) | isNa(x)}

#' @rdname is_failsafe
#' @export
isBL <- function(x) {isEQ(x, "")}

#' @rdname is_failsafe
#' @export
notIN <- function(x, ...) {
  if (!is.atomic(x) | length(x) != 1 | ...length() == 0) {return(T)}
  for (i in 1:...length()) {
    y <- ...elt(i)
    if (is.atomic(y)) {if (compatible(x, y)) {if (x %in% y) {return(F)}}}
  }
  return(T)
}

#' @rdname is_failsafe
#' @export
notID <- function(x, y) {!identical(x, y)}

#' @rdname is_failsafe
#' @export
notEQ <- function(x, y) {f0(!is.atomic(x) | !is.atomic(y), F, tryCatch(!setequal(x, y), error = function(e) T, finally = NULL))}

#' @rdname is_failsafe
#' @export
notBL <- function(x) {notIN(x, "")}

#' @rdname is_failsafe
#' @export
norIN <- function(x, ...) {!any(sapply(x, isIN, ...))}

#' @rdname is_failsafe
#' @export
norID <- function(x, y) {!any(sapply(x, isID, y))}

#' @rdname is_failsafe
#' @export
norEQ <- function(x, y) {!any(sapply(x, isEQ, y))}

#' @rdname is_failsafe
#' @export
norT <- function(x) {!any(sapply(x, isT))}

#' @rdname is_failsafe
#' @export
norF <- function(x) {!any(sapply(x, isF))}

#' @rdname is_failsafe
#' @export
norNa <- function(x) {!any(sapply(x, isNa))}

#' @rdname is_failsafe
#' @export
norOk <- function(x) {!any(sapply(x, isOk))}

#' @rdname is_failsafe
#' @export
norTF <- function(x) {!any(sapply(x, isTF))}

#' @rdname is_failsafe
#' @export
norLG <- function(x) {!any(sapply(x, isLG))}

#' @rdname is_failsafe
#' @export
norBL <- function(x) {!any(sapply(x, isBL))}

#' @rdname is_failsafe
#' @export
anyIN <- function(x, ...) {any(sapply(x, isIN, ...))}

#' @rdname is_failsafe
#' @export
anyID <- function(x, y) {any(sapply(x, isID, y))}

#' @rdname is_failsafe
#' @export
anyEQ <- function(x, y) {any(sapply(x, isEQ, y))}

#' @rdname is_failsafe
#' @export
anyT <- function(x) {any(sapply(x, isT))}

#' @rdname is_failsafe
#' @export
anyF <- function(x) {any(sapply(x, isF))}

#' @rdname is_failsafe
#' @export
anyNa <- function(x) {any(sapply(x, isNa))}

#' @rdname is_failsafe
#' @export
anyOk <- function(x) {any(sapply(x, isOk))}

#' @rdname is_failsafe
#' @export
anyTF <- function(x) {any(sapply(x, isTF))}

#' @rdname is_failsafe
#' @export
anyLG <- function(x) {any(sapply(x, isLG))}

#' @rdname is_failsafe
#' @export
anyBL <- function(x) {any(sapply(x, isBL))}

#' @rdname is_failsafe
#' @export
allIN <- function(x, ...) {all(sapply(x, isIN, ...))}

#' @rdname is_failsafe
#' @export
allID <- function(x, y) {all(sapply(x, isID, y))}

#' @rdname is_failsafe
#' @export
allEQ <- function(x, y) {all(sapply(x, isEQ, y))}

#' @rdname is_failsafe
#' @export
allT <- function(x) {all(sapply(x, isT))}

#' @rdname is_failsafe
#' @export
allF <- function(x) {all(sapply(x, isF))}

#' @rdname is_failsafe
#' @export
allNa <- function(x) {all(sapply(x, isNa))}

#' @rdname is_failsafe
#' @export
allOk <- function(x) {all(sapply(x, isOk))}

#' @rdname is_failsafe
#' @export
allTF <- function(x) {all(sapply(x, isTF))}

#' @rdname is_failsafe
#' @export
allLG <- function(x) {all(sapply(x, isLG))}

#' @rdname is_failsafe
#' @export
allBL <- function(x) {all(sapply(x, isBL))}

#' @rdname is_failsafe
#' @export
oneIN <- function(x, ...) {length(which(sapply(x, isIN, ...))) == 1}

#' @rdname is_failsafe
#' @export
oneID <- function(x, y) {length(which(sapply(x, isID, y))) == 1}

#' @rdname is_failsafe
#' @export
oneEQ <- function(x, y) {length(which(sapply(x, isEQ, y))) == 1}

#' @rdname is_failsafe
#' @export
oneT <- function(x) {length(which(sapply(x, isT))) == 1}

#' @rdname is_failsafe
#' @export
oneF <- function(x) {length(which(sapply(x, isF))) == 1}

#' @rdname is_failsafe
#' @export
oneNa <- function(x) {length(which(sapply(x, isNa))) == 1}

#' @rdname is_failsafe
#' @export
oneOk <- function(x) {length(which(sapply(x, isOk))) == 1}

#' @rdname is_failsafe
#' @export
oneTF <- function(x) {length(which(sapply(x, isTF))) == 1}

#' @rdname is_failsafe
#' @export
oneLG <- function(x) {length(which(sapply(x, isLG))) == 1}

#' @rdname is_failsafe
#' @export
oneBL <- function(x) {length(which(sapply(x, isBL))) == 1}

#' @rdname is_failsafe
#' @export
twoIN <- function(x, ...) {length(which(sapply(x, isIN, ...))) >= 2}

#' @rdname is_failsafe
#' @export
twoID <- function(x, y) {length(which(sapply(x, isID, y))) >= 2}

#' @rdname is_failsafe
#' @export
twoEQ <- function(x, y) {length(which(sapply(x, isEQ, y))) >= 2}

#' @rdname is_failsafe
#' @export
twoT <- function(x) {all(sapply(x, isT))}

#' @rdname is_failsafe
#' @export
twoF <- function(x) {length(which(sapply(x, isF))) >= 2}

#' @rdname is_failsafe
#' @export
twoNa <- function(x) {length(which(sapply(x, isNa))) >= 2}

#' @rdname is_failsafe
#' @export
twoOk <- function(x) {length(which(sapply(x, isOk))) >= 2}

#' @rdname is_failsafe
#' @export
twoTF <- function(x) {length(which(sapply(x, isTF))) >= 2}

#' @rdname is_failsafe
#' @export
twoLG <- function(x) {length(which(sapply(x, isLG))) >= 2}

#' @rdname is_failsafe
#' @export
twoBL <- function(x) {length(which(sapply(x, isBL))) >= 2}
