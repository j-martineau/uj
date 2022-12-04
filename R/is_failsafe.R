#' @name is_failsafe
#' @family failsafe
#' @family failsafe
#' @family logicals
#' @title Failsafe `is` functions
#' @description These functions **always** produce a `TRUE` or `FALSE` result unless identity-evaluating arguments (e.g., `identity(x)`) produce an error.
#' \cr\cr
#' Function names are constructed of prefixes and suffixes, where the suffix specifies what kind of check is conducted and the prefix specifies how the check is modified or applying the check to multiple values and sweeping across the results.
#' \cr\cr
#' **Available suffixes** check for the following:
#' \itemize{
#'   \item **`IN`**: checks whether `x` is in the collective set of atomic values contained by all `...` arguments.
#'   \item **`ID`**: checks whether `x` is identical to `y`.
#'   \item **`EQ`**: checks whether `x` is set-equal to `y`
#'   \item **`T`** : checks for scalar `TRUE`.
#'   \item **`F`** : checks for scalar `FALSE`.
#'   \item **`NA`**: checks for scalar `NA`.
#'   \item **`Ok`**: checks for scalar non-`NA`.
#'   \item **`TF`**: checks for scalar `TRUE` or `FALSE`.
#'   \item **`LG`**: checks for scalar logical, including `NA`.
#'   \item **`BL`**: checks for scalar `""` (blank string).
#' }
#' **Available prefixes** for modifying the result are
#' \itemize{
#'   \item **`is`**: identity.
#'   \item **`not`**: negation.
#' }
#' **Available prefixes** for applying to multiple values and sweeping across the results are
#' \itemize{
#'   \item **`nor`**: No resulting value is `TRUE`.
#'   \item **`any`**: Any resulting value is `TRUE`.
#'   \item **`all`**: Every resulting value is `TRUE`.
#'   \item **`one`**: Exactly 1 resulting value is `TRUE`.
#'   \item **`two`**: 2+ resulting values are `TRUE`.
#' }
#' **Combining prefixes and suffix**: All prefixes combine with all suffixes to create function names.
#' @param x,y Any R object.
#' @param ... Objects to check `x` against for functions with the suffix `IN`.
#' @return `TRUE` or `FALSE`.
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
  for (i. in 1:...length()) {
    y <- ...elt(i.)
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
notT <- function(x) {!isT(x)}

#' @rdname is_failsafe
#' @export
notF <- function(x) {!isF(x)}

#' @rdname is_failsafe
#' @export
notNa <- function(x) {!isNa(x)}

#' @rdname is_failsafe
#' @export
notOk <- function(x) {!isOk(x)}

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
