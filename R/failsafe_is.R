.pop <- function(x) {length(x) > 0}
.a_scl <- function(x) {is.atomic(x) & length(x) == 1}
.a_pop <- function(x) {is.atomic(x) & length(x) > 0}
.pop_pop <- function(x, y) {length(x) > 0 & length(y) > 0}
.a_pop_pop <- function(x, y) {.a_pop(x) & .a_pop(y)}
.a_pop_scl <- function(x, y) {.a_pop(x) & .a_scl(y)}
.a_pop_dot <- function(x, ...) {.a_pop(x) & length(av(...)) > 0}
.a_scl_dot <- function(x, ...) {.a_scl(x) & length(av(...)) > 0}

#' @name failsafe_is
#' @family failsafe
#' @family logicals
#' @title Failsafe `is` functions
#' @description These functions *always* produce `TRUE` or `FALSE` results unless calling `identity(.)` produce an error.
#' \cr\cr Function names are constructed of prefixes and suffixes, where the suffix specifies what kind of check is conducted and prefix specifies how the check is modified or applied and swept across multiple values.
#' \cr\cr **Type-of-check suffixes** \tabular{rl}{
#'       `NAS`   \tab `NA` scalar.
#'   \cr `OKS`   \tab Non-`NA` scalar.
#'   \cr `OUT`   \tab Atomic scalar `x` is *not* in `...`.\eqn{^1}
#'   \cr  `IN`   \tab Atomic scalar `x` is in `...`.\eqn{^1}
#'   \cr  `EQ`   \tab Calls \code{\link[base:setequal]{setequal(x, y)}}.
#'   \cr  `ID`   \tab Calls \code{\link[base:identical]{identical(x, y)}}.
#'   \cr  `LG`   \tab Scalar logical `TRUE`, `FALSE`, or `NA`.
#'   \cr  `TF`   \tab Scalar `TRUE` or `FALSE`.
#'   \cr  `BL`   \tab Scalar blank (`""`).
#'   \cr   `F`   \tab Scalar `FALSE`.
#'   \cr   `T`   \tab Scalar `TRUE`.
#' }
#'    \eqn{^{1.}} That is, evaluates membership in any \link[=av]{atomized} `...` argument.
#' \cr\cr **Modifying prefixes**:\tabular{rl}{
#'       `not`   \tab Negation.
#'   \cr  `is`   \tab Identity.
#' }
#' **Apply-and-sweep prefixes**\tabular{rl}{
#'       `nor`   \tab No value is `TRUE`.
#'   \cr `one`   \tab 1 value is `TRUE`.
#'   \cr `any`   \tab 1+ value is `TRUE`.
#'   \cr `two`   \tab 2+ values are `TRUE`.
#'   \cr `all`   \tab Every value is `TRUE`.
#' }
#' **Combining prefixes and suffix** \cr\cr All prefixes combine with all suffixes to create function names.
#' @param x,y Any R object.
#' @param ... Objects to check `x` against for functions with the suffix `IN` or `OUT`.
#' @return Scalar `TRUE` or scalar `FALSE`.
#' @examples
#' Blank. <- ""
#' MssScl. <- NA
#' LglScl. <- FALSE
#' FacScl. <- factor("q", levels = c("x", "q"))
#' FacVec. <- factor(c("x", "q"), levels = c("x", "q"))
#' ChrMat. <- matrix(c("a", "b", "c", "NA"), nrow = 2)
#' ChrDtf. <- data.frame(abc = letters[1:3], def = letters[4:6])
#' NumVls. <- list(first3 = c(1:3, NA), next3 = c(4:6, NA))
#' Combo. <- list(MssScl., LglScl., FacVec., ChrMat., ChrDtf., NumVls.)
#'
#' isIN(ChrMat., Blank., Combo.)
#' notIN(ChrMat., Blank., Combo.)
#' norIN(ChrMat., Blank., Combo.)
#' anyIN(ChrMat., Blank., Combo.)
#' allIN(ChrMat., Blank., Combo.)
#' oneIN(ChrMat., Blank., Combo.)
#' twoIN(ChrMat., Blank., Combo.)
#'
#' isOUT(ChrMat., Blank., Combo.)
#' notOUT(ChrMat., Blank., Combo.)
#' norOUT(ChrMat., Blank., Combo.)
#' anyOUT(ChrMat., Blank., Combo.)
#' allOUT(ChrMat., Blank., Combo.)
#' oneOUT(ChrMat., Blank., Combo.)
#' twoOUT(ChrMat., Blank., Combo.)
#'
#' isIN(NULL, Blank., Combo.)
#' notIN(NULL, Blank., Combo.)
#' norIN(NULL, Blank., Combo.)
#' anyIN(NULL, Blank., Combo.)
#' allIN(NULL, Blank., Combo.)
#' oneIN(NULL, Blank., Combo.)
#' twoIN(NULL, Blank., Combo.)
#'
#' isOUT(NULL, Blank., Combo.)
#' notOUT(NULL, Blank., Combo.)
#' norOUT(NULL, Blank., Combo.)
#' anyOUT(NULL, Blank., Combo.)
#' allOUT(NULL, Blank., Combo.)
#' oneOUT(NULL, Blank., Combo.)
#' twoOUT(NULL, Blank., Combo.)
#'
#' isID(LglScl., FALSE)
#' notID(LglScl., FALSE)
#' norID(Combo., ChrDtf.)
#' anyID(Combo., ChrDtf.)
#' allID(Combo., ChrDtf.)
#' oneID(Combo., ChrDtf.)
#' twoID(Combo., ChrDtf.)
#'
#' isEQ(FacVec., Combo.)
#' notEQ(FacVec., Combo.)
#' norEQ(FacVec., Combo.)
#' anyEQ(FacVec., Combo.)
#' allEQ(FacVec., Combo.)
#' oneEQ(FacVec., Combo.)
#' twoEQ(FacVec., Combo.)
#'
#' isT(c(list(Blank.), Combo.))
#' notT(c(list(Blank.), Combo.))
#' norT(c(list(Blank.), Combo.))
#' anyT(c(list(Blank.), Combo.))
#' allT(c(list(Blank.), Combo.))
#' oneT(c(list(Blank.), Combo.))
#' twoT(c(list(Blank.), Combo.))
#'
#' isF(c(list(Blank.), Combo.))
#' notF(c(list(Blank.), Combo.))
#' norF(c(list(Blank.), Combo.))
#' anyF(c(list(Blank.), Combo.))
#' allF(c(list(Blank.), Combo.))
#' oneF(c(list(Blank.), Combo.))
#' twoF(c(list(Blank.), Combo.))
#'
#' isTF(c(list(Blank.), Combo.))
#' notTF(c(list(Blank.), Combo.))
#' norTF(c(list(Blank.), Combo.))
#' anyTF(c(list(Blank.), Combo.))
#' allTF(c(list(Blank.), Combo.))
#' oneTF(c(list(Blank.), Combo.))
#' twoTF(c(list(Blank.), Combo.))
#'
#' isLG(c(list(Blank.), Combo.))
#' notLG(c(list(Blank.), Combo.))
#' norLG(c(list(Blank.), Combo.))
#' anyLG(c(list(Blank.), Combo.))
#' allLG(c(list(Blank.), Combo.))
#' oneLG(c(list(Blank.), Combo.))
#' twoLG(c(list(Blank.), Combo.))
#'
#' isBL(c(list(Blank.), Combo.))
#' notBL(c(list(Blank.), Combo.))
#' norBL(c(list(Blank.), Combo.))
#' anyBL(c(list(Blank.), Combo.))
#' allBL(c(list(Blank.), Combo.))
#' oneBL(c(list(Blank.), Combo.))
#' twoBL(c(list(Blank.), Combo.))
#'
#' isNAS(ChrMat.)
#' notNAS(ChrMat.)
#' norNAS(ChrMat.)
#' anyNAS(ChrMat.)
#' allNAS(ChrMat.)
#' oneNAS(ChrMat.)
#' twoNAS(ChrMat.)
#'
#' isOKS(ChrMat.)
#' notOKS(ChrMat.)
#' norOKS(ChrMat.)
#' anyOKS(ChrMat.)
#' allOKS(ChrMat.)
#' oneOKS(ChrMat.)
#' twoOKS(ChrMat.)
#' @export
isNAS <- function(x) {f0(!.a_scl(x), F, is.na(x))}

#' @rdname failsafe_is
#' @export
isOKS <- function(x) {f0(!.a_scl(x), F, !is.na(x))}

#' @rdname failsafe_is
#' @export
isOUT <- function(x, ...) {
  if (!.a_scl_dot(x, ...)) {return(FALSE)}
  for (i in 1:...length()) {if (is.atomic(...elt(i))) {if (compatible(x, ...elt(i))) {if (x %in% av(...elt(i))) {return(FALSE)}}}}
  TRUE
}

#' @rdname failsafe_is
#' @export
isIN <- function(x, ...) {
  if (!.a_scl_dot(x, ...)) {return(FALSE)}
  for (i in 1:...length()) {if (is.atomic(...elt(i))) {if (compatible(x, ...elt(i))) {if (x %in% av(...elt(i))) {return(TRUE)}}}}
  FALSE
}

#' @rdname failsafe_is
#' @export
isID <- function(x, y) {identical(x, y)}

#' @rdname failsafe_is
#' @export
isEQ <- function(x, y) {f0(!.a_pop_dot(x, y), FALSE, tryCatch(setequal(x, y), error = function(e) F, finally = NULL))}

#' @rdname failsafe_is
#' @export
isT <- function(x) {isTRUE(x)}

#' @rdname failsafe_is
#' @export
isF <- function(x) {isFALSE(x)}

#' @rdname failsafe_is
#' @export
isTF <- function(x) {isTRUE(x) | isFALSE(x)}

#' @rdname failsafe_is
#' @export
isLG <- function(x) {isTRUE(x) | isFALSE(x) | isNAS(x)}

#' @rdname failsafe_is
#' @export
isBL <- function(x) {f0(!.a_scl(x), F, isEQ(x, ""))}

#' @rdname failsafe_is
#' @export
notIN <- function(x, ...) {!isIN(x, ...)}

#' @rdname failsafe_is
#' @export
notOUT <- function(x, ...) {!isOUT(x, ...)}

#' @rdname failsafe_is
#' @export
notID <- function(x, y) {!isID(x, y)}

#' @rdname failsafe_is
#' @export
notEQ <- function(x, y) {!isEQ(x, y)}

#' @rdname failsafe_is
#' @export
notTF <- function(x) {!isTF(x)}

#' @rdname failsafe_is
#' @export
notLG <- function(x) {!isLG(x)}

#' @rdname failsafe_is
#' @export
notT <- function(x) {!isT(x)}

#' @rdname failsafe_is
#' @export
notF <- function(x) {!isF(x)}

#' @rdname failsafe_is
#' @export
notNAS <- function(x) {!isNAS(x)}

#' @rdname failsafe_is
#' @export
notOKS <- function(x) {!isOKS(x)}

#' @rdname failsafe_is
#' @export
notBL <- function(x) {!isBL(x)}

#' @rdname failsafe_is
#' @export
norIN <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, !any(sapply(x, isIN, ...)))}

#' @rdname failsafe_is
#' @export
norOUT <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, !any(sapply(x, isOUT, ...)))}

#' @rdname failsafe_is
#' @export
norID <- function(x, y) {f0(!.pop_pop(x, y), F, !any(sapply(x, isID, y = y)))}

#' @rdname failsafe_is
#' @export
norEQ <- function(x, y) {f0(!.pop_pop(x, y), F, !any(sapply(x, isEQ, y = y)))}

#' @rdname failsafe_is
#' @export
norT <- function(x) {f0(!.pop(x), T, !any(sapply(x, isTRUE)))}

#' @rdname failsafe_is
#' @export
norF <- function(x) {f0(!.pop(x), T, !any(sapply(x, isFALSE)))}

#' @rdname failsafe_is
#' @export
norNAS <- function(x) {f0(!.pop(x), T, !any(sapply(x, isNAS)))}

#' @rdname failsafe_is
#' @export
norOKS <- function(x) {f0(!.pop(x), T, !any(sapply(x, isNAS)))}

#' @rdname failsafe_is
#' @export
norTF <- function(x) {f0(!.pop(x), T, !any(sapply(x, isTF)))}

#' @rdname failsafe_is
#' @export
norLG <- function(x) {f0(!.pop(x), T, !any(sapply(x, isLG)))}

#' @rdname failsafe_is
#' @export
norBL <- function(x) {f0(!.pop(x), T, !any(sapply(x, isBL)))}

#' @rdname failsafe_is
#' @export
anyIN <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, any(sapply(x, isIN, ...)))}

#' @rdname failsafe_is
#' @export
anyOUT <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, any(sapply(x, isOUT, ...)))}

#' @rdname failsafe_is
#' @export
anyID <- function(x, y) {f0(!.pop_pop(x, y), F, any(sapply(x, isID, y = y)))}

#' @rdname failsafe_is
#' @export
anyEQ <- function(x, y) {f0(!.pop_pop(x, y), F, any(sapply(x, isEQ, y = y)))}

#' @rdname failsafe_is
#' @export
anyT <- function(x) {f0(!.pop(x), F, any(sapply(x, isTRUE)))}

#' @rdname failsafe_is
#' @export
anyF <- function(x) {f0(!.pop(x), F, any(sapply(x, isFALSE)))}

#' @rdname failsafe_is
#' @export
anyNAS <- function(x) {f0(!.pop(x), F, any(sapply(x, isNAS)))}

#' @rdname failsafe_is
#' @export
anyOKS <- function(x) {f0(!.pop(x), F, any(sapply(x, isNAS)))}

#' @rdname failsafe_is
#' @export
anyTF <- function(x) {f0(!.pop(x), F, any(sapply(x, isTF)))}

#' @rdname failsafe_is
#' @export
anyLG <- function(x) {f0(!.pop(x), F, any(sapply(x, isLG)))}

#' @rdname failsafe_is
#' @export
anyBL <- function(x) {f0(!.pop(x), F, any(sapply(x, isBL)))}

#' @rdname failsafe_is
#' @export
allIN <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, all(sapply(x, isIN, ...)))}

#' @rdname failsafe_is
#' @export
allOUT <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, all(sapply(x, isOUT, ...)))}

#' @rdname failsafe_is
#' @export
allID <- function(x, y) {f0(!.pop_pop(x, y), F, all(sapply(x, isID, y = y)))}

#' @rdname failsafe_is
#' @export
allEQ <- function(x, y) {f0(!.pop_pop(x, y), F, all(sapply(x, isEQ, y = y)))}

#' @rdname failsafe_is
#' @export
allT <- function(x) {f0(!.pop(x), F, all(sapply(x, isTRUE)))}

#' @rdname failsafe_is
#' @export
allF <- function(x) {f0(!.pop(x), F, all(sapply(x, isFALSE)))}

#' @rdname failsafe_is
#' @export
allNAS <- function(x) {f0(!.pop(x), F, all(sapply(x, isNAS)))}

#' @rdname failsafe_is
#' @export
allOKS <- function(x) {f0(!.pop(x), F, all(sapply(x, isNAS)))}

#' @rdname failsafe_is
#' @export
allTF <- function(x) {f0(!.pop(x), F, all(sapply(x, isTF)))}

#' @rdname failsafe_is
#' @export
allLG <- function(x) {f0(!.pop(x), T, all(sapply(x, isLG)))}

#' @rdname failsafe_is
#' @export
allBL <- function(x) {f0(!.pop(x), T, all(sapply(x, isBL)))}

#' @rdname failsafe_is
#' @export
oneIN <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, length(which(sapply(x, isIN, ...))) == 1)}

#' @rdname failsafe_is
#' @export
oneOUT <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, length(which(sapply(x, isOUT, ...))) == 1)}

#' @rdname failsafe_is
#' @export
oneID <- function(x, y) {f0(!.pop_pop(x, y), F, length(which(sapply(x, isID, y = y))) == 1)}

#' @rdname failsafe_is
#' @export
oneEQ <- function(x, y) {f0(!.pop_pop(x, y), F, length(which(sapply(x, isEQ, y = y))) == 1)}

#' @rdname failsafe_is
#' @export
oneT <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isTRUE))) == 1)}

#' @rdname failsafe_is
#' @export
oneF <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isFALSE))) == 1)}

#' @rdname failsafe_is
#' @export
oneNAS <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isNAS))) == 1)}

#' @rdname failsafe_is
#' @export
oneOKS <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isOKS))) == 1)}

#' @rdname failsafe_is
#' @export
oneTF <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isTF))) == 1)}

#' @rdname failsafe_is
#' @export
oneLG <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isLG))) == 1)}

#' @rdname failsafe_is
#' @export
oneBL <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isBL))) == 1)}

#' @rdname failsafe_is
#' @export
twoIN <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, length(which(sapply(x, isIN, ...))) > 1)}

#' @rdname failsafe_is
#' @export
twoOUT <- function(x, ...) {f0(!.a_pop_dot(x, ...), F, length(which(sapply(x, isOUT, ...))) > 1)}

#' @rdname failsafe_is
#' @export
twoID <- function(x, y) {f0(!.pop_pop(x, y), F, length(which(sapply(x, isID, y = y))) > 1)}

#' @rdname failsafe_is
#' @export
twoEQ <- function(x, y) {f0(!.pop_pop(x, y), F, length(which(sapply(x, isEQ, y = y))) > 1)}

#' @rdname failsafe_is
#' @export
twoT <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isTRUE))) > 1)}

#' @rdname failsafe_is
#' @export
twoF <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isFALSE))) > 1)}

#' @rdname failsafe_is
#' @export
twoNAS <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isNAS))) > 1)}

#' @rdname failsafe_is
#' @export
twoOKS <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isOKS))) > 1)}

#' @rdname failsafe_is
#' @export
twoTF <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isTF))) > 1)}

#' @rdname failsafe_is
#' @export
twoLG <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isLG))) > 1)}

#' @rdname failsafe_is
#' @export
twoBL <- function(x) {f0(!.pop(x), F, length(which(sapply(x, isBL))) > 1)}
