.pop <- function(x) {base::length(x) > 0}
.a_scl <- function(x) {base::is.atomic(x) & base::length(x) == 1}
.a_pop <- function(x) {base::is.atomic(x) & base::length(x) > 0}
.pop_pop <- function(x, y) {base::length(x) > 0 & base::length(y) > 0}
.a_pop_pop <- function(x, y) {uj:::.a_pop(x) & uj:::.a_pop(y)}
.a_pop_scl <- function(x, y) {uj:::.a_pop(x) & uj:::.a_scl(y)}
.a_pop_dot <- function(x, ...) {uj:::.a_pop(x) & base::length(uj::av(...)) > 0}
.a_scl_dot <- function(x, ...) {uj:::.a_scl(x) & base::length(uj::av(...)) > 0}

#' @name failsafe_is
#' @encoding UTF-8
#' @family extensions
#' @family failsafe
#' @family logicals
#' @title Failsafe `is` functions
#' @description These functions *always* produce `TRUE` or `FALSE` results unless calling `identity(.)` produce an error.
#' \cr\cr Function names are constructed of prefixes and suffixes, where the suffix specifies what type of check is conducted and the prefix specifies how the check is modified or applied and swept across multiple values.
#' \cr\cr *Type-of-check suffixes*
#' \tabular{rl}{
#'       `NAS`   \tab `NA` scalar.
#'   \cr `OKS`   \tab Non-`NA` scalar.
#'   \cr `OUT`   \tab Atomic scalar `x` *is not in* `...`\eqn{^1}.
#'   \cr  `IN`   \tab Atomic scalar `x` *is in* `...`\eqn{^1}.
#'   \cr  `EQ`   \tab Calls \code{\link[base:setequal]{setequal(x, y)}}.
#'   \cr  `ID`   \tab Calls \code{\link[base:identical]{identical(x, y)}}.
#'   \cr  `LG`   \tab Scalar logical (`TRUE`, `FALSE`, or `NA`).
#'   \cr  `TF`   \tab Scalar `TRUE` or `FALSE`.
#'   \cr  `BL`   \tab Scalar blank (`""`).
#'   \cr   `F`   \tab Scalar `FALSE`.
#'   \cr   `T`   \tab Scalar `TRUE`.
#' }
#' ` `\eqn{^{1.}} Evaluates membership in any \link[=av]{atomized} `...` argument.
#' \cr\cr *Modifying prefixes* \tabular{rl}{
#'      `not`   \tab Negation
#'   \cr `is`   \tab Identity
#' }
#' \cr *Apply-and-sweep prefixes to count* `TRUE` *values* \tabular{rl}{
#'       `nor`   \tab `0`
#'   \cr `one`   \tab `1`
#'   \cr `any`   \tab `1+`
#'   \cr `two`   \tab `2+`
#'   \cr `all`   \tab All
#' }
#' \cr *Combining prefixes and suffixes*
#' \cr\cr All prefixes combine with all suffixes to form function names.
#' @param x,y Any R object.
#' @param ... Objects to check `x` against for functions with the suffix `IN` or `OUT`.
#' @return Scalar `TRUE` or `FALSE`.
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
#' uj::isNAS(ChrMat.)
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
isNAS <- function(x) {uj::f0(!uj:::.a_scl(x), F, base::is.na(x))}

#' @rdname failsafe_is
#' @export
isOKS <- function(x) {uj::f0(!uj:::.a_scl(x), F, !base::is.na(x))}

#' @rdname failsafe_is
#' @export
isOUT <- function(x, ...) {
  if (!uj:::.a_scl_dot(x, ...)) {return(FALSE)}
  for (i in 1:base::...length()) {if (base::is.atomic(base::...elt(i))) {if (uj::compatible(x, base::...elt(i))) {if (x %in% uj::av(base::...elt(i))) {return(FALSE)}}}}
  TRUE
}

#' @rdname failsafe_is
#' @export
isIN <- function(x, ...) {
  if (!uj:::.a_scl_dot(x, ...)) {return(FALSE)}
  for (i in 1:base::...length()) {if (base::is.atomic(base::...elt(i))) {if (uj::compatible(x, base::...elt(i))) {if (x %in% uj::av(base::...elt(i))) {return(TRUE)}}}}
  FALSE
}

#' @rdname failsafe_is
#' @export
isID <- function(x, y) {base::identical(x, y)}

#' @rdname failsafe_is
#' @export
isEQ <- function(x, y) {uj::f0(!uj:::.a_pop_dot(x, y), FALSE, tryCatch(base::setequal(x, y), error = function(e) F, finally = NULL))}

#' @rdname failsafe_is
#' @export
isT <- function(x) {base::isTRUE(x)}

#' @rdname failsafe_is
#' @export
isF <- function(x) {base::isFALSE(x)}

#' @rdname failsafe_is
#' @export
isTF <- function(x) {base::isTRUE(x) | base::isFALSE(x)}

#' @rdname failsafe_is
#' @export
isLG <- function(x) {base::isTRUE(x) | base::isFALSE(x) | uj::isNAS(x)}

#' @rdname failsafe_is
#' @export
isBL <- function(x) {uj::f0(!uj:::.a_scl(x), F, uj::isEQ(x, ""))}

#' @rdname failsafe_is
#' @export
notIN <- function(x, ...) {!uj::isIN(x, ...)}

#' @rdname failsafe_is
#' @export
notOUT <- function(x, ...) {!uj::isOUT(x, ...)}

#' @rdname failsafe_is
#' @export
notID <- function(x, y) {!uj::isID(x, y)}

#' @rdname failsafe_is
#' @export
notEQ <- function(x, y) {!uj::isEQ(x, y)}

#' @rdname failsafe_is
#' @export
notTF <- function(x) {!uj::isTF(x)}

#' @rdname failsafe_is
#' @export
notLG <- function(x) {!uj::isLG(x)}

#' @rdname failsafe_is
#' @export
notT <- function(x) {!uj::isT(x)}

#' @rdname failsafe_is
#' @export
notF <- function(x) {!uj::isF(x)}

#' @rdname failsafe_is
#' @export
notNAS <- function(x) {!uj::isNAS(x)}

#' @rdname failsafe_is
#' @export
notOKS <- function(x) {!uj::isOKS(x)}

#' @rdname failsafe_is
#' @export
notBL <- function(x) {!uj::isBL(x)}

#' @rdname failsafe_is
#' @export
norIN <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, !base::any(base::sapply(x, uj::isIN, ...)))}

#' @rdname failsafe_is
#' @export
norOUT <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, !base::any(sbase::apply(x, uj::isOUT, ...)))}

#' @rdname failsafe_is
#' @export
norID <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, !base::any(base::sapply(x, uj::isID, y = y)))}

#' @rdname failsafe_is
#' @export
norEQ <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, !base::any(base::sapply(x, uj::isEQ, y = y)))}

#' @rdname failsafe_is
#' @export
norT <- function(x) {uj::f0(!uj:::.pop(x), T, !base::any(base::sapply(x, base::isTRUE)))}

#' @rdname failsafe_is
#' @export
norF <- function(x) {uj::f0(!uj:::.pop(x), T, !base::any(base::sapply(x, base::isFALSE)))}

#' @rdname failsafe_is
#' @export
norNAS <- function(x) {uj::f0(!uj:::.pop(x), T, !base::any(base::sapply(x, uj::isNAS)))}

#' @rdname failsafe_is
#' @export
norOKS <- function(x) {uj::f0(!uj:::.pop(x), T, !base::any(base::sapply(x, uj::isNAS)))}

#' @rdname failsafe_is
#' @export
norTF <- function(x) {uj::f0(!uj:::.pop(x), T, !base::any(base::sapply(x, uj::isTF)))}

#' @rdname failsafe_is
#' @export
norLG <- function(x) {uj::f0(!uj:::.pop(x), T, !base::any(base::sapply(x, uj::isLG)))}

#' @rdname failsafe_is
#' @export
norBL <- function(x) {uj::f0(!uj:::.pop(x), T, !base::any(base::sapply(x, uj::isBL)))}

#' @rdname failsafe_is
#' @export
anyIN <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, base::any(base::sapply(x, uj::isIN, ...)))}

#' @rdname failsafe_is
#' @export
anyOUT <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, base::any(base::sapply(x, uj::isOUT, ...)))}

#' @rdname failsafe_is
#' @export
anyID <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, base::any(base::sapply(x, uj::isID, y = y)))}

#' @rdname failsafe_is
#' @export
anyEQ <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, base::any(base::sapply(x, uj::isEQ, y = y)))}

#' @rdname failsafe_is
#' @export
anyT <- function(x) {uj::f0(!uj:::.pop(x), F, base::any(base::sapply(x, base::isTRUE)))}

#' @rdname failsafe_is
#' @export
anyF <- function(x) {uj::f0(!uj:::.pop(x), F, base::any(base::sapply(x, base::isFALSE)))}

#' @rdname failsafe_is
#' @export
anyNAS <- function(x) {uj::f0(!uj:::.pop(x), F, base::any(base::sapply(x, uj::isNAS)))}

#' @rdname failsafe_is
#' @export
anyOKS <- function(x) {uj::f0(!uj:::.pop(x), F, base::any(base::sapply(x, uj::isNAS)))}

#' @rdname failsafe_is
#' @export
anyTF <- function(x) {uj::f0(!uj:::.pop(x), F, base::any(base::sapply(x, uj::isTF)))}

#' @rdname failsafe_is
#' @export
anyLG <- function(x) {uj::f0(!uj:::.pop(x), F, base::any(base::sapply(x, uj::isLG)))}

#' @rdname failsafe_is
#' @export
anyBL <- function(x) {uj::f0(!uj:::.pop(x), F, base::any(base::sapply(x, uj::isBL)))}

#' @rdname failsafe_is
#' @export
allIN <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, uj::all(uj::sapply(x, uj::isIN, ...)))}

#' @rdname failsafe_is
#' @export
allOUT <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, uj::all(uj::sapply(x, uj::isOUT, ...)))}

#' @rdname failsafe_is
#' @export
allID <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, uj::all(uj::sapply(x, uj::isID, y = y)))}

#' @rdname failsafe_is
#' @export
allEQ <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, uj::all(uj::sapply(x, uj::isEQ, y = y)))}

#' @rdname failsafe_is
#' @export
allT <- function(x) {uj::f0(!uj:::.pop(x), F, uj::all(uj::sapply(x, base::isTRUE)))}

#' @rdname failsafe_is
#' @export
allF <- function(x) {uj::f0(!uj:::.pop(x), F, uj::all(uj::sapply(x, base::isFALSE)))}

#' @rdname failsafe_is
#' @export
allNAS <- function(x) {uj::f0(!uj:::.pop(x), F, uj::all(uj::sapply(x, uj::isNAS)))}

#' @rdname failsafe_is
#' @export
allOKS <- function(x) {uj::f0(!uj:::.pop(x), F, uj::all(uj::sapply(x, uj::isNAS)))}

#' @rdname failsafe_is
#' @export
allTF <- function(x) {uj::f0(!uj:::.pop(x), F, uj::all(uj::sapply(x, uj::isTF)))}

#' @rdname failsafe_is
#' @export
allLG <- function(x) {uj::f0(!uj:::.pop(x), T, uj::all(uj::sapply(x, uj::isLG)))}

#' @rdname failsafe_is
#' @export
allBL <- function(x) {uj::f0(!uj:::.pop(x), T, uj::all(uj::sapply(x, uj::isBL)))}

#' @rdname failsafe_is
#' @export
oneIN <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, uj::length(uj::which(uj::sapply(x, uj::isIN, ...))) == 1)}

#' @rdname failsafe_is
#' @export
oneOUT <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, uj::length(uj::which(uj::sapply(x, uj::isOUT, ...))) == 1)}

#' @rdname failsafe_is
#' @export
oneID <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, uj::length(uj::which(uj::sapply(x, uj::isID, y = y))) == 1)}

#' @rdname failsafe_is
#' @export
oneEQ <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, uj::length(uj::which(uj::sapply(x, uj::isEQ, y = y))) == 1)}

#' @rdname failsafe_is
#' @export
oneT <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, base::isTRUE))) == 1)}

#' @rdname failsafe_is
#' @export
oneF <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, base::isFALSE))) == 1)}

#' @rdname failsafe_is
#' @export
oneNAS <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isNAS))) == 1)}

#' @rdname failsafe_is
#' @export
oneOKS <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isOKS))) == 1)}

#' @rdname failsafe_is
#' @export
oneTF <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isTF))) == 1)}

#' @rdname failsafe_is
#' @export
oneLG <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isLG))) == 1)}

#' @rdname failsafe_is
#' @export
oneBL <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isBL))) == 1)}

#' @rdname failsafe_is
#' @export
twoIN <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, uj::length(uj::which(uj::sapply(x, uj::isIN, ...))) > 1)}

#' @rdname failsafe_is
#' @export
twoOUT <- function(x, ...) {uj::f0(!uj:::.a_pop_dot(x, ...), F, uj::length(uj::which(uj::sapply(x, uj::isOUT, ...))) > 1)}

#' @rdname failsafe_is
#' @export
twoID <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, uj::length(uj::which(uj::sapply(x, uj::isID, y = y))) > 1)}

#' @rdname failsafe_is
#' @export
twoEQ <- function(x, y) {uj::f0(!uj:::.pop_pop(x, y), F, uj::length(uj::which(uj::sapply(x, uj::isEQ, y = y))) > 1)}

#' @rdname failsafe_is
#' @export
twoT <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, base::isTRUE))) > 1)}

#' @rdname failsafe_is
#' @export
twoF <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, base::isFALSE))) > 1)}

#' @rdname failsafe_is
#' @export
twoNAS <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isNAS))) > 1)}

#' @rdname failsafe_is
#' @export
twoOKS <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isOKS))) > 1)}

#' @rdname failsafe_is
#' @export
twoTF <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isTF))) > 1)}

#' @rdname failsafe_is
#' @export
twoLG <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isLG))) > 1)}

#' @rdname failsafe_is
#' @export
twoBL <- function(x) {uj::f0(!uj:::.pop(x), F, uj::length(uj::which(uj::sapply(x, uj::isBL))) > 1)}
