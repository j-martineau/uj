#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Swap by Row Index, Column Index, Element Index, and Atomic Value.
#' @param X A non-empty data.frame, matrix, vlist (?uj::VLS), or an atomic object of any dimension.
#' @param DIM Dimension on which swapping occurs. `0` indicates swapping by element index of a non-empty vector, vlist, or other atomic object. `1` and `2` indicate, respectively, swapping rows and columns of a data.frame or matrix.
#' @param From,To Uniquely valued non-zero whole number vectors indexing source (`From`) and destination (`To`) positions, rows, or columns. `From < 0` and `To < 0` index from the last value rather than the first.
#' @param Old \link[=unq_atm_vec]{A uniquely valued atomic vec} of values to be replaced.
#' @param New \link[=atm_vec]{An atomic vec} of replacement values. `length(New)` must be in `c(1, length(Old))`.
#' @param All `TRUE` or `FALSE` indicating whether all values of `X` must be contained in `Old` (ignored when `Old` and `New` are `NULL`).
#' @return An object of the same class and dimensions as `X`.
#' @examples
#' letters[1:6]
#' swap(letters[1:6], c("a", "b", "c"), ".")
#' swap(letters[1:6], c("a", "b", "c"), c("-A-", "-B-", "-C-"))
#' @export
swap <- function(X, DIM = 0, From = NULL, To = NULL, Old = NULL, New = NULL, All = FALSE) {
  Errs <- NULL
  if (!uj::.pop_dtf(X) & !uj::.pop_mat(X) & !uj::.pop_atm(X) & !uj::.pop_vls(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix, data.frame, vlist, or an atomic object (?uj::VLS).")}
  if (uj::cmp_nnw_scl(DIM)) {if (base::any(!(DIM %in% 0:2))) {Errs <- base::c(Errs, "[DIM] must be 0, 1, or 2.")}}
  else {Errs <- base::c(Errs, "[DIM] must be 0, 1, or 2.")}
  if (!uj::is_TF(All)) {Errs <- base::c(Errs, "[All] must be TRUE or FALSE.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (uj::.DTF(X) | uj::.MAT(X)) {
    if (DIM == 0) {Errs <- base::c(Errs, "[DIM] must be 2 or 2 when [X] is a data frame or matrix.")}
    if (!uj::NLL(Old)) {Errs <- base::c(Errs, "[Old] must be NULL when [X] is a data frame or matrix.")}
    if (!uj::NLL(New)) {Errs <- base::c(Errs, "[New] must be NULL when [X] is a data frame or matrix.")}
    if (!uj::unq_whl_vec(From)) {Errs <- base::c(Errs, "[From] must be a uniquely valued whole-number vector.")}
    if (!uj::unq_whl_vec(From)) {Errs <- base::c(Errs, "[To] must be a uniquely valued whole-number vector.")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
    if (base::length(From) == 1 & base::length(To) > 1) {From <- base::rep(From, base::length(To))}
    if (base::length(To) == 1 & base::length(From) > 1) {To <- base::rep(To, base::length(From))}
    if (base::length(From) != base::length(To)) {Errs <- base::c(Errs, "[From] and [To] are not mutually recyclable.")}
    nRow <- base::nrow(X)
    nCol <- base::ncol(X)
    AbsI <- base::abs(From)
    AbsJ <- base::abs(To)
    if (base::any(From == 0) | base::any(To == 0)) {Errs <- base::c(Errs, "[From] and [To] may not contain values equal to zero.")}
    if (DIM == 1 & (base::any(AbsI > nRow))) {Errs <- base::c(Errs, "A value in abs(From) > nrow(X).")}
    if (DIM == 1 & (base::any(AbsJ > nRow))) {Errs <- base::c(Errs, "A value in abs(To) > nrow(X).")}
    if (DIM == 2 & (base::any(AbsI > nCol))) {Errs <- base::c(Errs, "A value in abs(From) > ncol(X).")}
    if (DIM == 2 & (base::any(AbsJ > nCol))) {Errs <- base::c(Errs, "A value in abs(To) > ncol(X).")}
    From[From < 0] <- uj::f0(DIM == 1, nRow, nCol) + From[From < 0] + 1
    To[To < 0] <- uj::f0(DIM == 1, nRow, nCol) + To[To < 0] + 1
    if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
    for (i in 1:base::length(From)) {
      PartI <- uj::f0(DIM == 1, X[From[i], , drop = F], X[ , From[i], drop = F])
      PartJ <- uj::f0(DIM == 1, X[To[i], , drop = F], X[ , To[i], drop = F])
      if (DIM == 1) {X[From[i], ] <- PartJ;  X[To[i], ] <- PartI}
      else {X[ , From[i]] <- PartI; X[ , To[i]] <- PartI}
    }
  } else if (uj::DEF(Old) | uj::DEF(New)) {
    if (!uj::.pop_atm(X)) {Errs <- base::c(Errs, "[Old] and/or [New] are supplied, but [X] is not atomic.")}
    if (DIM != 0) {Errs <- base::c(Errs, "[DIM] must be 0 when [X] is a vector or vlist (?uj::VLS).")}
    if (!uj::unq_atm_vec(Old)) {Errs <- base::c(Errs, "[Old] must be a unique-valued atomic vector (?uj::unq_atm_vec).")}
    if (!uj::atm_vec(New)) {Errs <- base::c(Errs, "[New] must be an atomic vector (?uj::atm_vec).")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
    if (!uj:::.compat(X, Old) | !uj:::.compat(X, New) | !uj:::.compat(Old, New)) {Errs <- base::c(Errs, "[X], [Old], and [New] must be compatible (?uj::compatible).")}
    if (base::length(Old) < base::length(New)) {Errs <- base::c(Errs, "length(Old) < length(New).")}
    if (base::length(Old) == 1 & base::length(New) > 1) {Old <- base::rep(Old, base::length(New))}
    if (base::length(Old) != base::length(New)) {Errs <- base::c(Errs, "[New] cannot be recycled to match length(Old).")}
    if (All & !base::all(X %in% Old)) {Errs <- base::c(Errs, "When [All] is TRUE, all elements of [X] must be contained in [Old].")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
    for (i in 1:base::length(Old)) {X[X == Old[i]] <- New[i]}
  } else {
    if (!uj::DEF(From)) {Errs <- base::c("When [X] is a vector or vlist (?uj::VLS) and neither [Old] nor [New] is supplied, both [From] and [To] must be supplied.")}
    if (base::length(From) == 1 & base::length(To) > 1) {From <- uj::r(uj::N(To))}
    if (base::length(To) == 1 & base::length(From) > 1) {To <- uj::r(uj::N(From))}
    if (base::length(From) != base::length(To)) {Errs <- base::c(Errs, "[From] and [To] are not recyclable.")}
    if (base::any(From == 0) | base::any(To == 0)) {Errs <- base::c(Errs, "[From] and [To] may not contain values equal to zero.")}
    if (base::any(base::abs(From) > base::nrow(X))) {Errs <- base::c(Errs, "A value in abs(From) > nrow(X).")}
    if (DIM == 1 & (base::any(base::abs(To) > base::nrow(X)))) {Errs <- base::c(Errs, "A value in abs(To) > nrow(X).")}
    nElt <- base::length(X)
    AbsI <- base::abs(From)
    AbsJ <- base::abs(To)
    if (base::any(From == 0) | base::any(To == 0)) {Errs <- base::c(Errs, "[From] and [To] may not contain values equal to zero.")}
    if (base::any(AbsI > nElt)) {Errs <- base::c(Errs, "A value in abs(From) > length(X).")}
    if (base::any(AbsJ > nElt)) {Errs <- base::c(Errs, "A value in abs(To) > length(X).")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
    From[From < 0] <- nElt + From[From < 0] + 1
    To[To < 0] <- nElt + To[To < 0] + 1
    for (i in 1:length(From)) {
      EltI <- X[From[i]]
      EltJ <- X[To[i]]
      X[From[i]] <- EltJ
      X[To[i]] <- EltI
    }
  }
  X
}

#' @rdname swap
#' @export
swap_vals <- function(X, Old, New, All = FALSE) {uj::swap(X, Old = Old, New = New, All = All)}

#' @rdname swap
#' @export
swap_elt <- function(X, From, To) {uj::swap(X, From = From, To = To)}

#' @rdname swap
#' @export
swap_rows <- function(X, From, To) {uj::swap(X, DIM = 1, From = From, To = To)}

#' @rdname swap
#' @export
swap_cols <- function(X, From, To) {uj::swap(X, DIM = 2, From = From, To = To)}
