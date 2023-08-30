#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Swap by Row Index, Column Index, Element Index, and Atomic Value.
#' @param x A non-empty data.frame, matrix, vlist (?uj::VLS), or an atomic object of any dimension.
#' @param from,to Uniquely valued non-zero whole number vectors indexing source (`from`) and destination (`to`) positions, rows, or columns. `from < 0` and `to < 0` index from the last value rather than the first.
#' @param old \link[=unq_atm_vec]{A uniquely valued atomic vec} of values to be replaced.
#' @param new \link[=atm_vec]{An atomic vec} of replacement values. `length(new)` must be in `c(1, length(old))`.
#' @param .DIM Dimension on which swapping occurs. `0` indicates swapping by element index of a non-empty vector, vlist, or other atomic object. `1` and `2` indicate, respectively, swapping rows and columns of a data.frame or matrix.
#' @param .ALL `TRUE` or `FALSE` indicating whether all values of `x` must be contained in `old` (ignored when `old` and `new` are `NULL`).
#' @return An object of the same class and dimensions as `x`.
#' @examples
#' letters[1:6]
#' swap(letters[1:6], c("a", "b", "c"), ".")
#' swap(letters[1:6], c("a", "b", "c"), c("-A-", "-B-", "-C-"))
#' @export
swap <- function(x, from = NULL, to = NULL, old = NULL, new = NULL, .DIM = 0, .ALL = FALSE) {
  Errs <- NULL
  if (!uj::.pop_dtf(x) & !uj::.pop_mat(x) & !uj::.pop_atm(x) & !uj::.pop_vls(x)) {Errs <- base::c(Errs, "[x] must be a populated matrix, data.frame, vlist, or an atomic object (?uj::VLS).")}
  if (uj::cmp_nnw_scl(.DIM)) {if (base::any(!(.DIM %in% 0:2))) {Errs <- base::c(Errs, "[.DIM] must be 0, 1, or 2.")}}
  else {Errs <- base::c(Errs, "[.DIM] must be 0, 1, or 2.")}
  if (!uj::is_TF(.ALL)) {Errs <- base::c(Errs, "[.ALL] must be TRUE or FALSE.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (uj::.DTF(x) | uj::.MAT(x)) {
    if (.DIM == 0) {Errs <- base::c(Errs, "[.DIM] must be 2 or 2 when [x] is a data frame or matrix.")}
    if (!uj::NLL(old)) {Errs <- base::c(Errs, "[old] must be NULL when [x] is a data frame or matrix.")}
    if (!uj::NLL(new)) {Errs <- base::c(Errs, "[new] must be NULL when [x] is a data frame or matrix.")}
    if (!uj::unq_whl_vec(from)) {Errs <- base::c(Errs, "[from] must be a uniquely valued whole-number vector.")}
    if (!uj::unq_whl_vec(from)) {Errs <- base::c(Errs, "[to] must be a uniquely valued whole-number vector.")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
    if (base::length(from) == 1 & base::length(to) > 1) {from <- base::rep(from, base::length(to))}
    if (base::length(to) == 1 & base::length(from) > 1) {to <- base::rep(to, base::length(from))}
    if (base::length(from) != base::length(to)) {Errs <- base::c(Errs, "[from] and [to] are not mutually recyclable.")}
    nRow <- base::nrow(x)
    nCol <- base::ncol(x)
    AbsI <- base::abs(from)
    AbsJ <- base::abs(to)
    if (base::any(from == 0) | base::any(to == 0)) {Errs <- base::c(Errs, "[from] and [to] may not contain values equal to zero.")}
    if (.DIM == 1 & (base::any(AbsI > nRow))) {Errs <- base::c(Errs, "A value in abs(from) > nrow(x).")}
    if (.DIM == 1 & (base::any(AbsJ > nRow))) {Errs <- base::c(Errs, "A value in abs(to) > nrow(x).")}
    if (.DIM == 2 & (base::any(AbsI > nCol))) {Errs <- base::c(Errs, "A value in abs(from) > ncol(x).")}
    if (.DIM == 2 & (base::any(AbsJ > nCol))) {Errs <- base::c(Errs, "A value in abs(to) > ncol(x).")}
    from[from < 0] <- uj::f0(.DIM == 1, nRow, nCol) + from[from < 0] + 1
    to[to < 0] <- uj::f0(.DIM == 1, nRow, nCol) + to[to < 0] + 1
    if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
    for (i in 1:base::length(from)) {
      PartI <- uj::f0(.DIM == 1, x[from[i], , drop = F], x[ , from[i], drop = F])
      PartJ <- uj::f0(.DIM == 1, x[to[i], , drop = F], x[ , to[i], drop = F])
      if (.DIM == 1) {x[from[i], ] <- PartJ;  x[to[i], ] <- PartI}
      else {x[ , from[i]] <- PartI; x[ , to[i]] <- PartI}
    }
  } else if (uj::DEF(old) | uj::DEF(new)) {
    if (!uj::.pop_atm(x)) {Errs <- base::c(Errs, "[old] and/or [new] are supplied, but [x] is not atomic.")}
    if (.DIM != 0) {Errs <- base::c(Errs, "[.DIM] must be 0 when [x] is a vector or vlist (?uj::VLS).")}
    if (!uj::unq_atm_vec(old)) {Errs <- base::c(Errs, "[old] must be a unique-valued atomic vector (?uj::unq_atm_vec).")}
    if (!uj::atm_vec(new)) {Errs <- base::c(Errs, "[new] must be an atomic vector (?uj::atm_vec).")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
    if (!uj:::.compat(x, old) | !uj:::.compat(x, new) | !uj:::.compat(old, new)) {Errs <- base::c(Errs, "[x], [old], and [new] must be compatible (?uj::compatible).")}
    if (base::length(old) < base::length(new)) {Errs <- base::c(Errs, "length(old) < length(new).")}
    if (base::length(old) == 1 & base::length(new) > 1) {old <- base::rep(old, base::length(new))}
    if (base::length(old) != base::length(new)) {Errs <- base::c(Errs, "[new] cannot be recycled to match length(old).")}
    if (.ALL & !base::all(x %in% old)) {Errs <- base::c(Errs, "When [.ALL = TRUE], all elements of [x] must be contained in [old].")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
    for (i in 1:base::length(old)) {x[x == old[i]] <- new[i]}
  } else {
    if (!uj::DEF(from)) {Errs <- base::c("When [x] is a vector or vlist (?uj::VLS) and neither [old] nor [new] is supplied, both [from] and [to] must be supplied.")}
    if (base::length(from) == 1 & base::length(to) > 1) {from <- uj::r(uj::N(to))}
    if (base::length(to) == 1 & base::length(from) > 1) {to <- uj::r(uj::N(from))}
    if (base::length(from) != base::length(to)) {Errs <- base::c(Errs, "[from] and [to] are not recyclable.")}
    if (base::any(from == 0) | base::any(to == 0)) {Errs <- base::c(Errs, "[from] and [to] may not contain values equal to zero.")}
    if (base::any(base::abs(from) > base::nrow(x))) {Errs <- base::c(Errs, "A value in abs(from) > nrow(x).")}
    if (dim == 1 & (base::any(base::abs(to) > base::nrow(x)))) {Errs <- base::c(Errs, "A value in abs(to) > nrow(x).")}
    nElt <- base::length(x)
    AbsI <- base::abs(from)
    AbsJ <- base::abs(to)
    if (base::any(from == 0) | base::any(to == 0)) {Errs <- base::c(Errs, "[from] and [to] may not contain values equal to zero.")}
    if (base::any(AbsI > nElt)) {Errs <- base::c(Errs, "A value in abs(from) > length(x).")}
    if (base::any(AbsJ > nElt)) {Errs <- base::c(Errs, "A value in abs(to) > length(x).")}
    if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
    from[from < 0] <- nElt + from[from < 0] + 1
    to[to < 0] <- nElt + to[to < 0] + 1
    for (i in 1:length(from)) {
      EltI <- x[from[i]]
      EltJ <- x[to[i]]
      x[from[i]] <- EltJ
      x[to[i]] <- EltI
    }
  }
  x
}

#' @rdname swap
#' @export
swap_vals <- function(x, old, new, all = FALSE) {uj::swap(x, old = old, new = new, all = all)}

#' @rdname swap
#' @export
swap_elt <- function(x, from, to) {uj::swap(x, from = from, to = to)}

#' @rdname swap
#' @export
swap_rows <- function(x, from, to) {uj::swap(x, .DIM = 1, from = from, to = to)}

#' @rdname swap
#' @export
swap_cols <- function(x, from, to) {uj::swap(x, .DIM = 2, from = from, to = to)}
