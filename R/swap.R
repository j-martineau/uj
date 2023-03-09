#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Swap atomic values.
#' @param x An atomic object.
#' @param old,new \link[=atm_vec]{Atomic vecs} of unique values to be replaced and replacement values, respectively. `length(new)` must be in `c(1, length(old))`.
#' @param all `TRUE` or `FALSE` indicating whether all values of `x` must be contained in `old`.
#' @return An atomic object of the same dimensions as `x`.
#' @examples
#' letters[1:6]
#' swap(letters[1:6], c("a", "b", "c"), ".")
#' swap(letters[1:6], c("a", "b", "c"), c("-A-", "-B-", "-C-"))
#' @export
swap <- function(x, old, new, all = FALSE) {
  errs <- NULL
  if (!uj:::.pop_atm(x)) {errs <- base::c(errs, "[x] must be populated and atomic (?pop_atm).")}
  if (!uj:::.atm_vec(old)) {errs <- base::c(errs, "[old] must be an atomic vec (?atm_vec).")}
  if (!uj:::.atm_vec(new)) {errs <- base::c(errs, "[new] must be an atomic vec (?atm_vec).")}
  if (!uj:::.cmp_lgl_scl(all)) {errs <- base::c(errs, "[all] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  n.old <- base::length(old)
  n.new <- base::length(new)
  if (all) {ok.old <- base::all(x %in% old)} else {ok.old <- T}
  if (!uj::compatible(x, old, new)) {errs <- base::c(errs, "[x], [old], and [new] are of incompatible modes (?compatible).")}
  if (base::length(old) != base::length(base::unique(old))) {errs <- base::c(errs, "[old] must contain only unique elements.")}
  if (!(n.new %in% base::c(1, n.old))) {errs <- base::c(errs, "[length(new)] must be in [c(1, length(old))].")}
  if (!ok.old) {errs <- base::c(errs, "[all = TRUE] but not all elements of [x] are contained in [old].")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (n.new == 1) {new <- base::rep.int(new, n.old)}
  if (base::any(base::is.na(old))) {x[base::is.na(x)] <- new[base::is.na(old)]}
  for (i in 1:n.old) {x[x == old[i]] <- new[i]}
  x
}
