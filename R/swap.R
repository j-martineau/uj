#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Swap atomic values.
#' @param X An atomic object.
#' @param Old,New \link[=atm_vec]{Atomic vecs} of unique values to be replaced and replacement values, respectively. `length(New)` must be in `c(1, length(Old))`.
#' @param All `TRUE` or `FALSE` indicating whether All values of `X` must be contained in `Old`.
#' @return An atomic object of the same dimensions as `X`.
#' @examples
#' letters[1:6]
#' swap(letters[1:6], c("a", "b", "c"), ".")
#' swap(letters[1:6], c("a", "b", "c"), c("-A-", "-B-", "-C-"))
#' @export
swap <- function(X, Old, New, All = FALSE) {
  Errors <- NULL
  if (!uj:::.pop_atm(X)) {Errors <- base::c(Errors, "[X] must be populated and atomic (?pop_atm).")}
  if (!uj:::.atm_vec(Old)) {Errors <- base::c(Errors, "[Old] must be an atomic vec (?atm_vec).")}
  if (!uj:::.atm_vec(New)) {Errors <- base::c(Errors, "[New] must be an atomic vec (?atm_vec).")}
  if (!uj:::.cmp_lgl_scl(All)) {Errors <- base::c(Errors, "[All] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  OldCount <- base::length(Old)
  NewCount <- base::length(New)
  if (All) {ok.Old <- base::All(X %in% Old)} else {ok.Old <- T}
  if (!uj::compatible(X, Old, New)) {Errors <- base::c(Errors, "[X], [Old], and [New] are of incompatible modes (?compatible).")}
  if (base::length(Old) != base::length(base::unique(Old))) {Errors <- base::c(Errors, "[Old] must contain only unique elements.")}
  if (!(NewCount %in% base::c(1, OldCount))) {Errors <- base::c(Errors, "[length(New)] must be in [c(1, length(Old))].")}
  if (!ok.Old) {Errors <- base::c(Errors, "[All = TRUE] but not All elements of [X] are contained in [Old].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (NewCount == 1) {New <- base::rep.int(New, OldCount)}
  if (base::any(base::is.na(Old))) {X[base::is.na(X)] <- New[base::is.na(Old)]}
  for (i in 1:OldCount) {X[X == Old[i]] <- New[i]}
  X
}
