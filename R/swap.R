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
  uj::errs_if_nots(uj::pop_atm(x)      , "[x] must be populated and atomic (?pop_atm).",
                   uj::atm_vec(old)    , "[old] must be an atomic vec (?atm_vec)."     ,
                   uj::atm_vec(new)    , "[new] must be an atomic vec (?atm_vec)."     ,
                   uj::cmp_lgl_scl(all), "[all] must be TRUE or FALSE"                 , PKG = "uj")
  n.old <- uj::N(old)
  n.new <- uj::N(new)
  uj::errs_if_nots(uj::compatible(x, old, new)      , "[x], [old], and [new] are of incompatible modes (?compatible)."  ,
                   uj::UNQ(old)                     , "[old] must contain only unique elements."                        ,
                   uj::isIN1(n.new, 1, n.old)       , "[length(new)] must be in [c(1, length(old))]."                   ,
                   uj::f0(all, uj::allIN(x, old), T), "[all = TRUE] but not all elements of [x] are contained in [old].", PKG = "uj")
  if (n.new == 1) {new <- base::rep.int(new, n.old)}
  if (uj::anyNAS(old)) {x[uj::na(x)] <- new[uj::na(old)]}
  for (i in 1:n.old) {x[x == old[i]] <- new[i]}
  x
}
