#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Swap atomic values.
#' @param x An atomic object.
#' @param old,new \link[=atm_vec]{Atomic vecs} of unique values to be replaced and replacement values, respectively. `length(new)` must be in `c(1, length(old))`.
#' @param all A non-`NA` logical scalar indicating whether all values of `x` must be contained in `old`.
#' @return An atomic object of the same dimensions as `x`.
#' @examples
#' letters[1:6]
#' swap(letters[1:6], c("a", "b", "c"), ".")
#' swap(letters[1:6], c("a", "b", "c"), c("-A-", "-B-", "-C-"))
#' @export
swap <- function(x, old, new, all = FALSE) {
  errs <- base::c(uj::f0(uj::ipop(x) & uj::iatm(x), NULL, "[x] must be populated and atomic (?ipop)."),
                  uj::g0(uj::atm_vec(old)         , NULL, "[old] must be populated and atomic (?ipop)."),
                  uj::f0(uj::atm_vec(new)         , NULL, "[new] must be populated and atomic (?ipop)."),
                  uj::f0(uj::isTF(all)            , NULL, "[all] must be scalar TRUE or scalar FALSE"))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  n.old <- base::length(old)
  n.new <- base::length(new)
  errs <- base::c(uj::f0(uj::compatible(x, old, new)      , NULL, "[x], [old], and [new] are of incompatible modes (?compatible)."),
                  uj::f0(uj::is_unq(old)                  , NULL, "old] must contain only unique elements."),
                  uj::f0(n.new %in% base::c(1, n.old)     , NULL, "[length(new)] must be in [c(1, length(old))]."),
                  uj::f0(uj::f0(all, uj::allIN(x, old), T), NULL, "[all = TRUE] but not all elements of [x] are contained in [old]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (n.new == 1) {new <- base::rep.int(new, n.old)}
  if (base::any(base::is.na(old))) {x[base::is.na(x)] <- new[base::which(base::is.na(old))]}
  for (i in 1:n.old) {x[x == old[i]] <- new[i]}
  x
}
