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
  errs <- c(f0(ipop(x) & iatm(x), NULL, "[x] must be populated and atomic (?ipop)."),
            f0(atm_vec(old)     , NULL, "[old] must be populated and atomic (?ipop)."),
            f0(atm_vec(new)     , NULL, "[new] must be populated and atomic (?ipop)."),
            f0(isTF(all)        , NULL, "[all] must be scalar TRUE or scalar FALSE"))
  if (!is.null(errs)) {stop(.errs(errs))}
  n.old <- length(old)
  n.new <- length(new)
  errs <- c(f0(compatible(x, old, new)  , NULL, "[x], [old], and [new] are of incompatible modes (?compatible)."),
            f0(is_unq(old)              , NULL, "old] must contain only unique elements."),
            f0(n.new %in% c(1, n.old)   , NULL, "[length(new)] must be in [c(1, length(old))]."),
            f0(f0(all, allIN(x, old), T), NULL, "[all = TRUE] but not all elements of [x] are contained in [old]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (n.new == 1) {new <- rep.int(new, n.old)}
  if (any(is.na(old))) {x[is.na(x)] <- new[which(is.na(old))]}
  for (i in 1:n.old) {x[x == old[i]] <- new[i]}
  x
}
