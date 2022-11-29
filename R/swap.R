#' @family forking
#' @family extensions
#' @title Swap atomic values.
#' @param x An atomic object.
#' @param old,new \link[=atm_vec]{Atomic vecs} of unique values to be replaced and replacement values, respectively. `length(new)` must be in `c(1, length(old))`.
#' @param all A non-`NA` logical scalar indicating whether all values of `x` must be contained in `old`.
#' @return An atomic object of the same dimensions as `x`.
#' @export
swap <- function(x, old, new, all = FALSE) {
  errs <- c(f0(ipop(x) & iatm(x), NULL, "\n \u2022 [x] must be populated and atomic (?ipop)."),
            f0(atm_vec(old)     , NULL, "\n \u2022 [old] must be populated and atomic (?ipop)."),
            f0(atm_vec(new)     , NULL, "\n \u2022 [new] must be populated and atomic (?ipop)."),
            f0(isTF(all)        , NULL, "\n \u2022 [all] must be scalar TRUE or scalar FALSE"))
  if (!is.null(errs)) {stop(errs)}
  n.old <- length(old)
  n.new <- length(new)
  errs <- c(f0(compatible(x, old, new)  , NULL, "\n \u2022 [x], [old], and [new] are of incompatible modes (?compatible)."),
            f0(is_unq(old)              , NULL, "\n \u2022 [old] must contain only unique elements."),
            f0(n.new %in% c(1, n.old)   , NULL, "\n \u2022 [length(new)] must be in [c(1, length(old))]."),
            f0(f0(all, allIN(x, old), T), NULL, "\n \u2022 [all = TRUE] but not all elements of [x] are contained in [old]."))
  if (!is.null(errs)) {stop(errs)}
  if (n.new == 1) {new <- rep.int(new, n.old)}
  if (any(is.na(old))) {x[is.na(x)] <- new[which(is.na(old))]}
  for (i in 1:n.old) {x[x == old[i]] <- new[i]}
  x
}
