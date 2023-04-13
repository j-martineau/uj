#' @encoding UTF-8
#' @family extensions
#' @title Create indices to build all possible non-empty sets
#' @description Creates a list of elements containing indices to create all possible unique-value sets of size `1` to `N` from a length-`N` vector of unique values.
#' @param N A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the length of set of unique values.
#' @return A \link[=cmp_psw_vls]{complete positive whole-number vlist}.
#' @examples
#' sets(1)
#' sets(2)
#' sets(3)
#' sets(4)
#' @export
sets <- function(N) {
  if (!uj:::.cmp_psw_scl(N)) {uj::stopperr("[N] must be a positive whole-number scalar.", PKG = "uj")}
  X <- base::lapply(1:N, function(x) utils::combn(N, x))                         # : all possible combinations from size 1 to size [N]
  Y <- NULL                                                                      # : initialize the result
  for (i in 1:base::length(X)) {                                                 # : FOR EACH element of the list returned in [x]
    Xi <- X[[i]]                                                                 # : : get that element and
    for (j in 1:base::ncol(Xi)) {Y <- base::c(Y, base::list(uj::av(Xi[ , j])))}  # : : FOR EACH column (i.e., unique combination) > append as a list element to [y]
  }                                                                              # : END
  Y                                                                              # : return value
}                                                                                # END

