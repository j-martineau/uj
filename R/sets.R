#' @title Create indices to build all possible non-empty sets
#' @description Creates a list of elements containing indices to create all possible unique-value sets of size `1` to `n` from a length-`n` vector of unique values.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the length of set of unique values.
#' @return A \link[=cmp_psw_vls]{complete positive whole-number vlist}.
#' @examples
#' sets(1)
#' sets(2)
#' sets(3)
#' sets(4)
#' @export
sets <- function(n) {
  if (!cmp_psw_scl(n)) {stop(.errs("[n] must be a positive whole-number scalar."))}
  x <- lapply(1:n, function(x) combn(n, x))                                      # : all possible combinations from size 1 to size [n]
  y <- NULL                                                                      # : initialize the result
  for (i in 1:nx(x)) {                                                           # : FOR EACH element of the list returned in [x]
    xi <- x[[i]]                                                                 # : : get that element and
    for (j in 1:nc(xi)) {y <- c(y, list(av(xi[ , j])))}                          # : : FOR EACH column (i.e., unique combination) > append as a list element to [y]
  }                                                                              # : END
  y                                                                              # : return value
}                                                                                # END

