#' @title Get All Possible Non-Empty Sets
#' @description Create a list containing elements of indices to create all
#'   possible sets of 1 or more elements of an object of size \code{n}.
#' @export
sets <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  x <- lapply(1:n, function(x) combn(n, x))                                      # : all possible combinations from size 1 to size [n]
  y <- NULL                                                                      # : initialize the result
  for (i in 1:nx(x)) {                                                           # : FOR EACH element of the list returned in [x]
    xi <- x[[i]]                                                                 # : : get that element and
    for (j in 1:nc(xi)) {y <- c(y, list(av(xi[ , j])))}                          # : : FOR EACH column (i.e., unique combination) > append as a list element to [y]
  }                                                                              # : END
  y                                                                              # : return value
}                                                                                # END

