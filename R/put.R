#' @name put
#' @family extensions
#' @title Put values into objects
#' @description \tabular{ll}{
#'   FUNCTION   \tab WHAT IT DOES                                            \cr
#'   `put`      \tab Put values of `v` into `e`-indexed elements of the vector
#'                   or \link[=ivls]{vlist} `x`.                             \cr
#'   `putr`     \tab Put the vector `v` into the `r`-indexed row of the matrix
#'                   or data frame `x`.                                      \cr
#'   `putc`     \tab or \link[=ivls]{vlist} `x`.                             \cr
#'   `putr`     \tab Put the vector `v` into the `c`-indexed column of the
#'                   matrix or data frame `x`.                                 }
#' @param x Object into which values will be placed. May be a \link[=ivec]{vec},
#'  \link[ivls]{vlist}, matrix, or data.frame.
#' @param v A vector of replacement values.
#' @param e A \link[=cmp_ind_vec]{complete indexer vec} or
#'   \link[=cmp_psw_scl]{complete positive whole-number scalar} indexing
#'   element(s) to replace.
#' @param r,c A \link[=cmp_ind_scl]{complete indexer scalar} indicating row or
#'   column to replace, respectively.
#' @return An object of the same class and dimension as `x`.
#' @export
put <- function(x, v, e) {x[e] <- v; x}

#' @rdname put
#' @export
putr <- function(x, v, r) {x[r,] <- v; x}

#' @rdname put
#' @export
putc <- function(x, v, c) {x[, c] <- v; x}

