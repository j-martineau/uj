#' @family extensions
#' @title Put values into objects
#' @description (These functions are not error checked.)
#' \itemize{
#'   \item **`put`**: puts values of `v` into `e`-indexed elements of `x` (assumed to be a vector or \link[=ccc]{vlist}).
#'   \item **`putc`**: puts the vector `v` into the `c`-indexed column of `x` (assumed to be a data.frame or matrix).
#'   \item **`putr`**: puts the vector `v` into the `r`-indexed row of `x` (assumed to be a data.frame or matrix).
#'  }
#' @param x For `put`, a vector or \link[=ccc]{vlist}. For `putc` and `putr`, a data.frame or matrix.
#' @param v A vector of replacement values. Must be of the same length as `e` for `put`. Length must equal the number of `nrow(x)` for `putc`. Length must equal the `ncol(x)` for `putr`
#' @param e A \link[=cmp_ind_vec]{complete indexer vec} of length equal to that of `v`.
#' @param r,c \link[=cmp_ind_scl]{Complete indexer scalars} indicating row or column to replace, respectively.
#' @return An object of the same class and dimension as `x`.
#' @export
put <- function(x, v, e) {x[e] <- v; x}

#' @rdname put
#' @export
putr <- function(x, v, r) {x[r,] <- v; x}

#' @rdname put
#' @export
putc <- function(x, v, c) {x[, c] <- v; x}

