#' @encoding UTF-8
#' @family function_form
#' @title Put values into objects
#' @description Put values into a vector/list, vectors into rows, and vectors into columns.
#' @details
#' \tabular{ll}{  `put`    \tab Puts vector `v` into `e`-indexed elements of `x` (assumed to be a vector or \link[=VLS]{vlist}). \cr   \tab     }
#' \tabular{ll}{  `putc`   \tab Puts vector `v` into `c`-indexed column of `x` (assumed to be a data.frame or matrix).           \cr   \tab   \cr
#'                `putr`   \tab Puts vector `v` into `r`-indexed row of `x` (assumed to be a data.frame or matrix).                             }
#' @param x For `put`, a vector or \link[=ccc]{vlist}. For `putc` and `putr`, a data.frame or matrix.
#' @param v A vector of replacement values. Must be of the same length as `e` for `put`. Length must equal the number of `nrow(x)` for `putc`. Length must equal `ncol(x)` for `putr`
#' @param e A \link[=cmp_ind_vec]{complete indexer vec} of length equal to that of `v`.
#' @param r,c \link[=cmp_ind_scl]{Complete indexer scalars} indicating row or column to replace, respectively.
#' @return An object of the same class and dimension as `x`.
#' @examples
#' egVec <- letters[1:5]
#' egVls <- list(a = letters[1:5], b = 1:5)
#' egMat <- matrix(1:25, nrow = 5)
#' egDtf <- data.frame(a = letters[1:5], b = 1:5, stringsAsFactors = FALSE)
#'
#' egVec
#' egVls
#' egMat
#' egDtf
#'
#' put(egVec, "z", 1:2)
#' put(egVls, 7, 2)
#' putr(egMat, 1:5, 1)
#' putc(egDtf, 1:5, 1)
#' @export
put <- function(x, v, e) {x[e] <- v; x}

#' @rdname put
#' @export
putr <- function(x, v, r) {x[r, ] <- v; x}

#' @rdname put
#' @export
putc <- function(x, v, c) {x[, c] <- v; x}

