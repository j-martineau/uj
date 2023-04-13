#' @encoding UTF-8
#' @family function_form
#' @title Put values into objects
#' @description Put values into a vector/list, vectors into rows, and vectors into columns.
#' @details
#' \tabular{ll}{  `put`    \tab Puts vector `V` into `E`-indexed elements of `X` (assumed to be a vector or \link[=VLS]{vlist}). \cr   \tab   \cr
#'                `putc`   \tab Puts vector `V` into `C`-indexed column of `X` (assumed to be a data.frame or matrix).           \cr   \tab   \cr
#'                `putr`   \tab Puts vector `V` into `R`-indexed row of `X` (assumed to be a data.frame or matrix).                             }
#' @param X For `put`, a vector or \link[=ccc]{vlist}. For `putc` and `putr`, a data.frame or matrix.
#' @param V A vector of replacement values. Must be of the same length as `E` for `put`. Length must equal the number of `nrow(X)` for `putc`. Length must equal `ncol(X)` for `putr`
#' @param E A \link[=cmp_ind_vec]{complete indexer vec} of length equal to that of `V`.
#' @param R,C \link[=cmp_ind_scl]{Complete indexer scalars} indicating row or column to replace, respectively.
#' @return An object of the same class and dimension as `X`.
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
put <- function(X, V, E) {X[E] <- V; X}

#' @rdname put
#' @export
putr <- function(X, V, R) {X[R, ] <- V; X}

#' @rdname put
#' @export
putc <- function(X, V, C) {X[, C] <- V; X}

