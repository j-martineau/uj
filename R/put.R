#' @name put
#' @family extensions
#' @title Put values into objects
#' @section Functions in This Family:
#'   \strong{\code{put}}
#'   \cr Put values in \code{v} into \code{e}-indexed elements of the vector or
#'   \code{\link[=ivls]{vlist}} \code{x}.
#'   \cr\cr
#'   \strong{\code{putr}}
#'   \cr Put the vector \code{v} into the \code{r}-indexed row of the matrix or
#'   data frame \code{x}.
#'   \cr\cr
#'   \strong{\code{putc}}
#'   \cr Put the vector \code{v} into the \code{c}-indexed of the matrix or data
#'   frame \code{x}.
#' @param x Object into which values will be placed. May be a \code{\link{vec}},
#'   \code{\link[ivls]{vlist}}, matrix, or data.frame.
#' @param v A vector of replacement values.
#' @param e A \link[=cmp_ind_vec]{complete indexer vec} or
#'   \link[=cmp_psw_scl]{complete positive whole-number scalar} indexing
#'   element(s) to replace.
#' @param r,c A \link[=cmp_ind_scl]{complete indexer scalar} indicating row or
#'   column to replace, respectively.
#' @return An object of the same dimension as \code{x}.
#' @export
put <- function(x, v, e) {x[e] <- v; x}

#' @rdname put
#' @export
putr <- function(x, v, r) {x[r,] <- v; x}

#' @rdname put
#' @export
putc <- function(x, v, c) {x[, c] <- v; x}

