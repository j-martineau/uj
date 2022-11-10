#' @name put.
#' @title Put values into objects
#' @family extensions
#' @param x Object into which values will be placed. May be a vector, list,
#'   matrix, or data.frame.
#' @param v Vector of replacement values.
#' @param e \link[cmp_ind_vec]{Complete indexer vec} or
#'   \link[cmp_psw_scl]{complete positive whole-number scalar} indexing
#'   element(s) to replace.
#' @param r,c \link[cmp_ind_scl]{Complete indexer scalar} indicating row or
#'   column to replace, respectively.
#' @return An object of the same dimension as \code{x}.
#' @export
put. <- function() {help("put.", package = "uj")}

#' @describeIn put. Put values in \code{v} into \code{e}-indexed elements of
#'   the vector or list \code{x}.
#' @export
put <- function(x, v, e) {x[e] <- v; x}

#' @describeIn put. Put the vector \code{v} into the \code{r}-indexed row of
#'   the matrix or data frame \code{x}.
#' @export
putr <- function(x, v, r) {x[r,] <- v; x}

#' @describeIn put. Put the vector \code{v} into the \code{c}-indexed of the
#'   matrix or data frame \code{x}.
#' @export
putc <- function(x, v, c) {x[, c] <- v; x}

