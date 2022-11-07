#' @name put.
#' @title Put values into objects
#' @family extensions
#' @param x Object into which values will be placed. May be a vector, list,
#'   matrix, or data.frame.
#' @param v Vector of replacement values.
#' @param e Logical vector, integer vector, or integer scalar indexing
#'   element(s) to replace.
#' @param r Logical vector or integer scalar indexing row to replace.
#' @param c Logical vector or integer scalar indexing column to replace.
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

