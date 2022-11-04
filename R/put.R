#' @name put_uj
#' @title Put Values into Objects
#' @family meta
#' @param x Object into which values will be placed. May be a vector, list,
#'   matrix, or data.frame.
#' @param rep Vector of replacement values.
#' @param elt Logical vector, integer vector, or integer scalar indexing
#'   element(s) to replace.
#' @param row Logical vector or integer scalar indexing row to replace.
#' @param col Logical vector or integer scalar indexing column to replace.
#' @return An object of the same dimension as \code{x}.
#' @export
put_uj <- function() {help("put_uj", package = "uj")}

#' @describeIn put_uj Put values into indexed elements of vector or list
#'   \code{x}.
#' @export
put <- function(x, rep, elt) {x[elt] <- rep; x}

#' @describeIn put_uj Put vector into indexed row of matrix or data frame
#'   \code{x}.
#' @export
putr <- function(x, rep, row) {x[row, ] <- rep; x}

#' @describeIn put_uj Put vector into indexed col of matrix or data frame
#'   \code{x}.
#' @export
putc <- function(x, rep, col) {x[ , col] <- rep; x}

