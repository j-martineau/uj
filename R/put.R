#' @name put
#' @title Put Values into Objects
#' @description Place values into indexed elements of vector or list \code{x}.
#' @param x Object into which values will be placed. May be a vector, list,
#'   matrix, or data.frame.
#' @param rep Vector of replacement values.
#' @param elt Logical vector, integer vector, or integer scalar indexing
#'   element(s) to replace.
#' @param row Logical vector or integer scalar indexing row to replace.
#' @param col Logical vector or integer scalar indexing column to replace.
#' @return An object of the same dimension as \code{x}.
#' @export
put <- function(x, rep, elt) {x[elt] <- rep; x}

#' @describeIn put Place vector into indexed row of matrix or data frame
#'   \code{x}.
#' @export
putr <- function(x, rep, row) {x[row, ] <- rep; x}

#' @describeIn put Place vector into indexed col of matrix or data frame
#'   \code{x}.
#' @export
putc <- function(x, rep, col) {x[ , col] <- rep; x}

