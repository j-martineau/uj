#' @name dplyr_tools
#' @title Aliases and wrappers for functions in package \code{dplyr}.
#' @description Alias for \code{\link[dplyr]{n}} (frequency).
#' @export
dpn <- dplyr::n

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{all_of}} (strict
#'   selection; throws error with mismatched names).
#' @export
dpall <- dplyr::all_of

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{mutate}} (create new
#'   variables).
#' @export
dpmod <- dplyr::mutate

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{summarize}}
#'   (aggregation).
#' @export
dpsum <- dplyr::summarize

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{left_join}}
#'   (merge variables from \code{y} into rows of \code{x}).
#' @export
dpjoin <- dplyr::left_join

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{count}}
#'   (counting observations).
#' @export
dpcount <- dplyr::count

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{inner_join}} (merge
#'   variables from \code{y} into rows of \code{x} and variables from \code{x}
#'   into rows of \code{y}).
#' @export
dpinner <- dplyr::inner_join

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{ungroup}} (undo case
#'   groupings).
#' @export
dpungrp <- dplyr::ungroup

#' @describeIn dplyr_tools Alias for \code{\link[dplyr]{rename}} (rename
#'   variables).
#' @export
dprename <- dplyr::rename

#' @describeIn dplyr_tools Limited functionality thin wrapper for
#'   \code{\link[dplyr]{group_by}} (group cases for group-based analyses).
#' @export
dpgrp <- function(x, keys) {run("dplyr::group_by(x, ", dw(keys, w. = v(comma)), ")")}
