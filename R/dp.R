#' @name wrap_dplyr
#' @family wraps
#' @title Wraps of functions from package \code{dplyr}.
#' @description The following table describes the wraps this group of functions
#'   covers:\tabular{ll}{
#'   WRAPPER         \tab FUNCTION                                           \cr
#'   \code{dpagg}    \tab \code{\link[dplyr]{summarize}}                     \cr
#'   \code{dpall}    \tab \code{\link[dplyr]{all_of}}                        \cr
#'   \code{dpjoin}   \tab \code{\link[dplyr]{left_join}}                     \cr
#'   \code{dplab}    \tab \code{\link[dplyr]{rename}}                        \cr
#'   \code{dpn}      \tab \code{\link[dplyr]{n}}                             \cr
#'   \code{dpmod}    \tab \code{\link[dplyr]{mutate}}                          }
#'   The function \code{dpgrp} wraps  \code{\link[dplyr]{group_by}}, but with
#'   limited functionality as explained in the \emph{Functions} section.
#' @return A data.frame.
#' @export
wrap_dplyr <- NULL

#' @describeIn wrap_dplyr Thin wrap aggregation with
#'   \code{\link[dplyr]{summarize}}.
#' @inherit dplyr::summarize
#' @export
dpagg <- function(.data, ..., .groups = NULL) {dplyr::summarize(.data, ..., .groups = .groups)}

#' @describeIn wrap_dplyr Thin wrap of \code{\link[dplyr]{n}}.
#' @inherit dplyr::n
#' @export
dpn <- function() {dplyr::n()}

#' @describeIn wrap_dplyr Thin wrap of \code{\link[dplyr]{all_of}}.
#' @inherit dplyr::all_of
#' @export
dpall <- function(x) {dplyr::all_of(x)}

#' @describeIn wrap_dplyr Limited functionality wrap of
#'   \code{\link[dplyr]{group_by}} Limited to a \code{\link[idtf]{dtf}}
#'   (\code{x}) and a character vector naming grouping variables (\code{keys}).
#' @param keys A character vector naming grouping variables in \code{x}.
#' @export
dpgrp <- function(x, keys) {run("dplyr::group_by(x, ", paste0(keys, collapse = ", "), ")")}

#' @describeIn wrap_dplyr Thin wrap of \code{\link[dplyr]{left_join}}.
#' @inherit dplyr::left_join
#' @export
dpjoin <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE) {dplyr::left_join(x, y, by - by, copy = copy, suffix = suffix, ..., keep = keep)}

#' @describeIn wrap_dplyr Thin wrap of \code{\link[dplyr]{rename}}.
#' @inherit dplyr::rename
#' @export
dplab <- function(.data, ...) {dplyr::rename(.data, ...)}

#' @describeIn wrap_dplyr Thin wrap of \code{\link[dplyr]{mutate}}.
#' @inherit dplyr::mutate
#' @export
dpmod <- function(.data, ...) {dplyr::mutate(.data, ...)}
