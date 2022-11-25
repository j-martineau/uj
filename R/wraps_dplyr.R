#' @name wraps_dplyr
#' @family wraps
#' @title Wraps of Functions from Package `dplyr`
#' @description The following table describes the wraps this group of functions
#'   covers:\tabular{ll}{
#'     WRAPPER    \tab DPLYR FUNCTION                                   \cr
#'     `dpagg`    \tab \code{\link[dplyr]{summarize}}                        \cr
#'     `dpall`    \tab \code{\link[dplyr]{all_of}}                           \cr
#'     `dpjoin`   \tab \code{\link[dplyr]{left_join}}                        \cr
#'     `dplab`    \tab \code{\link[dplyr]{rename}}                           \cr
#'     `dpn`      \tab \code{\link[dplyr]{n}}                                \cr
#'     `dpmod`    \tab \code{\link[dplyr]{mutate}}                             }
#'   The function `dpgrp` wraps `dplyr::group_by`, but with limited
#'   functionality: Limited to a data.frame (`x`) and a character vector naming
#'   grouping variables (`keys`).
#' @param x A data.frame
#' @return A data.frame.
#' @inherit dplyr::summarize
#' @export
dpagg <- function(.data, ..., .groups = NULL) {dplyr::summarize(.data, ..., .groups = .groups)}

#' @rdname wraps_dplyr
#' @inherit dplyr::n
#' @export
dpn <- function() {dplyr::n()}

#' @rdname wraps_dplyr
#' @inherit dplyr::all_of
#' @export
dpall <- function(x) {dplyr::all_of(x)}

#' @rdname wraps_dplyr
#' @param keys A character vector naming grouping variables in \code{x}.
#' @export
dpgrp <- function(x, keys) {run("dplyr::group_by(x, ", paste0(keys, collapse = ", "), ")")}

#' @rdname wraps_dplyr
#' @inherit dplyr::left_join
#' @export
dpjoin <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE) {dplyr::left_join(x, y, by - by, copy = copy, suffix = suffix, ..., keep = keep)}

#' @rdname wraps_dplyr
#' @inherit dplyr::rename
#' @export
dplab <- function(.data, ...) {dplyr::rename(.data, ...)}

#' @rdname wraps_dplyr
#' @inherit dplyr::mutate
#' @export
dpmod <- function(.data, ...) {dplyr::mutate(.data, ...)}
