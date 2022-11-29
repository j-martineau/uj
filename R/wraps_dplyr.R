#' @name wraps_dplyr
#' @family wraps
#' @title Thin wrappers of `dplyr` functions
#' @description \itemize{
#'   \item **`dpn`**: thinly wraps \code{\link[dplyr]{n}}.
#'   \item **`dpagg`** thinly wraps \code{\link[dplyr]{summarize}}.
#'   \item **`dpall`**: thinly wraps \code{\link[dplyr]{all_of}}.
#'   \item **`dplab`**: thinly wraps \code{\link[dplyr]{rename}}.
#'   \item **`dpmod`**: thinly wraps \code{\link[dplyr]{mutate}}.
#'   \item **`dpjoin`**: thinly wraps \code{\link[dplyr]{left_join}}.
#'   \item **`dpgrp`**: wraps \code{\link[dplyr]{group_by}} with limited functionality, accepting only a data.frame (`x`) and a character vector naming grouping variables (`keys`).
#' }
#' @param x A data.frame
#' @param keys A \link[=cmp_chr_vec]{complete character vec} naming grouping variables in `x`.
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
