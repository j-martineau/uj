#' @name dp
#' @encoding UTF-8
#' @family wraps
#' @title Thin wrappers of `dplyr` functions
#' @description \tabular{rl}{
#'      `dpjoin` \tab   Thinly wraps \code{\link[dplyr]{left_join}}.
#'   \cr `dpagg` \tab   Thinly wraps \code{\link[dplyr]{summarize}}.
#'   \cr `dpall` \tab   Thinly wraps \code{\link[dplyr]{all_of}}.
#'   \cr `dplab` \tab   Thinly wraps \code{\link[dplyr]{rename}}.
#'   \cr `dpmod` \tab   Thinly wraps \code{\link[dplyr]{mutate}}.
#'   \cr `dpgrp` \tab   Limitedly wraps \code{\link[dplyr]{group_by}}\eqn{^a}.
#'   \cr   `dpn` \tab   Thinly wraps \code{\link[dplyr]{n}}.
#' }
#' \eqn{^{a.}} Accepts as argument only a data.frame (`x`) and a character vector naming grouping variables (`keys`).
#' @param x A data.frame
#' @param keys A \link[=cmp_chr_vec]{complete character vec} naming grouping variables in `x`.
#' @return A data.frame.
#' @inherit dplyr::summarize
#' @export
dpagg <- function(.data, ..., .groups = NULL) {dplyr::summarize(.data, ..., .groups = .groups)}

#' @rdname dp
#' @inherit dplyr::n
#' @export
dpn <- function() {dplyr::n()}

#' @rdname dp
#' @inherit dplyr::all_of
#' @export
dpall <- function(x) {dplyr::all_of(x)}

#' @rdname dp
#' @export
dpgrp <- function(x, keys) {run("dplyr::group_by(x, ", paste0(keys, collapse = ", "), ")")}

#' @rdname dp
#' @inherit dplyr::left_join
#' @export
dpjoin <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE) {dplyr::left_join(x, y, by - by, copy = copy, suffix = suffix, ..., keep = keep)}

#' @rdname dp
#' @inherit dplyr::rename
#' @export
dplab <- function(.data, ...) {dplyr::rename(.data, ...)}

#' @rdname dp
#' @inherit dplyr::mutate
#' @export
dpmod <- function(.data, ...) {dplyr::mutate(.data, ...)}
