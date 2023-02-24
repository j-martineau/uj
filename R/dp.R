#' @name dp
#' @encoding UTF-8
#' @family wraps
#' @title Thin wrappers of `dplyr` functions
#' @details
#' \tabular{ll}{  `dpjoin`   \tab Thinly wraps \code{\link[dplyr]{left_join}}.               \cr
#'                `dpagg`    \tab Thinly wraps \code{\link[dplyr]{summarize}}.               \cr
#'                `dpall`    \tab Thinly wraps \code{\link[dplyr]{all_of}}.                  \cr
#'                `dplab`    \tab Thinly wraps \code{\link[dplyr]{rename}}.                  \cr
#'                `dpmod`    \tab Thinly wraps \code{\link[dplyr]{mutate}}.                  \cr
#'                `dpgrp`    \tab Limitedly wraps \code{\link[dplyr]{group_by}}\eqn{^{(1)}}  \cr
#'                `dpn`      \tab Thinly wraps \code{\link[dplyr]{n}}.                         }
#'  \tabular{l}{  \eqn{^{(1)}} Accepts as argument only a data.frame (`x`) and a character vector naming grouping variables (`keys`).}
#' @param x A data.frame
#' @param keys A \link[=cmp_chr_vec]{complete character vec} naming grouping variables in `x`.
#' @return A `data.frame`.
#' @export
dpagg <- function(.data, ..., .groups = NULL) {dplyr::summarize(.data, ..., .groups = .groups)}

#' @rdname dp
#' @export
dpn <- function(...) {uj::run_alias("dplyr", "n")}

#' @rdname dp
#' @export
dpall <- function(...) {uj::run_alias("dplyr", "all_of")}

#' @rdname dp
#' @export
dpgrp <- function(x, keys) {uj::run("dplyr::group_by(x, ", uj::g(", ", keys), ")")}

#' @rdname dp
#' @export
dpjoin <- function(...) {uj::run_alias("dplyr", "left_join")}

#' @rdname dp
#' @export
dplab <- function(...) {uj::run_alias("dplyr", "rename")}

#' @rdname dp
#' @export
dpmod <- function(...) {uj::run_alias("dplyr", "mutate")}
