#' @name dp
#' @encoding UTF-8
#' @family wraps
#' @title Thin wrappers of `dplyr` functions
#' @details
#' \tabular{ll}{  `dp_join`   \tab Thinly wraps \code{\link[dplyr]{left_join}}.               \cr
#'                `dp_agg`    \tab Thinly wraps \code{\link[dplyr]{summarize}}.               \cr
#'                `dp_all`    \tab Thinly wraps \code{\link[dplyr]{all_of}}.                  \cr
#'                `dp_lab`    \tab Thinly wraps \code{\link[dplyr]{rename}}.                  \cr
#'                `dp_mod`    \tab Thinly wraps \code{\link[dplyr]{mutate}}.                  \cr
#'                `dp_grp`    \tab Limitedly wraps \code{\link[dplyr]{group_by}}\eqn{^{(1)}}  \cr
#'                `dp_n`      \tab Thinly wraps \code{\link[dplyr]{n}}.                         }
#'  \tabular{l}{  \eqn{^{(1)}} Accepts as argument only a data.frame (`x`) and a character vector naming grouping variables (`keys`).}
#' @param x A data.frame
#' @param keys A \link[=cmp_chr_vec]{complete character vec} naming grouping variables in `x`.
#' @return A `data.frame`.
#' @export
dp_agg <- function(.data, ..., .groups = NULL) {dplyr::summarize(.data, ..., .groups = .groups)}

#' @rdname dp
#' @export
dp_n <- function(...) {uj::run_alias("dplyr", "n")}

#' @rdname dp
#' @export
dp_all <- function(...) {uj::run_alias("dplyr", "all_of")}

#' @rdname dp
#' @export
dp_grp <- function(x, keys) {uj::run("dplyr::group_by(x, ", base::paste0(keys, collapse = ", "), ")")}

#' @rdname dp
#' @export
dp_join <- function(...) {uj::run_alias("dplyr", "left_join")}

#' @rdname dp
#' @export
dp_lab <- function(...) {uj::run_alias("dplyr", "rename")}

#' @rdname dp
#' @export
dp_mod <- function(...) {uj::run_alias("dplyr", "mutate")}
