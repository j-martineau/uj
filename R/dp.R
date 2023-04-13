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
dp <- function() {utils::help("dp", package = "uj")}

#' @rdname dp
#' @export
dp_agg <- dplyr::summarize

#' @rdname dp
#' @export
dp_n <- dplyr::n

#' @rdname dp
#' @export
dp_all <- dplyr::all_of

#' @rdname dp
#' @export
dp_grp <- function(X, Keys) {uj::run("dplyr::group_by(X, ", base::paste0(Keys, collapse = ", "), ")")}

#' @rdname dp
#' @export
dp_join <- dplyr::left_join

#' @rdname dp
#' @export
dp_lab <- dplyr::rename

#' @rdname dp
#' @export
dp_mod <- dplyr::mutate
