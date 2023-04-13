#' @name dt
#' @encoding UTF-8
#' @family wraps
#' @title Wrappers of `data.table` functions
#' @details
#' \tabular{ll}{  `dt_merge`   \tab Thinly wraps \code{\link[data.table]{merge}}.         \cr
#'                `dt_wide`    \tab Thinly wraps \code{\link[data.table]{dcast}}.         \cr
#'                `dt_rows`    \tab Selects rows.                                         \cr
#'                `dt_cols`    \tab Selects columns without `X[ , ..var]`.                \cr
#'                `dt_sub`     \tab Selects a subtable without `X[row.var, ..col.var]`.   \cr
#'                `as_dt`      \tab Thinly wraps \code{\link[data.table]{as.data.table}}. \cr
#'                `is_dt`      \tab Thinly wraps \code{\link[data.table]{is.data.table}}. \cr
#'                `ie_dt`      \tab Convert to `data.table`, if needed.                     }
#' @param X An object for `as_dt`, `is_dt`, and `ie_dt`. A \code{\link[data.table]{data.table}} for all others.
#' @param x,y Data frames to be merged.
#' @param R,C \link[=cmp_ind_vec]{Complete indexer vecs} or \link[=cmp_chr_vec]{complete character vecs} identifying rows and columns of `X`, respectively.
#' @param .say Logical scalar indicating whether to update user on progress.
#' @return A data.table.
#' @export
as_dt <- data.table::as.data.table

#' @rdname dt
#' @export
is_dt <- data.table::is.data.table

#' @rdname dt
#' @export
ie_dt <- function(X) {if (data.table::is.data.table(X)) {X} else {data.table::as.data.table(X)}}

#' @rdname dt
#' @export
dt_sub <- function(X, R, C) {X[R , C, with = FALSE]}

#' @rdname dt
#' @export
dt_cols <- function(X, C) {X[ , C, with = FALSE]}

#' @rdname dt
#' @inherit data.table::merge
#' @export
dt_merge <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian = base::getOption("datatable.allow.cartesian"), .say = TRUE) {
  if (.say) {uj::say("wait", Sub = 2)}
  if (base::is.null(by) & base::is.null(by.x) & base::is.null(by.y)) {Data <- data.table::merge.data.table(x, y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else if (!base::is.null(by)) {Data <- data.table::merge.data.table(x, y, by = by, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else {Data <- data.table::merge.data.table(x, y, by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  if (.say) {uj::say("done", Sub = 2)}
  Data
}

#' @rdname dt
#' @export
dt_rows <- function(X, R) {X[R, ]}

#' @rdname dt
#' @inherit data.table::dcast
#' @export
dt_wide <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = data.table:::guess(data), verbose = base::getOption("datatable.verbose"), Say = TRUE) {
  if (Say) {uj::say("wait", Sub = 2)}
  Code <- base::paste0("data.table::dcast.data.table(data, formula, sep = sep, ..., drop = drop, value.var = value.var, verbose = verbose",
                       uj::f0(base::is.null(fun.aggregate), "", ", fun.aggregate = fun.aggregate"),
                       uj::f0(base::is.null(margins), "", ", margins = margins"),
                       uj::f0(base::is.null(subset), "", ", subset = subset"),
                       uj::f0(base::is.null(fill), "", ", fill = fill"), ")")
  data <- uj::run(Code)
  if (Say) {uj::say("done", Sub = 2)}
  data
}
