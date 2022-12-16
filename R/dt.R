#' @name dt
#' @family wraps
#' @title Wrappers of `data.table` functions
#' @description \tabular{rl}{
#'      `dtmerge`   \tab Thinly wraps \code{\link[data.table]{merge}}.
#'   \cr `dtwide`   \tab Thinly wraps \code{\link[data.table]{dcast}}.
#'   \cr   `dtas`   \tab Thinly wraps \code{\link[data.table]{as.data.table}}.
#'   \cr   `dtis`   \tab Thinly wraps \code{\link[data.table]{is.data.table}}.
#'   \cr            \tab   
#'   \cr `dtrows`   \tab Selects rows.
#'   \cr            \tab   
#'   \cr `dtcols`   \tab Selects columns without non-standard `[` (i.e., `x[ , ..var]`).
#'   \cr            \tab   
#'   \cr  `dtsub`   \tab Selects a sub-`data.table` without non-standard `[` (i.e., `x[row.var, ..col.var]`).
#'   \cr            \tab   
#'   \cr   `dtie`   \tab Evaluates whether `x` is a `data.table`. If so, returns as is, otherwise converts to a `data.table`.
#' }
#' @param x An R object for `dtas`, `dtis`, and `dtie`. A \code{\link[data.table]{data.table}} for all others.
#' @param ir,ic \link[=cmp_ind_vec]{Complete indexer vecs} or \link[=cmp_chr_vec]{complete character vecs} identifying rows and columns of `x`, respectively.
#' @param say A non-`NA` logical scalar indicating whether to notify user of starting a potentially time-consuming process (converting to `data.table`, joining two large `data.table`s, and reshaping from long to wide).
#' @return A data.table.
#' @inherit data.table::as.data.table
#' @export
dtas <- function(x,keep.rownames = FALSE, ..., say = TRUE) {if (say) {say("\n| coerce to data.table")}; data.table::as.data.table(x)}

#' @rdname dt
#' @inherit data.table::is.data.table
#' @export
dtis <- function(x) {data.table::is.data.table(x)}

#' @rdname dt
#' @export
dtie <- function(x, say = TRUE) {if (data.table::is.data.table(x)) {x} else {dtas(x, say)}}

#' @rdname dt
#' @export
dtsub <- function(x, ir, ic) {x[ir , ic, with = FALSE]}

#' @rdname dt
#' @export
dtcols <- function(x, ic) {x[ , ic, with = FALSE]}

#' @rdname dt
#' @inherit data.table::merge
#' @export
dtmerge <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian = getOption("datatable.allow.cartesian"), say = TRUE) {
  if (say) {say("\n| merge data.tables")}
  if (is.null(by) & is.null(by.x) & is.null(by.y)) {data.table::merge.data.table(x, y,                                    all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else if (!is.null(by))                           {data.table::merge.data.table(x, y, by = by,                           all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else                                             {data.table::merge.data.table(x, y,          by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
}

#' @rdname dt
#' @export
dtrows <- function(x, ir) {x[ir, ]}

#' @rdname dt
#' @inherit data.table::dcast
#' @export
dtwide <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = data.table:::guess(data), verbose = getOption("datatable.verbose"), say = TRUE) {
  if (say) {say("\n| reshape data.table from long to wide")}
  code <- paste0(
    "data.table::dcast.data.table(data, formula, sep = sep, ..., drop = drop, value.var = value.var, verbose = verbose",
    f0(is.null(fun.aggregate), "", ", fun.aggregate = fun.aggregate"),
    f0(is.null(margins      ), "", ", margins = margins"),
    f0(is.null(subset       ), "", ", subset = subset"),
    f0(is.null(fill         ), "", ", fill = fill"), ")"
  )
  run(code)
}

