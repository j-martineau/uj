#' @name dt
#' @encoding UTF-8
#' @family wraps
#' @title Wrappers of `data.table` functions
#' @details
#' \tabular{ll}{  `dtmerge`   \tab Thinly wraps \code{\link[data.table]{merge}}.         \cr
#'                `dtwide`    \tab Thinly wraps \code{\link[data.table]{dcast}}.         \cr
#'                `dtrows`    \tab Selects rows.                                         \cr
#'                `dtcols`    \tab Selects columns without `x[ , ..var]`.                \cr
#'                `dtsub`     \tab Selects a subtable without `x[row.var, ..col.var]`.   \cr
#'                `asdt`      \tab Thinly wraps \code{\link[data.table]{as.data.table}}. \cr
#'                `isdt`      \tab Thinly wraps \code{\link[data.table]{is.data.table}}. \cr
#'                `iedt`      \tab Convert to `data.table`, if needed.                     }
#' @param x An object for `asdt`, `isdt`, and `iedt`. A \code{\link[data.table]{data.table}} for all others.
#' @param ir,ic \link[=cmp_ind_vec]{Complete indexer vecs} or \link[=cmp_chr_vec]{complete character vecs} identifying rows and columns of `x`, respectively.
#' @param say `TRUE` or `FALSE` indicating whether to notify user of starting a potentially time-consuming process (converting to `data.table`, joining two large `data.table`s, and reshaping from long to wide).
#' @return A data.table.
#' @inherit data.table::as.data.table
#' @export
asdt <- function(x, keep.rownames = FALSE, ..., say = TRUE) {if (say) {uj::say("\n| coerce to data.table")}; data.table::as.data.table(x)}

#' @rdname dt
#' @inherit data.table::is.data.table
#' @export
isdt <- function(...) {uj::run_alias("data.table", "is.data.table")}

#' @rdname dt
#' @export
iedt <- function(x, say = TRUE) {if (uj::isdt(x)) {x} else {uj::asdt(x, say)}}

#' @rdname dt
#' @export
dtsub <- function(x, ir, ic) {x[ir , ic, with = FALSE]}

#' @rdname dt
#' @export
dtcols <- function(x, ic) {x[ , ic, with = FALSE]}

#' @rdname dt
#' @inherit data.table::merge
#' @export
dtmerge <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian = base::getOption("datatable.allow.cartesian"), say = TRUE) {
  if (say) {uj::say("\n| merge data.tables")}
  if (uj::NLL(by) & uj::NLL(by.x) & uj::NLL(by.y)) {data.table::merge.data.table(x, y,                                    all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else if (uj::DEF(by))                            {data.table::merge.data.table(x, y, by = by,                           all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else                                             {data.table::merge.data.table(x, y,          by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
}

#' @rdname dt
#' @export
dtrows <- function(x, ir) {x[ir, ]}

#' @rdname dt
#' @inherit data.table::dcast
#' @export
dtwide <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = data.table:::guess(data), verbose = base::getOption("datatable.verbose"), say = TRUE) {
  if (say) {uj::say("\n| reshape data.table from long to wide")}
  code <- uj::p0("data.table::dcast.data.table(data, formula, sep = sep, ..., drop = drop, value.var = value.var, verbose = verbose",
                 uj::f0(uj::NLL(fun.aggregate), "", ", fun.aggregate = fun.aggregate")                                              ,
                 uj::f0(uj::NLL(margins      ), "", ", margins = margins")                                                          ,
                 uj::f0(uj::NLL(subset       ), "", ", subset = subset")                                                            ,
                 uj::f0(uj::NLL(fill         ), "", ", fill = fill"), ")"                                                           )
  uj::run(code)
}
