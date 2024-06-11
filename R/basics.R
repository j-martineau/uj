#' @encoding UTF-8
#' @family wraps
#' @title Wrappers for `base` functions
#' @description Wrappers for frequently used `base` package functions.
#' @details **Basic function wrappers not related to counts/lengths**
#' \tabular{ll}{  `g`       \tab `paste0(av(...), collapse = g)` (glues `...`) \cr
#'                `p`       \tab `paste(..., sep = p)`                         \cr
#'                `g0`      \tab `paste0(av(...), collapse = "")`              \cr
#'                `p0`      \tab `paste(..., sep = "")`                        \cr
#'                `g1`      \tab `paste0(av(...), collapse = " ")`             \cr
#'                `p1`      \tab `paste(..., sep = " ")`          \cr   \tab   \cr
#'                `hi`      \tab `ceiling(x)`                                  \cr
#'                `lo`      \tab `floor(x)`                       \cr   \tab   \cr
#'                `na`      \tab `is.na(x)`                                    \cr
#'                `ok`      \tab `!is.na(x)`                      \cr   \tab   \cr
#'                `wna`     \tab `which(na(x))`                                \cr
#'                `wok`     \tab `which(ok(x))`                   \cr   \tab   \cr
#'                `len`     \tab `nchar(x)`                       \cr   \tab   \cr
#'                `mid`     \tab `substr(.)`                      \cr   \tab   \cr
#'                `spf`     \tab `sprintf(.)`                     \cr   \tab   \cr
#'                `levs`    \tab `levels(x)`                      \cr   \tab   \cr
#'                `none`    \tab `!any(av(...))`                  \cr   \tab   \cr
#'                `x_in`    \tab `x[isIN(x, ...)]`                             \cr
#'                `x_mf`    \tab `x[isMF(x, ...)]`                \cr   \tab   \cr
#'                `clabs`   \tab `colnames(x)`                                 \cr
#'                `dlabs`   \tab `...names(x)`                                 \cr
#'                `elabs`   \tab `names(x)`                                    \cr
#'                `rlabs`   \tab `rownames`                       \cr   \tab     }
#' \tabular{ll}{  `is_arr, not_arr`   \tab `(!)is.array(x)`                           \cr
#'                `is_dtf, not_dtf`   \tab `(!)is.data.frame(x)`                      \cr
#'                `is_fun, not_fun`   \tab `(!)is.function(x)`                        \cr
#'                `is_lst, not_lst`   \tab `(!)is.list(x)`                            \cr
#'                `is_mat, not_mat`   \tab `(!)is.matrix(x)`                          \cr
#'                `is_vec, not_veq`   \tab `(!)is.vector(x)`             \cr   \tab   \cr
#'                `is_atm, not_atm`   \tab `(!)is.atomic(x)`                          \cr
#'                `is_chr, not_chr`   \tab `(!)is.character(x)`                       \cr
#'                `is_fac, not_fac`   \tab `(!)is.factor(x)`                          \cr
#'                `is_int, not_int`   \tab `(!)is.integer(x)`                         \cr
#'                `is_lgl, not_lgl`   \tab `(!)is.logical(x)`                         \cr
#'                `is_num, not_num`   \tab `(!)is.numeric(x)`                         \cr
#'                `is_ord, not_ord`   \tab `(!)is.ordered(x)`                         \cr
#'                `is_uno, not_uno`   \tab `(!)is_fac(x) & (!)is_ord(x)` \cr   \tab     }
#' @examples
#' vals <- c(1:3, 2:4) / 3
#' vars <- c("a", "bb", "ccc", "dddd", "ccc", "bb")
#' text <- "%s = %0.2f and %s = %0.0f"
#' vals
#' vars
#' text
#' lo(vals)
#' hi(vals)
#' len(vars)
#' mid(vars, 1, 3)
#' spf(text, vars[1:3], vals[1:3], vars[4:6], vals[4:6])
#' ### more examples ###
#' @export
basics_help <- function() {utils::help("basics", package = "uj")}

# g (glue within) ####

#' @describeIn basics_help Calls `paste0(av(...), collapse = g)`.
#' @export
g <- function(g, ...) {base::paste(uj::av(...), collapse = g)}

#' @describeIn basics_help Calls `paste0(av(...), collapse = "")`.
#' @export
g0 <- function(...) {base::paste0(uj::av(...), collapse = "")}

#' @describeIn basics_help Calls `paste0(av(...), collapse = " ")`.
#' @export
g1 <- function(...) {base::paste0(uj::av(...), collapse = " ")}

# p (paste across) ####

#' @describeIn basics_help Calls `paste0(..., sep = p)`.
#' @export
p <- function(p, ...) {base::paste(..., sep = p)}

#' @describeIn basics_help Calls `paste0(...)`.
#' @export
p0 <- function(...) {base::paste0(...)}

#' @describeIn basics_help Calls `paste(..., sep = " ")`.
#' @export
p1 <- function(...) {base::paste(..., sep = " ")}

# special values ####

#' @describeIn basics_help A thin wrapper for \code{\link[base]{is.na}}
#' @export
na <- function(x) {base::is.na(x)}

#' @describeIn basics_help A thin wrapper for \code{\link[base]{!is.na}}
#' @export
ok <- function(x) {!base::is.na(x)}

#' @describeIn basics_help Evaluates which values of `x` are `NA`.
#' @export
wna <- function(x) {base::which(base::is.na(x))}

#' @describeIn basics_help Evaluates which values of `x` are OK (i.e., not `NA`).
#' @export
wok <- function(x) {base::which(!base::is.na(x))}

#' @describeIn basics_help Evaluates whether `x` is `NULL`.
#' @export
null <- function(x) {if (uj::is_err(x)) {T} else {base::is.null(x)}}

#' @describeIn basics_help Evaluates whether `x` is defined (i.e., not `NULL`).
#' @export
def <- function(x) {if (uj::is_err(x)) {F} else {!base::is.null(x)}}

#' @describeIn basics_help A thin wrapper for \code{\link[base]{floor}}
#' @export
lo <- function(x) {base::floor(x)}

#' @describeIn basics_help A thin wrapper for \code{\link[base]{ceiling}}
#' @export
hi <- function(x) {base::ceiling(x)}

#' @describeIn basics_help A thin wrapper for \code{\link[base]{nchar}}
#' @export
len <- function(x) {base::nchar(x)}

#' @describeIn basics_help A thin wrapper for \code{\link[base]{substr}}
#' @export
mid <- function(x, start, stop) {base::substr(x, start, stop)}

#' @describeIn basics_help A thin wrapper for \code{\link[base]{sprintf}}.
#' @export
spf <- function(fmt, ...) {base::sprintf(fmt, ...)}

#' @describeIn basics_help Gets the levels (ordered, unordered factor) `x`.
#' @export
levs <- function(x) {base::levels(x)}

#' @describeIn basics_help Evaluates whether `x` is rounded to the nearest integer.
#' @export
rounded <- function(x) {base::all(x == base::round(x))}

# names ####

#' @describeIn basics_help Gets names of columns of `x`.
#' @export
clabs <- function(x) {base::colnames(x)}

#' @describeIn basics_help Gets names of dots (`...`).
#' @export
dlabs <- function() {base::eval.parent(base::...names())}

#' @describeIn basics_help Gets names of the rows of `x`.
#' @export
rlabs <- function(x) {base::rownames(x)}

#' @describeIn basics_help Gets names of the values of `x`.
#' @export
vlabs <- function(x) {base::names(x)}

# is(nt) mode ####

#' @describeIn basics_help Evaluates whether `x` is of atomic mode.
#' @export
is_atm <- function(x) {base::is.atomic(x)}

#' @describeIn basics_help Evaluates whether `x` is of mode character.
#' @export
is_chr <- function(x) {base::is.character(x)}

#' @describeIn basics_help Evaluates whether `x` is of mode factor.
#' @export
is_fac <- function(x) {base::is.factor(x)}

#' @describeIn basics_help Evaluates whether `x` is of mode integer.
#' @export
is_int <- function(x) {base::is.integer(x)}

#' @describeIn basics_help Evaluates whether `x` is of mode logical
#' @export
is_lgl <- function(x) {base::is.logical(x)}

#' @describeIn basics_help Evaluates whether `x` is of mode numeric.
#' @export
is_num <- function(x) {base::is.numeric(x)}

#' @describeIn basics_help Evaluates whether `x` is of mode ordered factor.
#' @export
is_ord <- function(x) {base::is.ordered(x)}

#' @describeIn basics_help Evaluates whether `x` is of mode unordered factor.
#' @export
is_uno <- function(x) {base::is.factor(x) & !base::is.ordered(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* atomic.
#' @export
not_atm <- function(x) {!base::is.atomic(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* of mode character
#' @export
not_chr <- function(x) {!base::is.character(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* of mode factor.
#' @export
not_fac <- function(x) {!base::is.factor(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* of mode integer.
#' @export
not_int <- function(x) {!base::is.integer(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* of mode logical.
#' @export
not_lgl <- function(x) {!base::is.logical(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* of mode numeric.
#' @export
not_num <- function(x) {!base::is.numeric(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* of mode ordered factor.
#' @export
not_ord <- function(x) {!base::is.ordered(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* of mode unordered factor.
#' @export
not_uno <- function(x) {!base::is.factor(x) | base::is.ordered(x)}

# is(n't) class ####

#' @describeIn basics_help Evaluates whether `x` is an array.
#' @export
is_arr <- function(x) {base::is.array(x)}

#' @describeIn basics_help Evaluates whether `x` is a data.frame.
#' @export
is_dtf <- function(x) {base::is.data.frame(x)}

#' @describeIn basics_help Evaluates whether `x` is a function.
#' @export
is_fun <- function(x) {base::is.function(x)}

#' @describeIn basics_help Evaluates whether `x` is a list.
#' @export
is_lst <- function(x) {base::is.list(x)}

#' @describeIn basics_help Evaluates whether `x` is a matrix.
#' @export
is_mat <- function(x) {base::is.matrix(x)}

#' @describeIn basics_help Evaluates whether `x` is a vector.
#' @export
is_vec <- function(x) {base::is.vector(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* an array
#' @export
not_arr <- function(x) {!base::is.array(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* a data.frame.
#' @export
not_dtf <- function(x) {!base::is.data.frame(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* a function.
#' @export
not_fun <- function(x) {!base::is.function(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* a list.
#' @export
not_lst <- function(x) {!base::is.list(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* a matrix.
#' @export
not_mat <- function(x) {!base::is.matrix(x)}

#' @describeIn basics_help Evaluates whether `x` is *not* a vector.
#' @export
not_vec <- function(x) {!base::is.vector(x)}

# subsets ####

#' @describeIn basics_help Evaluates whether elements of `x` are contained in the atomic elements of `...`.
#' @export
x_in <- function(x, ...) {x[uj::is_in(x, ...)]}

#' @describeIn basics_help Evaluates whether elements of `x` are missing from the atomic elements of `...`.
#' @export
x_mf <- function(x, ...) {x[uj::is_mf(x, ...)]}
