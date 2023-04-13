#' @encoding UTF-8
#' @name basics
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
#'                `hi`      \tab `ceiling(X)`                                  \cr
#'                `lo`      \tab `floor(X)`                       \cr   \tab   \cr
#'                `na`      \tab `is.na(X)`                                    \cr
#'                `ok`      \tab `!is.na(X)`                      \cr   \tab   \cr
#'                `wna`     \tab `which(na(X))`                                \cr
#'                `wok`     \tab `which(ok(X))`                   \cr   \tab   \cr
#'                `len`     \tab `nchar(X)`                       \cr   \tab   \cr
#'                `mid`     \tab `substr(.)`                      \cr   \tab   \cr
#'                `spf`     \tab `sprintf(.)`                     \cr   \tab   \cr
#'                `levs`    \tab `levels(X)`                      \cr   \tab   \cr
#'                `none`    \tab `!any(av(...))`                  \cr   \tab   \cr
#'                `x_in`    \tab `X[isIN(X, ...)]`                             \cr
#'                `x_mf`    \tab `X[isMF(X, ...)]`                \cr   \tab   \cr
#'                `clabs`   \tab `colnames(X)`                                 \cr
#'                `dlabs`   \tab `...names(X)`                                 \cr
#'                `elabs`   \tab `names(X)`                                    \cr
#'                `rlabs`   \tab `rownames`                       \cr   \tab     }
#' \tabular{ll}{  `is_arr, not_arr`   \tab `(!)is.array(X)`                           \cr
#'                `is_dtf, not_dtf`   \tab `(!)is.data.frame(X)`                      \cr
#'                `is_fun, not_fun`   \tab `(!)is.function(X)`                        \cr
#'                `is_lst, not_lst`   \tab `(!)is.list(X)`                            \cr
#'                `is_mat, not_mat`   \tab `(!)is.matrix(X)`                          \cr
#'                `is_vec, not_veq`   \tab `(!)is.vector(X)`             \cr   \tab   \cr
#'                `is_atm, not_atm`   \tab `(!)is.atomic(X)`                          \cr
#'                `is_chr, not_chr`   \tab `(!)is.character(X)`                       \cr
#'                `is_fac, not_fac`   \tab `(!)is.factor(X)`                          \cr
#'                `is_int, not_int`   \tab `(!)is.integer(X)`                         \cr
#'                `is_lgl, not_lgl`   \tab `(!)is.logical(X)`                         \cr
#'                `is_num, not_num`   \tab `(!)is.numeric(X)`                         \cr
#'                `is_ord, not_ord`   \tab `(!)is.ordered(X)`                         \cr
#'                `is_uno, not_uno`   \tab `(!)is_fac(X) & (!)is_ord(X)` \cr   \tab     }
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
basics <- function() {utils::help("basics", package = "uj")}

# g (glue within) ####

#' @rdname basics
#' @export
g <- function(G, ...) {base::paste(uj::av(...), collapse = G)}

#' @rdname basics
#' @export
g0 <- function(...) {base::paste0(uj::av(...), collapse = "")}

#' @rdname basics
#' @export
g1 <- function(...) {base::paste0(uj::av(...), collapse = " ")}

# p (paste across) ####

#' @rdname basics
#' @inherit base::paste
#' @export
p <- function(P, ...) {base::paste(..., sep = P)}

#' @rdname basics
#' @export
p0 <- function(...) {base::paste0(...)}

#' @rdname basics
#' @export
p1 <- function(...) {base::paste(..., sep = " ")}

# special values ####

#' @rdname basics
#' @export
na <- function(X) {base::is.na(X)}

#' @rdname basics
#' @export
ok <- function(X) {!base::is.na(X)}

#' @rdname basics
#' @export
wna <- function(X) {base::which(base::is.na(X))}

#' @rdname basics
#' @export
wok <- function(X) {base::which(!base::is.na(X))}

#' @rdname basics
#' @export
null <- function(X) {if (uj::is_err(X)) {T} else {base::is.null(X)}}

#' @rdname basics
#' @export
def <- function(X) {if (uj::is_err(X)) {F} else {!base::is.null(X)}}

#' @rdname basics
#' @export
lo <- function(X) {base::floor(X)}

#' @rdname basics
#' @export
hi <- function(X) {base::ceiling(X)}

#' @rdname basics
#' @export
len <- function(X) {base::nchar(X)}

#' @rdname basics
#' @export
mid <- function(X, Start, Stop) {base::substr(X, Start, Stop)}

#' @rdname basics
#' @export
spf <- function(Fmt, ...) {base::sprintf(Fmt, ...)}

#' @rdname basics
#' @export
levs <- function(X) {base::levels(X)}

#' @rdname basics
#' @export
rounded <- function(X) {base::all(X == base::round(X))}

# names ####

#' @rdname basics
#' @export
clabs <- function(X) {base::colnames(X)}

#' @rdname basics
#' @export
dlabs <- function() {base::eval.parent(base::...names())}

#' @rdname basics
#' @export
rlabs <- function(X) {base::rownames(X)}

#' @rdname basics
#' @export
vlabs <- function(X) {base::names(X)}

# is(nt) class ####

#' @rdname basics
#' @export
is_atm <- function(X) {base::is.atomic(X)}

#' @rdname basics
#' @export
is_chr <- function(X) {base::is.character(X)}

#' @rdname basics
#' @export
is_fac <- function(X) {base::is.factor(X)}

#' @rdname basics
#' @export
is_int <- function(X) {base::is.integer(X)}

#' @rdname basics
#' @export
is_lgl <- function(X) {base::is.logical(X)}

#' @rdname basics
#' @export
is_num <- function(X) {base::is.numeric(X)}

#' @rdname basics
#' @export
is_ord <- function(X) {base::is.ordered(X)}

#' @rdname basics
#' @export
is_uno <- function(X) {base::is.factor(X) & !base::is.ordered(X)}

#' @rdname basics
#' @export
not_atm <- function(X) {!base::is.atomic(X)}

#' @rdname basics
#' @export
not_chr <- function(X) {!base::is.character(X)}

#' @rdname basics
#' @export
not_fac <- function(X) {!base::is.factor(X)}

#' @rdname basics
#' @export
not_int <- function(X) {!base::is.integer(X)}

#' @rdname basics
#' @export
not_lgl <- function(X) {!base::is.logical(X)}

#' @rdname basics
#' @export
not_num <- function(X) {!base::is.numeric(X)}

#' @rdname basics
#' @export
not_ord <- function(X) {!base::is.ordered(X)}

#' @rdname basics
#' @export
not_uno <- function(X) {!base::is.factor(X) | base::is.ordered(X)}

# is(n't) class ####

#' @rdname basics
#' @export
is_arr <- function(X) {base::is.array(X)}

#' @rdname basics
#' @export
is_dtf <- function(X) {base::is.data.frame(X)}

#' @rdname basics
#' @export
is_fun <- function(X) {base::is.function(X)}

#' @rdname basics
#' @export
is_lst <- function(X) {base::is.list(X)}

#' @rdname basics
#' @export
is_mat <- function(X) {base::is.matrix(X)}

#' @rdname basics
#' @export
is_vec <- function(X) {base::is.vector(X)}

#' @rdname basics
#' @export
not_arr <- function(X) {!base::is.array(X)}

#' @rdname basics
#' @export
not_dtf <- function(X) {!base::is.data.frame(X)}

#' @rdname basics
#' @export
not_fun <- function(X) {!base::is.function(X)}

#' @rdname basics
#' @export
not_lst <- function(X) {!base::is.list(X)}

#' @rdname basics
#' @export
not_mat <- function(X) {!base::is.matrix(X)}

#' @rdname basics
#' @export
not_veq <- function(X) {!base::is.vector(X)}

# subsets ####

#' @rdname basics
#' @export
x_in <- function(X, ...) {X[uj::is_in(X, ...)]}

#' @rdname basics
#' @export
x_mf <- function(X, ...) {X[uj::is_mf(X, ...)]}
