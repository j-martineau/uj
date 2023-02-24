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
#'                `is_vec, not_vec`   \tab `(!)is.vector(x)`             \cr   \tab   \cr
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
#' u(vals)
#' u(vars)
#' u(data.frame(var = vars, val = vals))
#' dn(vals)
#' up(vals)
#' len(vars)
#' mid(vars, 1, 3)
#' spf(text, vars[1:3], vals[1:3], vars[4:6], vals[4:6])
#' ### more examples ###
#' @export
basics <- function() {utils::help("basics", package = "uj")}

# g (glue within) ####

#' @rdname basics
#' @export
g <- function(g, ...) {base::paste(uj::av(...), collapse = g)}

#' @rdname basics
#' @export
g0 <- function(...) {uj::g("", ...)}

#' @rdname basics
#' @export
g1 <- function(...) {uj::g(" ", ...)}

# p (paste across) ####

#' @rdname basics
#' @inherit base::paste
#' @export
p <- function(p, ...) {base::paste(..., sep = p)}

#' @rdname basics
#' @export
p0 <- function(...) {uj::p("", ...)}

#' @rdname basics
#' @export
p1 <- function(...) {uj::p(" ", ...)}

# special values ####

#' @rdname basics
#' @export
na <- function(x) {base::is.na(x)}

#' @rdname basics
#' @export
ok <- function(x) {!base::is.na(x)}

#' @rdname basics
#' @export
wNA <- function(x) {base::which(base::is.na(x))}

#' @rdname basics
#' @export
wOK <- function(x) {base::which(!base::is.na(x))}

#' @rdname basics
#' @export
null <- function(x) {
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (assertthat::is.error(x)) {T} else {base::is.null(x)}
}

#' @rdname basics
#' @export
def <- function(x) {
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (assertthat::is.error(x)) {F} else {!base::is.null(x)}
}

#' @rdname basics
#' @export
lo <- function(x) {base::floor(x)}

#' @rdname basics
#' @export
hi <- function(x) {base::ceiling(x)}

#' @rdname basics
#' @export
len <- function(x) {base::nchar(x)}

#' @rdname basics
#' @export
mid <- function(x, start, stop) {base::substr(x, start, stop)}

#' @rdname basics
#' @export
spf <- function(fmt, ...) {base::sprintf(fmt, ...)}

#' @rdname basics
#' @export
levs <- function(x) {base::levels(x)}

#' @rdname basics
#' @export
rounded <- function(x) {base::all(x == base::round(x))}

# names ####

#' @rdname basics
#' @export
clabs <- function(x) {base::colnames(x)}

#' @rdname basics
#' @export
dlabs <- function() {base::eval.parent(base::...names())}

#' @rdname basics
#' @export
elabs <- function(x) {base::names(x)}

#' @rdname basics
#' @export
rlabs <- function(x) {base::rownames(x)}

# is(nt) class ####

#' @rdname basics
#' @export
is_atm <- function(x) {base::is.atomic(x)}

#' @rdname basics
#' @export
is__chr <- function(x) {base::is.character(x)}

#' @rdname basics
#' @export
is__fac <- function(x) {base::is.factor(x)}

#' @rdname basics
#' @export
is_int <- function(x) {base::is.integer(x)}

#' @rdname basics
#' @export
is_lgl <- function(x) {base::is.logical(x)}

#' @rdname basics
#' @export
is_num <- function(x) {base::is.numeric(x)}

#' @rdname basics
#' @export
is_ord <- function(x) {base::is.ordered(x)}

#' @rdname basics
#' @export
is_uno <- function(x) {uj::is_fac(x) & !uj::is_ord(x)}

#' @rdname basics
#' @export
not_atm <- function(x) {!uj::is_atm(x)}

#' @rdname basics
#' @export
not_chr <- function(x) {!uj::is_chr(x)}

#' @rdname basics
#' @export
not_fac <- function(x) {!uj::is_fac(x)}

#' @rdname basics
#' @export
not_int <- function(x) {!uj::is_int(x)}

#' @rdname basics
#' @export
not_lgl <- function(x) {!uj::is_lgl(x)}

#' @rdname basics
#' @export
not_num <- function(x) {!uj::is_num(x)}

#' @rdname basics
#' @export
not_ord <- function(x) {!uj::is_ord(x)}

#' @rdname basics
#' @export
not_uno <- function(x) {!uj::is_uno(x)}

# is(n't) class ####

#' @rdname basics
#' @export
is_arr <- function(x) {base::is.array(x)}

#' @rdname basics
#' @export
is_dtf <- function(x) {base::is.data.frame(x)}

#' @rdname basics
#' @export
is_fun <- function(x) {base::is.function(x)}

#' @rdname basics
#' @export
is_lst <- function(x) {base::is.list(x)}

#' @rdname basics
#' @export
is_mat <- function(x) {base::is.matrix(x)}

#' @rdname basics
#' @export
is_vec <- function(x) {base::is.vector(x)}

#' @rdname basics
#' @export
not_arr <- function(x) {!uj::is_arr(x)}

#' @rdname basics
#' @export
not_dtf <- function(x) {!uj::is_dtf(x)}

#' @rdname basics
#' @export
not_fun <- function(x) {!uj::is_fun(x)}

#' @rdname basics
#' @export
not_lst <- function(x) {!uj::is_lst(x)}

#' @rdname basics
#' @export
not_mat <- function(x) {!uj::is_mat(x)}

#' @rdname basics
#' @export
not_vec <- function(x) {!uj::is_vec(x)}

#' @rdname basics
#' @export
x_in <- function(x, ...) {x[uj::isIN(x, ...)]}

#' @rdname basics
#' @export
x_in <- function(x, ...) {x[uj::isMF(x, ...)]}

