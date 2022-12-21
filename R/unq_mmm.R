#' @encoding UTF-8
#' @family props
#' @title unique + xmode combination properties
#' @description \tabular{rl}{
#'     `unq_mmm_funs`   \tab What \link[=iunq]{unique} + link[=mmm]{xmode} combination \link[=prop_funs]{property functions} are there?
#'   \cr                \tab  
#'   \cr    `unq_mmm`   \tab Is `x` both unique and a match to the single xmode property in argument `mmm`?
#'   \cr                \tab  
#'   \cr    `unq_MMM`   \tab Is `x` both unique and a match to single xmode property `'MMM'`?
#' }
#' @param x An R object.
#' @param mmm A character scalar `xmode` property from \code{\link{mmm_props}()}`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr    `unq_mmm_funs`
#'  \cr\cr *A logical scalar*
#'  \cr    `unq_mmm`
#'  \cr    `unq_MMM`
#' @examples
#' unq_mmm_funs()
#' unq_mmm(letters, "ch1")
#' unq_mmm(c("abc", "def", "fgh"), "ch3")
#' unq_psw(0:10)
#' unq_ord(factor(letters, ordered = T))
#' @export
unq_mmm <- function(x, mmm, ...) {
  errs <- c(.meets_errs(x, ...),
            f0(f0(length(mmm) != 1 | !is.character(mmm), F, f0(is.na(mmm), F, mmm %in% .mmms)), NULL, '[mmm] is not a scalar value from mmm_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  f0(!meets(x, ...), F, f0(!is.atomic(x) | length(x) == 0, F, f0(any(is.na(x)), F, f0(length(x) != length(unique(x)), F, run("i", mmm, "(x)")))))
}

#' @rdname unq_mmm
#' @export
unq_mmm_funs <- function() {paste0('unq_', .mmms)}

#' @rdname unq_mmm
#' @export
unq_atm <- function(x, ...) {f0(is.atomic(x), .iunq(x, ...), F)}

#' @rdname unq_mmm
#' @export
unq_ch1 <- function(x, ...) {unq_mmm(x, 'ch1', ...)}

#' @rdname unq_mmm
#' @export
unq_ch3 <- function(x, ...) {unq_mmm(x, 'ch3', ...)}

#' @rdname unq_mmm
#' @export
unq_chr <- function(x, ...) {unq_mmm(x, 'chr', ...)}

#' @rdname unq_mmm
#' @export
unq_clr <- function(x, ...) {unq_mmm(x, 'clr', ...)}

#' @rdname unq_mmm
#' @export
unq_evn <- function(x, ...) {unq_mmm(x, 'evn', ...)}

#' @rdname unq_mmm
#' @export
unq_fac <- function(x, ...) {unq_mmm(x, 'fac', ...)}

#' @rdname unq_mmm
#' @export
unq_frc <- function(x, ...) {unq_mmm(x, 'frc', ...)}

#' @rdname unq_mmm
#' @export
unq_ind <- function(x, ...) {unq_mmm(x, 'ind', ...)}

#' @rdname unq_mmm
#' @export
unq_lgl <- function(x, ...) {unq_mmm(x, 'lgl', ...)}

#' @rdname unq_mmm
#' @export
unq_neg <- function(x, ...) {unq_mmm(x, 'neg', ...)}

#' @rdname unq_mmm
#' @export
unq_ngw <- function(x, ...) {unq_mmm(x, 'ngw', ...)}

#' @rdname unq_mmm
#' @export
unq_nng <- function(x, ...) {unq_mmm(x, 'nng', ...)}

#' @rdname unq_mmm
#' @export
unq_nnw <- function(x, ...) {unq_mmm(x, 'nnw', ...)}

#' @rdname unq_mmm
#' @export
unq_nps <- function(x, ...) {unq_mmm(x, 'nps', ...)}

#' @rdname unq_mmm
#' @export
unq_npw <- function(x, ...) {unq_mmm(x, 'npw', ...)}

#' @rdname unq_mmm
#' @export
unq_nst <- function(x, ...) {unq_mmm(x, 'nst', ...)}

#' @rdname unq_mmm
#' @export
unq_num <- function(x, ...) {unq_mmm(x, 'num', ...)}

#' @rdname unq_mmm
#' @export
unq_odd <- function(x, ...) {unq_mmm(x, 'odd', ...)}

#' @rdname unq_mmm
#' @export
unq_ord <- function(x, ...) {unq_mmm(x, 'ord', ...)}

#' @rdname unq_mmm
#' @export
unq_pct <- function(x, ...) {unq_mmm(x, 'pct', ...)}

#' @rdname unq_mmm
#' @export
unq_pos <- function(x, ...) {unq_mmm(x, 'pos', ...)}

#' @rdname unq_mmm
#' @export
unq_ppn <- function(x, ...) {unq_mmm(x, 'ppn', ...)}

#' @rdname unq_mmm
#' @export
unq_psw <- function(x, ...) {unq_mmm(x, 'psw', ...)}

#' @rdname unq_mmm
#' @export
unq_srt <- function(x, ...) {unq_mmm(x, 'srt', ...)}

#' @rdname unq_mmm
#' @export
unq_str <- function(x, ...) {unq_mmm(x, 'str', ...)}

#' @rdname unq_mmm
#' @export
unq_uno <- function(x, ...) {unq_mmm(x, 'uno', ...)}

#' @rdname unq_mmm
#' @export
unq_whl <- function(x, ...) {unq_mmm(x, 'whl', ...)}
