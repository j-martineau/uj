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
#'  \cr   `unq_mmm_funs`
#'  \cr\cr *A logical scalar*
#'  \cr   `unq_mmm`
#'  \cr   `unq_MMM`
#' @examples
#' unq_mmm_funs()
#' unq_mmm(letters, "ch1")
#' unq_mmm(c("abc", "def", "fgh"), "ch3")
#' unq_psw(0:10)
#' unq_ord(factor(letters, ordered = T))
#' @export
unq_mmm <- function(x, mmm, ...) {
  if (uj::cmp_mmm(x, mmm, ...)) {
    uj::is_unique(x, a = T)
  } else {F}
}

#' @rdname unq_mmm
#' @export
unq_mmm_funs <- function() {base::paste0('unq_', .mmms)}

#' @rdname unq_mmm
#' @export
unq_atm <- function(x, ...) {uj::f0(base::is.atomic(x), uj:::.iunq(x, ...), F)}

#' @rdname unq_mmm
#' @export
unq_ch1 <- function(x, ...) {uj::unq_mmm(x, 'ch1', ...)}

#' @rdname unq_mmm
#' @export
unq_ch3 <- function(x, ...) {uj::unq_mmm(x, 'ch3', ...)}

#' @rdname unq_mmm
#' @export
unq_chr <- function(x, ...) {uj::unq_mmm(x, 'chr', ...)}

#' @rdname unq_mmm
#' @export
unq_clr <- function(x, ...) {uj::unq_mmm(x, 'clr', ...)}

#' @rdname unq_mmm
#' @export
unq_evn <- function(x, ...) {uj::unq_mmm(x, 'evn', ...)}

#' @rdname unq_mmm
#' @export
unq_fac <- function(x, ...) {uj::unq_mmm(x, 'fac', ...)}

#' @rdname unq_mmm
#' @export
unq_frc <- function(x, ...) {uj::unq_mmm(x, 'frc', ...)}

#' @rdname unq_mmm
#' @export
unq_ind <- function(x, ...) {uj::unq_mmm(x, 'ind', ...)}

#' @rdname unq_mmm
#' @export
unq_lgl <- function(x, ...) {uj::unq_mmm(x, 'lgl', ...)}

#' @rdname unq_mmm
#' @export
unq_neg <- function(x, ...) {uj::unq_mmm(x, 'neg', ...)}

#' @rdname unq_mmm
#' @export
unq_ngw <- function(x, ...) {uj::unq_mmm(x, 'ngw', ...)}

#' @rdname unq_mmm
#' @export
unq_nng <- function(x, ...) {uj::unq_mmm(x, 'nng', ...)}

#' @rdname unq_mmm
#' @export
unq_nnw <- function(x, ...) {uj::unq_mmm(x, 'nnw', ...)}

#' @rdname unq_mmm
#' @export
unq_nps <- function(x, ...) {uj::unq_mmm(x, 'nps', ...)}

#' @rdname unq_mmm
#' @export
unq_npw <- function(x, ...) {uj::unq_mmm(x, 'npw', ...)}

#' @rdname unq_mmm
#' @export
unq_nst <- function(x, ...) {uj::unq_mmm(x, 'nst', ...)}

#' @rdname unq_mmm
#' @export
unq_num <- function(x, ...) {uj::unq_mmm(x, 'num', ...)}

#' @rdname unq_mmm
#' @export
unq_odd <- function(x, ...) {uj::unq_mmm(x, 'odd', ...)}

#' @rdname unq_mmm
#' @export
unq_ord <- function(x, ...) {uj::unq_mmm(x, 'ord', ...)}

#' @rdname unq_mmm
#' @export
unq_pct <- function(x, ...) {uj::unq_mmm(x, 'pct', ...)}

#' @rdname unq_mmm
#' @export
unq_pos <- function(x, ...) {uj::unq_mmm(x, 'pos', ...)}

#' @rdname unq_mmm
#' @export
unq_ppn <- function(x, ...) {uj::unq_mmm(x, 'ppn', ...)}

#' @rdname unq_mmm
#' @export
unq_psw <- function(x, ...) {uj::unq_mmm(x, 'psw', ...)}

#' @rdname unq_mmm
#' @export
unq_srt <- function(x, ...) {uj::unq_mmm(x, 'srt', ...)}

#' @rdname unq_mmm
#' @export
unq_str <- function(x, ...) {uj::unq_mmm(x, 'str', ...)}

#' @rdname unq_mmm
#' @export
unq_uno <- function(x, ...) {uj::unq_mmm(x, 'uno', ...)}

#' @rdname unq_mmm
#' @export
unq_whl <- function(x, ...) {uj::unq_mmm(x, 'whl', ...)}
