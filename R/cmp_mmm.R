#' @encoding UTF-8
#' @family properties
#' @title complete + xmode combination properties
#' @description \tabular{rl}{
#'     `cmp_mmm_funs`   \tab What \link[=icmp]{complete} + \link[=mmm]{xmode} combination \link[=prop_funs]{property functions} are there?
#'   \cr                \tab  
#'   \cr    `cmp_mmm`   \tab Is `x` both complete and a match to the single xmode property in `mmm`?
#'   \cr                \tab  
#'   \cr    `cmp_MMM`   \tab Is `x` both complete and a match to single xmode property `'MMM'`?
#' }
#' @param x An R object.
#' @param mmm A character scalar xmode property from `mmm_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr    `cmp_mmm_funs`
#'  \cr\cr *A logical scalar*
#'  \cr    `cmp_MMM`
#'  \cr    `cmp_mmm`
#' @examples
#' cmp_mmm_funs()
#' cmp_mmm(letters, "ch1")
#' cmp_mmm(c("abc", "def"), "ch3")
#' cmp_psw(0:10)
#' cmp_ord(factor(letters, ordered = T))
#' @export
cmp_mmm <- function(x, mmm, ...) {
  errs <- c(.meets_errs(x, ...),
            f0(f0(length(mmm) != 1 | !is.character(mmm), F, f0(is.na(mmm), F, mmm %in% .mmms)), NULL, '[mmm] is not a scalar value from mmm_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
  f0(!meets(x, ...), F, f0(!is.atomic(x) | length(x) == 0, F, f0(any(is.na(x)), F, run("i", mmm, "(x)"))))
}

#' @rdname cmp_mmm
#' @export
cmp_mmm_funs <- function() {paste0('cmp_', .mmms)}

#' @rdname cmp_mmm
#' @export
cmp_atm <- function(x, ...) {f0(is.atomic(x), icmp(x, ...), F)}

#' @rdname cmp_mmm
#' @export
cmp_ch1 <- function(x, ...) {cmp_mmm(x, 'ch1', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ch3 <- function(x, ...) {cmp_mmm(x, 'ch3', ...)}

#' @rdname cmp_mmm
#' @export
cmp_chr <- function(x, ...) {cmp_mmm(x, 'chr', ...)}

#' @rdname cmp_mmm
#' @export
cmp_clr <- function(x, ...) {cmp_mmm(x, 'clr', ...)}

#' @rdname cmp_mmm
#' @export
cmp_evn <- function(x, ...) {cmp_mmm(x, 'evn', ...)}

#' @rdname cmp_mmm
#' @export
cmp_fac <- function(x, ...) {cmp_mmm(x, 'fac', ...)}

#' @rdname cmp_mmm
#' @export
cmp_frc <- function(x, ...) {cmp_mmm(x, 'frc', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ind <- function(x, ...) {cmp_mmm(x, 'ind', ...)}

#' @rdname cmp_mmm
#' @export
cmp_lgl <- function(x, ...) {cmp_mmm(x, 'lgl', ...)}

#' @rdname cmp_mmm
#' @export
cmp_neg <- function(x, ...) {cmp_mmm(x, 'neg', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ngw <- function(x, ...) {cmp_mmm(x, 'ngw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nng <- function(x, ...) {cmp_mmm(x, 'nng', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nnw <- function(x, ...) {cmp_mmm(x, 'nnw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nps <- function(x, ...) {cmp_mmm(x, 'nps', ...)}

#' @rdname cmp_mmm
#' @export
cmp_npw <- function(x, ...) {cmp_mmm(x, 'npw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_nst <- function(x, ...) {cmp_mmm(x, 'nst', ...)}

#' @rdname cmp_mmm
#' @export
cmp_num <- function(x, ...) {cmp_mmm(x, 'num', ...)}

#' @rdname cmp_mmm
#' @export
cmp_odd <- function(x, ...) {cmp_mmm(x, 'odd', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ord <- function(x, ...) {cmp_mmm(x, 'ord', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pct <- function(x, ...) {cmp_mmm(x, 'pct', ...)}

#' @rdname cmp_mmm
#' @export
cmp_pos <- function(x, ...) {cmp_mmm(x, 'pos', ...)}

#' @rdname cmp_mmm
#' @export
cmp_ppn <- function(x, ...) {cmp_mmm(x, 'ppn', ...)}

#' @rdname cmp_mmm
#' @export
cmp_psw <- function(x, ...) {cmp_mmm(x, 'psw', ...)}

#' @rdname cmp_mmm
#' @export
cmp_srt <- function(x, ...) {cmp_mmm(x, 'srt', ...)}

#' @rdname cmp_mmm
#' @export
cmp_str <- function(x, ...) {cmp_mmm(x, 'str', ...)}

#' @rdname cmp_mmm
#' @export
cmp_uno <- function(x, ...) {cmp_mmm(x, 'uno', ...)}

#' @rdname cmp_mmm
#' @export
cmp_whl <- function(x, ...) {cmp_mmm(x, 'whl', ...)}
