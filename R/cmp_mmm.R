#' @name cmp_mmm
#' @family props
#' @title Complete + Extended Mode (cmp + mmm)
#' @description See \code{\link{xcmp}}, and \code{\link{xmmm}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_mmm_vals <- function() {
  x <- paste0("", c("cmp", mmm_vals()))
  names(x) <- rep.int("cmp_mmm", length(x))
  x
}

#' @rdname cmp_mmm
#' @export
cmp_atm <- function(x) {is_cmp_generic(x)}

#' @rdname cmp_mmm
#' @export
cmp_chr <- function(x) {is_cmp_generic(x, "chr")}

#' @rdname cmp_mmm
#' @export
cmp_ch1 <- function(x) {is_cmp_generic(x, "ch1")}

#' @rdname cmp_mmm
#' @export
cmp_clr <- function(x) {is_cmp_generic(x, "clr")}

#' @rdname cmp_mmm
#' @export
cmp_evn <- function(x) {is_cmp_generic(x, "evn")}

#' @rdname cmp_mmm
#' @export
cmp_fac <- function(x) {is_cmp_generic(x, "fac")}

#' @rdname cmp_mmm
#' @export
cmp_frc <- function(x) {is_cmp_generic(x, "frc")}

#' @rdname cmp_mmm
#' @export
cmp_ind <- function(x) {is_cmp_generic(x, "ind")}

#' @rdname cmp_mmm
#' @export
cmp_lgc <- function(x) {is_cmp_generic(x, "lgc")}

#' @rdname cmp_mmm
#' @export
cmp_cnb <- function(x) {is_cmp_generic(x, "cnb")}

#' @rdname cmp_mmm
#' @export
cmp_neg <- function(x) {is_cmp_generic(x, "neg")}

#' @rdname cmp_mmm
#' @export
cmp_ngw <- function(x) {is_cmp_generic(x, "ngw")}

#' @rdname cmp_mmm
#' @export
cmp_nng <- function(x) {is_cmp_generic(x, "nng")}

#' @rdname cmp_mmm
#' @export
cmp_nnw <- function(x) {is_cmp_generic(x, "nnw")}

#' @rdname cmp_mmm
#' @export
cmp_nps <- function(x) {is_cmp_generic(x, "nps")}

#' @rdname cmp_mmm
#' @export
cmp_npw <- function(x) {is_cmp_generic(x, "npw")}

#' @rdname cmp_mmm
#' @export
cmp_nst <- function(x) {is_cmp_generic(x, "nst")}

#' @rdname cmp_mmm
#' @export
cmp_num <- function(x) {is_cmp_generic(x, "num")}

#' @rdname cmp_mmm
#' @export
cmp_odd <- function(x) {is_cmp_generic(x, "odd")}

#' @rdname cmp_mmm
#' @export
cmp_ord <- function(x) {is_cmp_generic(x, "ord")}

#' @rdname cmp_mmm
#' @export
cmp_pct <- function(x) {is_cmp_generic(x, "pct")}

#' @rdname cmp_mmm
#' @export
cmp_pos <- function(x) {is_cmp_generic(x, "pos")}

#' @rdname cmp_mmm
#' @export
cmp_ppn <- function(x) {is_cmp_generic(x, "ppn")}

#' @rdname cmp_mmm
#' @export
cmp_psw <- function(x) {is_cmp_generic(x, "psw")}

#' @rdname cmp_mmm
#' @export
cmp_srt <- function(x) {is_cmp_generic(x, "srt")}

#' @rdname cmp_mmm
#' @export
cmp_str <- function(x) {is_cmp_generic(x, "str")}

#' @rdname cmp_mmm
#' @export
cmp_uno <- function(x) {is_cmp_generic(x, "uno")}

#' @rdname cmp_mmm
#' @export
cmp_whl <- function(x) {is_cmp_generic(x, "whl")}
