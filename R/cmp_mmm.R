#' @name cmp_mmm
#' @family props
#' @title Complete + Extended Mode (cmp + mmm)
#' @description Get all possible complete + extended mode properties.
#' @details See \code{\link{xcmp}}, and \code{\link{xmmm}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_mmm_vals <- function() {
  x <- paste0("", c("cmp", mmm_vals()))
  names(x) <- rep.int("cmp_mmm", length(x))
  x
}

#' @describeIn cmp_mmm Is \code{x} a complete atomic generic?
#' @export
cmp_atm <- function(x) {is_atm(x)}

#' @describeIn cmp_mmm Is \code{x} a complete character atomic generic?
#' @export
cmp_chr <- function(x) {is_chr(x)}

#' @describeIn cmp_mmm Is \code{x} a complete onechar atomic generic?
#' @export
cmp_ch1 <- function(x) {is_ch1(x)}

#' @describeIn cmp_mmm Is \code{x} a complete color atomic generic?
#' @export
cmp_clr <- function(x) {is_clr(x)}

#' @describeIn cmp_mmm Is \code{x} a complete even-numeric atomic generic?
#' @export
cmp_evn <- function(x) {if (!is_num(x)) {F} else {all(x / 2 == round(x / 2))}}

#' @describeIn cmp_mmm Is \code{x} a complete factor atomic generic?
#' @export
cmp_fac <- function(x) {is_fac(x)}

#' @describeIn cmp_mmm Is \code{x} a complete fractional-numeric atomic generic?
#' @export
cmp_frc <- function(x) {is_frc(x)}

#' @describeIn cmp_mmm Is \code{x} a complete indexer atomic generic?
#' @export
cmp_ind <- function(x) {is_ind(x)}

#' @describeIn cmp_mmm Is \code{x} a complete logical atomic generic?
#' @export
cmp_lgl <- function(x) {is_lgl(x)}

#' @describeIn cmp_mmm Is \code{x} a complete negative-numeric atomic generic?
#' @export
cmp_neg <- function(x) {is_neg(x)}

#' @describeIn cmp_mmm Is \code{x} a complete negative-whole-numeric atomic
#'   generic?
#' @export
cmp_ngw <- function(x) {is_ngw(x)}

#' @describeIn cmp_mmm Is \code{x} a complete non-negative-numeric atomic
#'   generic?
#' @export
cmp_nng <- function(x) {is_nng(x)}

#' @describeIn cmp_mmm Is \code{x} a complete non-negative-whole-numeric atomic
#'   generic?
#' @export
cmp_nnw <- function(x) {is_nnw(x)}

#' @describeIn cmp_mmm Is \code{x} a complete non-negative-whole atomic generic?
#' @export
cmp_nps <- function(x) {is_nps(x)}

#' @describeIn cmp_mmm Is \code{x} a complete non-positive-numeric atomic
#'   generic?
#' @export
cmp_npw <- function(x) {is_npw(x)}

#' @describeIn cmp_mmm Is \code{x} a complete non-sortable atomic generic?
#' @export
cmp_nst <- function(x) {is_nst(x)}

#' @describeIn cmp_mmm Is \code{x} a complete numeric atomic generic?
#' @export
cmp_num <- function(x) {is_num(x)}

#' @describeIn cmp_mmm Is \code{x} a complete odd-numeric atomic generic?
#' @export
cmp_odd <- function(x) {is_odd(x)}

#' @describeIn cmp_mmm Is \code{x} a complete ordered-factor atomic generic?
#' @export
cmp_ord <- function(x) {is_ord(x)}

#' @describeIn cmp_mmm Is \code{x} a complete percent-numeric atomic generic?
#' @export
cmp_pct <- function(x) {is_pct(x)}

#' @describeIn cmp_mmm Is \code{x} a complete positive-numeric atomic generic?
#' @export
cmp_pos <- function(x) {is_pos(x)}

#' @describeIn cmp_mmm Is \code{x} a complete proportion-numeric atomic generic?
#' @export
cmp_ppn <- function(x) {is_ppn(x)}

#' @describeIn cmp_mmm Is \code{x} a complete positive-whole-numeric atomic
#'   generic?
#' @export
cmp_psw <- function(x) {is_psw(x)}

#' @describeIn cmp_mmm Is \code{x} a complete sortable atomic generic?
#' @export
cmp_srt <- function(x) {is_srt(x)}

#' @describeIn cmp_mmm Is \code{x} a complete string atomic generic?
#' @export
cmp_str <- function(x) {is_str(x)}

#' @describeIn cmp_mmm Is \code{x} a complete unordered-factor atomic generic?
#' @export
cmp_uno <- function(x) {is_uno(x)}

#' @describeIn cmp_mmm Is \code{x} a complete whole-numeric atomic generic?
#' @export
cmp_whl <- function(x) {is_whl(x)}
