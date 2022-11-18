#' @name cmp_mmm.
#' @family props
#' @title Completeness + Extended Mode Properties
#' @description \tabular{ll}{
#'   \code{cmp_mmm_vals}   \tab Gets a character vector of all possible
#'                              \link[=icmp]{completeness} +
#'                              \link[=mmm]{extended mode} properties.       \cr
#'   \code{cmp_[mmm]}      \tab Evaluates whether \code{x} is
#'                              \link[=icmp]{complete} and of the
#'                              \link[=mmm]{extended mode} represented by the
#'                              placeholder \code{[mmm]}.                      }
#' @param x An R object.
#' @return \tabular{ll}{
#'   \code{cmp_mmm_vals}   \tab A character vector.                          \cr
#'   \code{cmp_[mmm]}      \tab A logical scalar.                              }
#' @export
cmp_mmm. <- function() {help("cmp_mmm.", package = "uj")}

#' @rdname cmp_mmm.
#' @export
cmp_mmm_vals <- function() {paste0("cmp_", mmm_vals())}

#' @rdname cmp_mmm.
#' @export
cmp_atm <- function(x) {icmp(x) & iatm(x)}

#' @rdname cmp_mmm.
#' @export
cmp_ch1 <- function(x) {icmp(x) & ich1(x)}

#' @rdname cmp_mmm.
#' @export
cmp_chr <- function(x) {icmp(x) & ichr(x)}

#' @rdname cmp_mmm.
#' @export
cmp_clr <- function(x) {icmp(x) & iclr(x)}

#' @rdname cmp_mmm.
#' @export
cmp_evn <- function(x) {icmp(x) & ievn(x)}

#' @rdname cmp_mmm.
#' @export
cmp_fac <- function(x) {icmp(x) & ifac(x)}

#' @rdname cmp_mmm.
#' @export
cmp_frc <- function(x) {icmp(x) & ifrc(x)}

#' @rdname cmp_mmm.
#' @export
cmp_ind <- function(x) {icmp(x) & iind(x)}

#' @rdname cmp_mmm.
#' @export
cmp_lgl <- function(x) {icmp(x) & ilgl(x)}

#' @rdname cmp_mmm.
#' @export
cmp_neg <- function(x) {icmp(x) & ineg(x)}

#' @rdname cmp_mmm.
#' @export
cmp_ngw <- function(x) {icmp(x) & ingw(x)}

#' @rdname cmp_mmm.
#' @export
cmp_nng <- function(x) {icmp(x) & inng(x)}

#' @rdname cmp_mmm.
#' @export
cmp_nnw <- function(x) {icmp(x) & innw(x)}

#' @rdname cmp_mmm.
#' @export
cmp_nps <- function(x) {icmp(x) & inps(x)}

#' @rdname cmp_mmm.
#' @export
cmp_npw <- function(x) {icmp(x) & inpw(x)}

#' @rdname cmp_mmm.
#' @export
cmp_nst <- function(x) {icmp(x) & inst(x)}

#' @rdname cmp_mmm.
#' @export
cmp_num <- function(x) {icmp(x) & inum(x)}

#' @rdname cmp_mmm.
#' @export
cmp_odd <- function(x) {icmp(x) & iodd(x)}

#' @rdname cmp_mmm.
#' @export
cmp_ord <- function(x) {icmp(x) & iord(x)}

#' @rdname cmp_mmm.
#' @export
cmp_pct <- function(x) {icmp(x) & ipct(x)}

#' @rdname cmp_mmm.
#' @export
cmp_pos <- function(x) {icmp(x) & ipos(x)}

#' @rdname cmp_mmm.
#' @export
cmp_ppn <- function(x) {icmp(x) & ippn(x)}

#' @rdname cmp_mmm.
#' @export
cmp_psw <- function(x) {icmp(x) & ipsw(x)}

#' @rdname cmp_mmm.
#' @export
cmp_srt <- function(x) {icmp(x) & isrt(x)}

#' @rdname cmp_mmm.
#' @export
cmp_str <- function(x) {icmp(x) & istr(x)}

#' @rdname cmp_mmm.
#' @export
cmp_uno <- function(x) {icmp(x) & iuno(x)}

#' @rdname cmp_mmm.
#' @export
cmp_whl <- function(x) {icmp(x) & iwhl(x)}
