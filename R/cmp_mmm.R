#' @name cmp_mmm.
#' @family props
#' @title Complete plus extended mode properties
#' @description Combinations of \link[icmp]{completeness} and
#'   \link[mmm]{extended mode}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE} except for \code{cmp_mmm_vals}, which
#'   returns a character vector.
#' @export
cmp_mmm. <- function() {help("cmp_mmm.", package = "uj")}

#' @describeIn cmp_mmm. Get a character vector of all possible complete +
#'   extended mode properties
#' @export
cmp_mmm_vals <- function() {paste0("", c("cmp", mmm_vals()))}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[iatm]{atomic}
#'   object?
#' @export
cmp_atm <- function(x) {icmp(x) & iatm(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[ich1]{onechar}
#'   object?
#' @export
cmp_ch1 <- function(x) {icmp(x) & ich1(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ichr]{character} object?
#' @export
cmp_chr <- function(x) {icmp(x) & ichr(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[iclr]{color}
#'   object?
#' @export
cmp_clr <- function(x) {icmp(x) & iclr(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ievn]{even-valued whole-number} object?
#' @export
cmp_evn <- function(x) {icmp(x) & ievn(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[ifac]{factor}
#'   object?
#' @export
cmp_fac <- function(x) {icmp(x) & ifac(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ifrc]{fractional-valued numeric} object?
#' @export
cmp_frc <- function(x) {icmp(x) & ifrc(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[iind]{indexer}
#'   object?
#' @export
cmp_ind <- function(x) {icmp(x) & iind(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[ilgl]{logical}
#'   object?
#' @export
cmp_lgl <- function(x) {icmp(x) & ilgl(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ineg]{negative-valued numeric} object?
#' @export
cmp_neg <- function(x) {icmp(x) & ineg(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ingw]{negative-valued whole-number} object?
#' @export
cmp_ngw <- function(x) {icmp(x) & ingw(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inng]{non-negative-valued numeric} object?
#' @export
cmp_nng <- function(x) {icmp(x) & inng(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[innw]{non-negative-valued whole-number} object?
#' @export
cmp_nnw <- function(x) {icmp(x) & innw(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inps]{non-positive-valued numeric} object?
#' @export
cmp_nps <- function(x) {icmp(x) & inps(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inpw]{non-positive-valued whole-number} object?
#' @export
cmp_npw <- function(x) {icmp(x) & inpw(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inst]{non-sortable} object?
#' @export
cmp_nst <- function(x) {icmp(x) & inst(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[inum]{numeric}
#'   object?
#' @export
cmp_num <- function(x) {icmp(x) & inum(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iodd]{odd-valued whole-number} object?
#' @export
cmp_odd <- function(x) {icmp(x) & iodd(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iord]{ordered-factor} object?
#' @export
cmp_ord <- function(x) {icmp(x) & iord(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ipct]{percent-valued (0-100) numeric} object?
#' @export
cmp_pct <- function(x) {icmp(x) & ipct(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ipos]{positive-valued numeric} object?
#' @export
cmp_pos <- function(x) {icmp(x) & ipos(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ippn]{proportion-valued (0-1) numeric} object?
#' @export
cmp_ppn <- function(x) {icmp(x) & ippn(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ipos]{positive-valued whole-number} object?
#' @export
cmp_psw <- function(x) {icmp(x) & ipsw(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[isrt]{sortable} object?
#' @export
cmp_srt <- function(x) {icmp(x) & isrt(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete} \link[istr]{string}
#'   object?
#' @export
cmp_str <- function(x) {icmp(x) & istr(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iuno]{unordered-factor} object?
#' @export
cmp_uno <- function(x) {icmp(x) & iuno(x)}

#' @describeIn cmp_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iwhl]{whole-number} object?
#' @export
cmp_whl <- function(x) {icmp(x) & iwhl(x)}
