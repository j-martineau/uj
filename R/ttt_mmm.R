#' @name ttt_mmm.
#' @family props
#' @title Fundamental type plus extended mode properties
#' @description Combinations of \link[ttt]{fundamental type} and
#'   \link[mmm]{extended mode} properties.
#' @param x An R object.
#' @return Logical scalar.
#' @export
ttt_mmm. <- function() {help("ttt_mmm.", package = "uj")}

#' @describeIn ttt_mmm. Get a character vector of all possible fundamental type
#'   plus extended class properties.
#' @export
ttt_mmm_vals <- function() {
  join <- function(x) {paste0(av(x), collapse = "_")}
  sort(av(apply(expand.grid(ttt = ttt_vals(), mmm = mmm_vals()), 1, join)))
}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated} \link[iatm]{atomic}
#'   object?
#' @export
pop_atm <- function(x) {ipop(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ich1]{onechar} object?
#' @export
pop_ch1 <- function(x) {ipop(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ichr]{character} object?
#' @export
pop_chr <- function(x) {ipop(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated} \link[iclr]{color}
#'   object?
#' @export
pop_clr <- function(x) {ipop(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ievn]{even-valued whole-number} object?
#' @export
pop_evn <- function(x) {ipop(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated} \link[ifac]{factor}
#'   object?
#' @export
pop_fac <- function(x) {ipop(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ifrc]{fractional-valued numeric} object?
#' @export
pop_frc <- function(x) {ipop(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[iind]{indexer} object?
#' @export
pop_ind <- function(x) {ipop(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ilgl]{logical} object?
#' @export
pop_lgl <- function(x) {ipop(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ineg]{negative-valued mumeric} object?
#' @export
pop_neg <- function(x) {ipop(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ingw]{negative-valued whole-number} object?
#' @export
pop_ngw <- function(x) {ipop(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[inng]{non-negative-valued numeric} object?
#' @export
pop_nng <- function(x) {ipop(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[innw]{non-negative-valued whole-number} object?
#' @export
pop_nnw <- function(x) {ipop(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[inps]{non-positive-valued numeric object} object?
#' @export
pop_nps <- function(x) {ipop(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[inpw]{non-positive-valued whole-number} object?
#' @export
pop_npw <- function(x) {ipop(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[inst]{non-sortable} object?
#' @export
pop_nst <- function(x) {ipop(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[inum]{numeric} object?
#' @export
pop_num <- function(x) {ipop(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[iodd]{odd-valued whole-number} object?
#' @export
pop_odd <- function(x) {ipop(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[iord]{ordered-factor} object?
#' @export
pop_ord <- function(x) {ipop(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ipct]{percent-valued (0-100) numeric} object?
#' @export
pop_pct <- function(x) {ipop(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ipos]{positive-valued numeric object} object?
#' @export
pop_pos <- function(x) {ipop(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ippn]{proportion-valued (0-1) numeric} object?
#' @export
pop_ppn <- function(x) {ipop(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[ipsw]{positive-valued whole-number} object?
#' @export
pop_psw <- function(x) {ipop(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[isrt]{sortable} object?
#' @export
pop_srt <- function(x) {ipop(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated} \link[istr]{string}
#'   object?
#' @export
pop_str <- function(x) {ipop(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[iuno]{unordered-factor} object?
#' @export
pop_uno <- function(x) {ipop(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ipop]{populated}
#'   \link[iwhl]{whole-number} object?
#' @export
pop_whl <- function(x) {ipop(x) & iwhl(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[iatm]{atomic}
#'   object?
#' @export
cmp_atm <- function(x) {icmp(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[ich1]{onechar}
#'   object?
#' @export
cmp_ch1 <- function(x) {icmp(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ichr]{character} object?
#' @export
cmp_chr <- function(x) {icmp(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[iclr]{color}
#'   object?
#' @export
cmp_clr <- function(x) {icmp(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ievn]{even-valued whole-number} object?
#' @export
cmp_evn <- function(x) {icmp(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[ifac]{factor}
#'   object?
#' @export
cmp_fac <- function(x) {icmp(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ifrc]{fractional-valued numeric} object?
#' @export
cmp_frc <- function(x) {icmp(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[iind]{indexer}
#'   object?
#' @export
cmp_ind <- function(x) {icmp(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[ilgl]{logical}
#'   object?
#' @export
cmp_lgl <- function(x) {icmp(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ineg]{negative-valued mumeric} object?
#' @export
cmp_neg <- function(x) {icmp(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ingw]{negative-valued whole-number} object?
#' @export
cmp_ngw <- function(x) {icmp(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inng]{non-negative-valued numeric} object?
#' @export
cmp_nng <- function(x) {icmp(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[innw]{non-negative-valued whole-number} object?
#' @export
cmp_nnw <- function(x) {icmp(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inps]{non-positive-valued numeric object} object?
#' @export
cmp_nps <- function(x) {icmp(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inpw]{non-positive-valued whole-number} object?
#' @export
cmp_npw <- function(x) {icmp(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[inst]{non-sortable} object?
#' @export
cmp_nst <- function(x) {icmp(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[inum]{numeric}
#'   object?
#' @export
cmp_num <- function(x) {icmp(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iodd]{odd-valued whole-number} object?
#' @export
cmp_odd <- function(x) {icmp(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iord]{ordered-factor} object?
#' @export
cmp_ord <- function(x) {icmp(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ipct]{percent-valued (0-100) numeric} object?
#' @export
cmp_pct <- function(x) {icmp(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ipos]{positive-valued numeric object} object?
#' @export
cmp_pos <- function(x) {icmp(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ippn]{proportion-valued (0-1) numeric} object?
#' @export
cmp_ppn <- function(x) {icmp(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[ipsw]{positive-valued whole-number} object?
#' @export
cmp_psw <- function(x) {icmp(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[isrt]{sortable} object?
#' @export
cmp_srt <- function(x) {icmp(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete} \link[istr]{string}
#'   object?
#' @export
cmp_str <- function(x) {icmp(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iuno]{unordered-factor} object?
#' @export
cmp_uno <- function(x) {icmp(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[icmp]{complete}
#'   \link[iwhl]{whole-number} object?
#' @export
cmp_whl <- function(x) {icmp(x) & iwhl(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[iatm]{atomic} object?
#' @export
emp_atm <- function(x) {iemp(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ich1]{onechar} object?
#' @export
emp_ch1 <- function(x) {iemp(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ichr]{character} object?
#' @export
emp_chr <- function(x) {iemp(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[iclr]{color} object?
#' @export
emp_clr <- function(x) {iemp(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ievn]{even-valued whole-number} object?
#' @export
emp_evn <- function(x) {iemp(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ifac]{factor} object?
#' @export
emp_fac <- function(x) {iemp(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ifrc]{fractional-valued numeric} object?
#' @export
emp_frc <- function(x) {iemp(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[iind]{indexer} object?
#' @export
emp_ind <- function(x) {iemp(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ilgl]{logical} object?
#' @export
emp_lgl <- function(x) {iemp(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ineg]{negative-valued mumeric} object?
#' @export
emp_neg <- function(x) {iemp(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ingw]{negative-valued whole-number} object?
#' @export
emp_ngw <- function(x) {iemp(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[inng]{non-negative-valued numeric} object?
#' @export
emp_nng <- function(x) {iemp(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[innw]{non-negative-valued whole-number} object?
#' @export
emp_nnw <- function(x) {iemp(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[inps]{non-positive-valued numeric object} object?
#' @export
emp_nps <- function(x) {iemp(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[inpw]{non-positive-valued whole-number} object?
#' @export
emp_npw <- function(x) {iemp(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[inst]{non-sortable} object?
#' @export
emp_nst <- function(x) {iemp(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[inum]{numeric} object?
#' @export
emp_num <- function(x) {iemp(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[iodd]{odd-valued whole-number} object?
#' @export
emp_odd <- function(x) {iemp(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[iord]{ordered-factor} object?
#' @export
emp_ord <- function(x) {iemp(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ipct]{percent-valued (0-100) numeric} object?
#' @export
emp_pct <- function(x) {iemp(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ipos]{positive-valued numeric object} object?
#' @export
emp_pos <- function(x) {iemp(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ippn]{proportion-valued (0-1) numeric} object?
#' @export
emp_ppn <- function(x) {iemp(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[ipsw]{positive-valued whole-number} object?
#' @export
emp_psw <- function(x) {iemp(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[isrt]{sortable} object?
#' @export
emp_srt <- function(x) {iemp(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[istr]{string} object?
#' @export
emp_str <- function(x) {iemp(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[iuno]{unordered-factor} object?
#' @export
emp_uno <- function(x) {iemp(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} an \link[iemp]{empty}
#'   \link[iwhl]{whole-number} object?
#' @export
emp_whl <- function(x) {iemp(x) & iwhl(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[iatm]{atomic} object?
#' @export
rcr_atm <- function(x) {ircr(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ich1]{onechar} object?
#' @export
rcr_ch1 <- function(x) {ircr(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ichr]{character} object?
#' @export
rcr_chr <- function(x) {ircr(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[iclr]{color} object?
#' @export
rcr_clr <- function(x) {ircr(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ievn]{even-valued whole-number} object?
#' @export
rcr_evn <- function(x) {ircr(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ifac]{factor} object?
#' @export
rcr_fac <- function(x) {ircr(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ifrc]{fractional-valued numeric} object?
#' @export
rcr_frc <- function(x) {ircr(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[iind]{indexer} object?
#' @export
rcr_ind <- function(x) {ircr(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ilgl]{logical} object?
#' @export
rcr_lgl <- function(x) {ircr(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ineg]{negative-valued mumeric} object?
#' @export
rcr_neg <- function(x) {ircr(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ingw]{negative-valued whole-number} object?
#' @export
rcr_ngw <- function(x) {ircr(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[inng]{non-negative-valued numeric} object?
#' @export
rcr_nng <- function(x) {ircr(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[innw]{non-negative-valued whole-number} object?
#' @export
rcr_nnw <- function(x) {ircr(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[inps]{non-positive-valued numeric object} object?
#' @export
rcr_nps <- function(x) {ircr(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[inpw]{non-positive-valued whole-number} object?
#' @export
rcr_npw <- function(x) {ircr(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[inst]{non-sortable} object?
#' @export
rcr_nst <- function(x) {ircr(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[inum]{numeric} object?
#' @export
rcr_num <- function(x) {ircr(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[iodd]{odd-valued whole-number} object?
#' @export
rcr_odd <- function(x) {ircr(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[iord]{ordered-factor} object?
#' @export
rcr_ord <- function(x) {ircr(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ipct]{percent-valued (0-100) numeric} object?
#' @export
rcr_pct <- function(x) {ircr(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ipos]{positive-valued numeric object} object?
#' @export
rcr_pos <- function(x) {ircr(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ippn]{proportion-valued (0-1) numeric} object?
#' @export
rcr_ppn <- function(x) {ircr(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[ipsw]{positive-valued whole-number} object?
#' @export
rcr_psw <- function(x) {ircr(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[isrt]{sortable} object?
#' @export
rcr_srt <- function(x) {ircr(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive} \link[istr]{string}
#'   object?
#' @export
rcr_str <- function(x) {ircr(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[iuno]{unordered-factor} object?
#' @export
rcr_uno <- function(x) {ircr(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} a \link[ircr]{recursive}
#'   \link[iwhl]{whole-number} object?
#' @export
rcr_whl <- function(x) {ircr(x) & iwhl(x)}
