#' @name ttt_mmm.
#' @family props
#' @title Fundamental type + extended mode properties
#' @param x An object.
#' @param xxx Optional character scalar extended mode (see
#'   \code{\link{mmm_vals}}).
#' @return Logical scalar.
#' @export
ttt_mmm. <- function() {help("ttt_mmm.", package = "uj")}

#' @describeIn ttt_mmm. Get a character vector of all possible fundamental
#'   type plus extended class properties.
#' @export
ttt_mmm_vals <- function() {av(apply(expand.grid(ttt = ttt_vals(), mmm = mmm_vals()), 2, dw0))}

#' @describeIn ttt_mmm. Is \code{x} a populated atomic object?
#'   (\code{\link{ipop}}, \code{\link{iatm}}).
#' @export
pop_atm <- function(x) {ipop(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated onechar object?
#'   (\code{\link{ipop}}, \code{\link{ich1}}).
#' @export
pop_ch1 <- function(x) {ipop(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated character object?
#'   (\code{\link{ipop}}, \code{\link{ichr}}).
#' @export
pop_chr <- function(x) {ipop(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated color object?
#'   (\code{\link{ipop}}, \code{\link{iclr}}).
#' @export
pop_clr <- function(x) {ipop(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated even-valued
#'   whole-number object?
#'   (\code{\link{ipop}}, \code{\link{ievn}}).
#' @export
pop_evn <- function(x) {ipop(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated factor object?
#'   (\code{\link{ipop}}, \code{\link{ifac}}).
#' @export
pop_fac <- function(x) {ipop(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated fractional-valued
#'   numeric object?
#'   (\code{\link{ipop}}, \code{\link{ifrc}}).
#' @export
pop_frc <- function(x) {ipop(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated indexer object?
#'   (\code{\link{ipop}}, \code{\link{iind}}).
#' @export
pop_ind <- function(x) {ipop(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated logical object?
#'   (\code{\link{ipop}}, \code{\link{ilgl}}).
#' @export
pop_lgl <- function(x) {ipop(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated negative-valued
#'   numeric object?
#'   (\code{\link{ipop}}, \code{\link{ineg}}).
#' @export
pop_neg <- function(x) {ipop(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated negative-valued
#'   whole-number object?
#'   (\code{\link{ipop}}, \code{\link{ingw}}).
#' @export
pop_ngw <- function(x) {ipop(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated non-negative-valued
#'   numeric object?
#'   (\code{\link{ipop}}, \code{\link{inng}}).
#' @export
pop_nng <- function(x) {ipop(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated non-negative-valued
#'   whole-number object?
#'   (\code{\link{ipop}}, \code{\link{innw}}).
#' @export
pop_nnw <- function(x) {ipop(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated non-positive-valued
#'   numeric object?
#'   (\code{\link{ipop}}, \code{\link{inps}}).
#' @export
pop_nps <- function(x) {ipop(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated non-positive-valued
#'   whole-number object?
#'   (\code{\link{ipop}}, \code{\link{inpw}}).
#' @export
pop_npw <- function(x) {ipop(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated non-sortable object?
#'   (\code{\link{ipop}}, \code{\link{inst}}).
#' @export
pop_nst <- function(x) {ipop(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated numeric object?
#'   (\code{\link{ipop}}, \code{\link{inum}}).
#' @export
pop_num <- function(x) {ipop(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated odd-valued
#'   whole-number object?
#'   (\code{\link{ipop}}, \code{\link{iodd}}).
#' @export
pop_odd <- function(x) {ipop(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated ordered-factor object?
#'   (\code{\link{ipop}}, \code{\link{iord}}).
#' @export
pop_ord <- function(x) {ipop(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated percent-valued
#'   (0-100) numeric object?
#'   (\code{\link{ipop}}, \code{\link{ipct}}).
#' @export
pop_pct <- function(x) {ipop(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated positive-valued
#'   numeric object?
#'   (\code{\link{ipop}}, \code{\link{ipos}}).
#' @export
pop_pos <- function(x) {ipop(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated proportion-valued
#'   (0-1) numeric object?
#'   (\code{\link{ipop}}, \code{\link{ippn}}).
#' @export
pop_ppn <- function(x) {ipop(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated positive-valued
#'   whole-number object?
#'   (\code{\link{ipop}}, \code{\link{ipsw}}).
#' @export
pop_psw <- function(x) {ipop(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated sortable object?
#'   (\code{\link{ipop}}, \code{\link{isrt}}).
#' @export
pop_srt <- function(x) {ipop(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated string object?
#'   (\code{\link{ipop}}, \code{\link{istr}}).
#' @export
pop_str <- function(x) {ipop(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated unordered-factor object?
#'   (\code{\link{ipop}}, \code{\link{iuno}}).
#' @export
pop_uno <- function(x) {ipop(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} a populated whole-number object?
#'   (\code{\link{ipop}}, \code{\link{iwhl}}).
#' @export
pop_whl <- function(x) {ipop(x) & iwhl(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete atomic object?
#'   (\code{\link{icmp}}, \code{\link{iatm}}).
#' @export
cmp_atm <- function(x) {icmp(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete onechar object?
#'   (\code{\link{icmp}}, \code{\link{ich1}}).
#' @export
cmp_ch1 <- function(x) {icmp(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete character object?
#'   (\code{\link{icmp}}, \code{\link{ichr}}).
#' @export
cmp_chr <- function(x) {icmp(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete color object?
#'   (\code{\link{icmp}}, \code{\link{iclr}}).
#' @export
cmp_clr <- function(x) {icmp(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete even-valued
#'   whole-number object?
#'   (\code{\link{icmp}}, \code{\link{ievn}}).
#' @export
cmp_evn <- function(x) {icmp(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete factor object?
#'   (\code{\link{icmp}}, \code{\link{ifac}}).
#' @export
cmp_fac <- function(x) {icmp(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete fractional-valued
#'   numeric object?
#'   (\code{\link{icmp}}, \code{\link{ifrc}}).
#' @export
cmp_frc <- function(x) {icmp(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete indexer object?
#'   (\code{\link{icmp}}, \code{\link{iind}}).
#' @export
cmp_ind <- function(x) {icmp(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete logical object?
#'   (\code{\link{icmp}}, \code{\link{ilgl}}).
#' @export
cmp_lgl <- function(x) {icmp(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete negative-valued
#'   numeric object?
#'   (\code{\link{icmp}}, \code{\link{ineg}}).
#' @export
cmp_neg <- function(x) {icmp(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete negative-valued
#'   whole-number object?
#'   (\code{\link{icmp}}, \code{\link{ingw}}).
#' @export
cmp_ngw <- function(x) {icmp(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete non-negative-valued
#'   numeric object?
#'   (\code{\link{icmp}}, \code{\link{inng}}).
#' @export
cmp_nng <- function(x) {icmp(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete non-negative-valued
#'   whole-number object?
#'   (\code{\link{icmp}}, \code{\link{innw}}).
#' @export
cmp_nnw <- function(x) {icmp(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete non-positive-valued
#'   numeric object?
#'   (\code{\link{icmp}}, \code{\link{inps}}).
#' @export
cmp_nps <- function(x) {icmp(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete non-positive-valued
#'   whole-number object?
#'   (\code{\link{icmp}}, \code{\link{inpw}}).
#' @export
cmp_npw <- function(x) {icmp(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete non-sortable object?
#'   (\code{\link{icmp}}, \code{\link{inst}}).
#' @export
cmp_nst <- function(x) {icmp(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete numeric object?
#'   (\code{\link{icmp}}, \code{\link{inum}}).
#' @export
cmp_num <- function(x) {icmp(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete odd-valued
#'   whole-number object?
#'   (\code{\link{icmp}}, \code{\link{iodd}}).
#' @export
cmp_odd <- function(x) {icmp(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete ordered-factor object?
#'   (\code{\link{icmp}}, \code{\link{iord}}).
#' @export
cmp_ord <- function(x) {icmp(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete percent-valued
#'   (0-100) numeric object?
#'   (\code{\link{icmp}}, \code{\link{ipct}}).
#' @export
cmp_pct <- function(x) {icmp(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete positive-valued
#'   numeric object?
#'   (\code{\link{icmp}}, \code{\link{ipos}}).
#' @export
cmp_pos <- function(x) {icmp(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete proportion-valued
#'   (0-1) numeric object?
#'   (\code{\link{icmp}}, \code{\link{ippn}}).
#' @export
cmp_ppn <- function(x) {icmp(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete positive-valued
#'   whole-number object?
#'   (\code{\link{icmp}}, \code{\link{ipsw}}).
#' @export
cmp_psw <- function(x) {icmp(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete sortable object?
#'   (\code{\link{icmp}}, \code{\link{isrt}}).
#' @export
cmp_srt <- function(x) {icmp(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete string object?
#'   (\code{\link{icmp}}, \code{\link{istr}}).
#' @export
cmp_str <- function(x) {icmp(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete unordered-factor object?
#'   (\code{\link{icmp}}, \code{\link{iuno}}).
#' @export
cmp_uno <- function(x) {icmp(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} a complete whole-number object?
#'   (\code{\link{icmp}}, \code{\link{iwhl}}).
#' @export
cmp_whl <- function(x) {icmp(x) & iwhl(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty atomic object?
#'   (\code{\link{iemp}}, \code{\link{iatm}}).
#' @export
emp_atm <- function(x) {iemp(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty onechar object?
#'   (\code{\link{iemp}}, \code{\link{ich1}}).
#' @export
emp_ch1 <- function(x) {iemp(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty character object?
#'   (\code{\link{iemp}}, \code{\link{ichr}}).
#' @export
emp_chr <- function(x) {iemp(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty color object?
#'   (\code{\link{iemp}}, \code{\link{iclr}}).
#' @export
emp_clr <- function(x) {iemp(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty even-valued
#'   whole-number object?
#'   (\code{\link{iemp}}, \code{\link{ievn}}).
#' @export
emp_evn <- function(x) {iemp(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty factor object?
#'   (\code{\link{iemp}}, \code{\link{ifac}}).
#' @export
emp_fac <- function(x) {iemp(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty fractional-valued
#'   numeric object?
#'   (\code{\link{iemp}}, \code{\link{ifrc}}).
#' @export
emp_frc <- function(x) {iemp(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty indexer object?
#'   (\code{\link{iemp}}, \code{\link{iind}}).
#' @export
emp_ind <- function(x) {iemp(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty logical object?
#'   (\code{\link{iemp}}, \code{\link{ilgl}}).
#' @export
emp_lgl <- function(x) {iemp(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty negative-valued
#'   numeric object?
#'   (\code{\link{iemp}}, \code{\link{ineg}}).
#' @export
emp_neg <- function(x) {iemp(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty negative-valued
#'   whole-number object?
#'   (\code{\link{iemp}}, \code{\link{ingw}}).
#' @export
emp_ngw <- function(x) {iemp(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty non-negative-valued
#'   numeric object?
#'   (\code{\link{iemp}}, \code{\link{inng}}).
#' @export
emp_nng <- function(x) {iemp(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty non-negative-valued
#'   whole-number object?
#'   (\code{\link{iemp}}, \code{\link{innw}}).
#' @export
emp_nnw <- function(x) {iemp(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty non-positive-valued
#'   numeric object?
#'   (\code{\link{iemp}}, \code{\link{inps}}).
#' @export
emp_nps <- function(x) {iemp(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty non-positive-valued
#'   whole-number object?
#'   (\code{\link{iemp}}, \code{\link{inpw}}).
#' @export
emp_npw <- function(x) {iemp(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty non-sortable object?
#'   (\code{\link{iemp}}, \code{\link{inst}}).
#' @export
emp_nst <- function(x) {iemp(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty numeric object?
#'   (\code{\link{iemp}}, \code{\link{inum}}).
#' @export
emp_num <- function(x) {iemp(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty odd-valued
#'   whole-number object?
#'   (\code{\link{iemp}}, \code{\link{iodd}}).
#' @export
emp_odd <- function(x) {iemp(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty ordered-factor object?
#'   (\code{\link{iemp}}, \code{\link{iord}}).
#' @export
emp_ord <- function(x) {iemp(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty percent-valued
#'   (0-100) numeric object?
#'   (\code{\link{iemp}}, \code{\link{ipct}}).
#' @export
emp_pct <- function(x) {iemp(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty positive-valued
#'   numeric object?
#'   (\code{\link{iemp}}, \code{\link{ipos}}).
#' @export
emp_pos <- function(x) {iemp(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty proportion-valued
#'   (0-1) numeric object?
#'   (\code{\link{iemp}}, \code{\link{ippn}}).
#' @export
emp_ppn <- function(x) {iemp(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty positive-valued
#'   whole-number object?
#'   (\code{\link{iemp}}, \code{\link{ipsw}}).
#' @export
emp_psw <- function(x) {iemp(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty sortable object?
#'   (\code{\link{iemp}}, \code{\link{isrt}}).
#' @export
emp_srt <- function(x) {iemp(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty string object?
#'   (\code{\link{iemp}}, \code{\link{istr}}).
#' @export
emp_str <- function(x) {iemp(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty unordered-factor object?
#'   (\code{\link{iemp}}, \code{\link{iuno}}).
#' @export
emp_uno <- function(x) {iemp(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} an empty whole-number object?
#'   (\code{\link{iemp}}, \code{\link{iwhl}}).
#' @export
emp_whl <- function(x) {iemp(x) & iwhl(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive atomic object?
#'   (\code{\link{ircr}}, \code{\link{iatm}}).
#' @export
rcr_atm <- function(x) {ircr(x) & iatm(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive onechar object?
#'   (\code{\link{ircr}}, \code{\link{ich1}}).
#' @export
rcr_ch1 <- function(x) {ircr(x) & ich1(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive character object?
#'   (\code{\link{ircr}}, \code{\link{ichr}}).
#' @export
rcr_chr <- function(x) {ircr(x) & ichr(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive color object?
#'   (\code{\link{ircr}}, \code{\link{iclr}}).
#' @export
rcr_clr <- function(x) {ircr(x) & iclr(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive even-valued
#'   whole-number object?
#'   (\code{\link{ircr}}, \code{\link{ievn}}).
#' @export
rcr_evn <- function(x) {ircr(x) & ievn(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive factor object?
#'   (\code{\link{ircr}}, \code{\link{ifac}}).
#' @export
rcr_fac <- function(x) {ircr(x) & ifac(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive fractional-valued
#'   numeric object?
#'   (\code{\link{ircr}}, \code{\link{ifrc}}).
#' @export
rcr_frc <- function(x) {ircr(x) & ifrc(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive indexer object?
#'   (\code{\link{ircr}}, \code{\link{iind}}).
#' @export
rcr_ind <- function(x) {ircr(x) & iind(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive logical object?
#'   (\code{\link{ircr}}, \code{\link{ilgl}}).
#' @export
rcr_lgl <- function(x) {ircr(x) & ilgl(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive negative-valued
#'   numeric object?
#'   (\code{\link{ircr}}, \code{\link{ineg}}).
#' @export
rcr_neg <- function(x) {ircr(x) & ineg(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive negative-valued
#'   whole-number object?
#'   (\code{\link{ircr}}, \code{\link{ingw}}).
#' @export
rcr_ngw <- function(x) {ircr(x) & ingw(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive non-negative-valued
#'   numeric object?
#'   (\code{\link{ircr}}, \code{\link{inng}}).
#' @export
rcr_nng <- function(x) {ircr(x) & inng(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive non-negative-valued
#'   whole-number object?
#'   (\code{\link{ircr}}, \code{\link{innw}}).
#' @export
rcr_nnw <- function(x) {ircr(x) & innw(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive non-positive-valued
#'   numeric object?
#'   (\code{\link{ircr}}, \code{\link{inps}}).
#' @export
rcr_nps <- function(x) {ircr(x) & inps(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive non-positive-valued
#'   whole-number object?
#'   (\code{\link{ircr}}, \code{\link{inpw}}).
#' @export
rcr_npw <- function(x) {ircr(x) & inpw(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive non-sortable object?
#'   (\code{\link{ircr}}, \code{\link{inst}}).
#' @export
rcr_nst <- function(x) {ircr(x) & inst(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive numeric object?
#'   (\code{\link{ircr}}, \code{\link{inum}}).
#' @export
rcr_num <- function(x) {ircr(x) & inum(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive odd-valued
#'   whole-number object?
#'   (\code{\link{ircr}}, \code{\link{iodd}}).
#' @export
rcr_odd <- function(x) {ircr(x) & iodd(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive ordered-factor object?
#'   (\code{\link{ircr}}, \code{\link{iord}}).
#' @export
rcr_ord <- function(x) {ircr(x) & iord(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive percent-valued
#'   (0-100) numeric object?
#'   (\code{\link{ircr}}, \code{\link{ipct}}).
#' @export
rcr_pct <- function(x) {ircr(x) & ipct(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive positive-valued
#'   numeric object?
#'   (\code{\link{ircr}}, \code{\link{ipos}}).
#' @export
rcr_pos <- function(x) {ircr(x) & ipos(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive proportion-valued
#'   (0-1) numeric object?
#'   (\code{\link{ircr}}, \code{\link{ippn}}).
#' @export
rcr_ppn <- function(x) {ircr(x) & ippn(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive positive-valued
#'   whole-number object?
#'   (\code{\link{ircr}}, \code{\link{ipsw}}).
#' @export
rcr_psw <- function(x) {ircr(x) & ipsw(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive sortable object?
#'   (\code{\link{ircr}}, \code{\link{isrt}}).
#' @export
rcr_srt <- function(x) {ircr(x) & isrt(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive string object?
#'   (\code{\link{ircr}}, \code{\link{istr}}).
#' @export
rcr_str <- function(x) {ircr(x) & istr(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive unordered-factor object?
#'   (\code{\link{ircr}}, \code{\link{iuno}}).
#' @export
rcr_uno <- function(x) {ircr(x) & iuno(x)}

#' @describeIn ttt_mmm. Is \code{x} a recursive whole-number object?
#'   (\code{\link{ircr}}, \code{\link{iwhl}}).
#' @export
rcr_whl <- function(x) {ircr(x) & iwhl(x)}
