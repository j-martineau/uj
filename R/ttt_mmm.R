#' @name ttt_mmm.
#' @family props
#' @title Fundamental Type + Extended Mode Properties
#' @description \tabular{ll}{
#'   \code{ttt_mmm_vals}   \tab Gets a character vector of all possible
#'                              \link[=ttt]{fundamental type} +
#'                              \link[=mmm]{extended mode} properties.       \cr
#'   \code{[ttt]_[mmm]}    \tab Evaluates whether \code{x} is of the
#'                              \link[=ttt]{fundamental type} represented by the
#'                              placeholder \code{[ttt]} and of the
#'                              \link[=mmm]{extended mode} represented by the
#'                              placeholder \code{[mmm]}.                      }
#' @param x An R object.
#' @return \tabular{ll}{
#'   \code{ttt_mmm_vals}   \tab A character vector.                          \cr
#'   \code{[ttt]_[mmm]}    \tab A logical scalar.                              }
#' @export
ttt_mmm. <- function() {help("ttt_mmm.", package = "uj")}

#' @rdname ttt_mmm.
#' @export
ttt_mmm_vals <- function() {
  join <- function(x) {paste0(av(x), collapse = "_")}
  x <- sort(unique(c("pop_atm", av(apply(expand.grid(ttt = c("atm", "pop", "cmp", "emp"), mmm = mmm_vals()), 1, join)))))
  x <- x[x != "atm_atm"]
}

#' @rdname ttt_mmm.
#' @export
atm_ch1 <- function(x) {iatm(x) & ich1(x)}

#' @rdname ttt_mmm.
#' @export
atm_chr <- function(x) {iatm(x) & ichr(x)}

#' @rdname ttt_mmm.
#' @export
atm_clr <- function(x) {iatm(x) & iclr(x)}

#' @rdname ttt_mmm.
#' @export
atm_evn <- function(x) {iatm(x) & ievn(x)}

#' @rdname ttt_mmm.
#' @export
atm_fac <- function(x) {iatm(x) & ifac(x)}

#' @rdname ttt_mmm.
#' @export
atm_frc <- function(x) {iatm(x) & ifrc(x)}

#' @rdname ttt_mmm.
#' @export
atm_ind <- function(x) {iatm(x) & iind(x)}

#' @rdname ttt_mmm.
#' @export
atm_lgl <- function(x) {iatm(x) & ilgl(x)}

#' @rdname ttt_mmm.
#' @export
atm_neg <- function(x) {iatm(x) & ineg(x)}

#' @rdname ttt_mmm.
#' @export
atm_ngw <- function(x) {iatm(x) & ingw(x)}

#' @rdname ttt_mmm.
#' @export
atm_nng <- function(x) {iatm(x) & inng(x)}

#' @rdname ttt_mmm.
#' @export
atm_nnw <- function(x) {iatm(x) & innw(x)}

#' @rdname ttt_mmm.
#' @export
atm_nps <- function(x) {iatm(x) & inps(x)}

#' @rdname ttt_mmm.
#' @export
atm_npw <- function(x) {iatm(x) & inpw(x)}

#' @rdname ttt_mmm.
#' @export
atm_nst <- function(x) {iatm(x) & inst(x)}

#' @rdname ttt_mmm.
#' @export
atm_num <- function(x) {iatm(x) & inum(x)}

#' @rdname ttt_mmm.
#' @export
atm_odd <- function(x) {iatm(x) & iodd(x)}

#' @rdname ttt_mmm.
#' @export
atm_ord <- function(x) {iatm(x) & iord(x)}

#' @rdname ttt_mmm.
#' @export
atm_pct <- function(x) {iatm(x) & ipct(x)}

#' @rdname ttt_mmm.
#' @export
atm_pos <- function(x) {iatm(x) & ipos(x)}

#' @rdname ttt_mmm.
#' @export
atm_ppn <- function(x) {iatm(x) & ippn(x)}

#' @rdname ttt_mmm.
#' @export
atm_psw <- function(x) {iatm(x) & ipsw(x)}

#' @rdname ttt_mmm.
#' @export
atm_srt <- function(x) {iatm(x) & isrt(x)}

#' @rdname ttt_mmm.
#' @export
atm_str <- function(x) {iatm(x) & istr(x)}

#' @rdname ttt_mmm.
#' @export
atm_uno <- function(x) {iatm(x) & iuno(x)}

#' @rdname ttt_mmm.
#' @export
atm_whl <- function(x) {iatm(x) & iwhl(x)}

#' @rdname ttt_mmm.
#' @export
pop_atm <- function(x) {ipop(x) & iatm(x)}

#' @rdname ttt_mmm.
#' @export
pop_ch1 <- function(x) {ipop(x) & ich1(x)}

#' @rdname ttt_mmm.
#' @export
pop_chr <- function(x) {ipop(x) & ichr(x)}

#' @rdname ttt_mmm.
#' @export
pop_clr <- function(x) {ipop(x) & iclr(x)}

#' @rdname ttt_mmm.
#' @export
pop_evn <- function(x) {ipop(x) & ievn(x)}

#' @rdname ttt_mmm.
#' @export
pop_fac <- function(x) {ipop(x) & ifac(x)}

#' @rdname ttt_mmm.
#' @export
pop_frc <- function(x) {ipop(x) & ifrc(x)}

#' @rdname ttt_mmm.
#' @export
pop_ind <- function(x) {ipop(x) & iind(x)}

#' @rdname ttt_mmm.
#' @export
pop_lgl <- function(x) {ipop(x) & ilgl(x)}

#' @rdname ttt_mmm.
#' @export
pop_neg <- function(x) {ipop(x) & ineg(x)}

#' @rdname ttt_mmm.
#' @export
pop_ngw <- function(x) {ipop(x) & ingw(x)}

#' @rdname ttt_mmm.
#' @export
pop_nng <- function(x) {ipop(x) & inng(x)}

#' @rdname ttt_mmm.
#' @export
pop_nnw <- function(x) {ipop(x) & innw(x)}

#' @rdname ttt_mmm.
#' @export
pop_nps <- function(x) {ipop(x) & inps(x)}

#' @rdname ttt_mmm.
#' @export
pop_npw <- function(x) {ipop(x) & inpw(x)}

#' @rdname ttt_mmm.
#' @export
pop_nst <- function(x) {ipop(x) & inst(x)}

#' @rdname ttt_mmm.
#' @export
pop_num <- function(x) {ipop(x) & inum(x)}

#' @rdname ttt_mmm.
#' @export
pop_odd <- function(x) {ipop(x) & iodd(x)}

#' @rdname ttt_mmm.
#' @export
pop_ord <- function(x) {ipop(x) & iord(x)}

#' @rdname ttt_mmm.
#' @export
pop_pct <- function(x) {ipop(x) & ipct(x)}

#' @rdname ttt_mmm.
#' @export
pop_pos <- function(x) {ipop(x) & ipos(x)}

#' @rdname ttt_mmm.
#' @export
pop_ppn <- function(x) {ipop(x) & ippn(x)}

#' @rdname ttt_mmm.
#' @export
pop_psw <- function(x) {ipop(x) & ipsw(x)}

#' @rdname ttt_mmm.
#' @export
pop_srt <- function(x) {ipop(x) & isrt(x)}

#' @rdname ttt_mmm.
#' @export
pop_str <- function(x) {ipop(x) & istr(x)}

#' @rdname ttt_mmm.
#' @export
pop_uno <- function(x) {ipop(x) & iuno(x)}

#' @rdname ttt_mmm.
#' @export
pop_whl <- function(x) {ipop(x) & iwhl(x)}

#' @rdname ttt_mmm.
#' @export
cmp_atm <- function(x) {icmp(x) & iatm(x)}

#' @rdname ttt_mmm.
#' @export
cmp_ch1 <- function(x) {icmp(x) & ich1(x)}

#' @rdname ttt_mmm.
#' @export
cmp_chr <- function(x) {icmp(x) & ichr(x)}

#' @rdname ttt_mmm.
#' @export
cmp_clr <- function(x) {icmp(x) & iclr(x)}

#' @rdname ttt_mmm.
#' @export
cmp_evn <- function(x) {icmp(x) & ievn(x)}

#' @rdname ttt_mmm.
#' @export
cmp_fac <- function(x) {icmp(x) & ifac(x)}

#' @rdname ttt_mmm.
#' @export
cmp_frc <- function(x) {icmp(x) & ifrc(x)}

#' @rdname ttt_mmm.
#' @export
cmp_ind <- function(x) {icmp(x) & iind(x)}

#' @rdname ttt_mmm.
#' @export
cmp_lgl <- function(x) {icmp(x) & ilgl(x)}

#' @rdname ttt_mmm.
#' @export
cmp_neg <- function(x) {icmp(x) & ineg(x)}

#' @rdname ttt_mmm.
#' @export
cmp_ngw <- function(x) {icmp(x) & ingw(x)}

#' @rdname ttt_mmm.
#' @export
cmp_nng <- function(x) {icmp(x) & inng(x)}

#' @rdname ttt_mmm.
#' @export
cmp_nnw <- function(x) {icmp(x) & innw(x)}

#' @rdname ttt_mmm.
#' @export
cmp_nps <- function(x) {icmp(x) & inps(x)}

#' @rdname ttt_mmm.
#' @export
cmp_npw <- function(x) {icmp(x) & inpw(x)}

#' @rdname ttt_mmm.
#' @export
cmp_nst <- function(x) {icmp(x) & inst(x)}

#' @rdname ttt_mmm.
#' @export
cmp_num <- function(x) {icmp(x) & inum(x)}

#' @rdname ttt_mmm.
#' @export
cmp_odd <- function(x) {icmp(x) & iodd(x)}

#' @rdname ttt_mmm.
#' @export
cmp_ord <- function(x) {icmp(x) & iord(x)}

#' @rdname ttt_mmm.
#' @export
cmp_pct <- function(x) {icmp(x) & ipct(x)}

#' @rdname ttt_mmm.
#' @export
cmp_pos <- function(x) {icmp(x) & ipos(x)}

#' @rdname ttt_mmm.
#' @export
cmp_ppn <- function(x) {icmp(x) & ippn(x)}

#' @rdname ttt_mmm.
#' @export
cmp_psw <- function(x) {icmp(x) & ipsw(x)}

#' @rdname ttt_mmm.
#' @export
cmp_srt <- function(x) {icmp(x) & isrt(x)}

#' @rdname ttt_mmm.
#' @export
cmp_str <- function(x) {icmp(x) & istr(x)}

#' @rdname ttt_mmm.
#' @export
cmp_uno <- function(x) {icmp(x) & iuno(x)}

#' @rdname ttt_mmm.
#' @export
cmp_whl <- function(x) {icmp(x) & iwhl(x)}

#' @rdname ttt_mmm.
#' @export
emp_atm <- function(x) {iemp(x) & iatm(x)}

#' @rdname ttt_mmm.
#' @export
emp_ch1 <- function(x) {iemp(x) & ich1(x)}

#' @rdname ttt_mmm.
#' @export
emp_chr <- function(x) {iemp(x) & ichr(x)}

#' @rdname ttt_mmm.
#' @export
emp_clr <- function(x) {iemp(x) & iclr(x)}

#' @rdname ttt_mmm.
#' @export
emp_evn <- function(x) {iemp(x) & ievn(x)}

#' @rdname ttt_mmm.
#' @export
emp_fac <- function(x) {iemp(x) & ifac(x)}

#' @rdname ttt_mmm.
#' @export
emp_frc <- function(x) {iemp(x) & ifrc(x)}

#' @rdname ttt_mmm.
#' @export
emp_ind <- function(x) {iemp(x) & iind(x)}

#' @rdname ttt_mmm.
#' @export
emp_lgl <- function(x) {iemp(x) & ilgl(x)}

#' @rdname ttt_mmm.
#' @export
emp_neg <- function(x) {iemp(x) & ineg(x)}

#' @rdname ttt_mmm.
#' @export
emp_ngw <- function(x) {iemp(x) & ingw(x)}

#' @rdname ttt_mmm.
#' @export
emp_nng <- function(x) {iemp(x) & inng(x)}

#' @rdname ttt_mmm.
#' @export
emp_nnw <- function(x) {iemp(x) & innw(x)}

#' @rdname ttt_mmm.
#' @export
emp_nps <- function(x) {iemp(x) & inps(x)}

#' @rdname ttt_mmm.
#' @export
emp_npw <- function(x) {iemp(x) & inpw(x)}

#' @rdname ttt_mmm.
#' @export
emp_nst <- function(x) {iemp(x) & inst(x)}

#' @rdname ttt_mmm.
#' @export
emp_num <- function(x) {iemp(x) & inum(x)}

#' @rdname ttt_mmm.
#' @export
emp_odd <- function(x) {iemp(x) & iodd(x)}

#' @rdname ttt_mmm.
#' @export
emp_ord <- function(x) {iemp(x) & iord(x)}

#' @rdname ttt_mmm.
#' @export
emp_pct <- function(x) {iemp(x) & ipct(x)}

#' @rdname ttt_mmm.
#' @export
emp_pos <- function(x) {iemp(x) & ipos(x)}

#' @rdname ttt_mmm.
#' @export
emp_ppn <- function(x) {iemp(x) & ippn(x)}

#' @rdname ttt_mmm.
#' @export
emp_psw <- function(x) {iemp(x) & ipsw(x)}

#' @rdname ttt_mmm.
#' @export
emp_srt <- function(x) {iemp(x) & isrt(x)}

#' @rdname ttt_mmm.
#' @export
emp_str <- function(x) {iemp(x) & istr(x)}

#' @rdname ttt_mmm.
#' @export
emp_uno <- function(x) {iemp(x) & iuno(x)}

#' @rdname ttt_mmm.
#' @export
emp_whl <- function(x) {iemp(x) & iwhl(x)}
