#' @name mmm_atb
#' @family props
#' @title Extended Mode + Atomic tibble (mmm + atb)
#' @description See \code{\link{mmm}} and \code{\link{is_atm_tibble}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_atb_vals <- function() {
  x <- paste0(c("atm", mmm_vals()), "_atb")
  names(x) <- rep.int("mmm_atb", length(x))
  x
}

#' @rdname mmm_atb
#' @export
atm_atb <- function(x) {is_atm_tibble(x)}

#' @rdname mmm_atb
#' @export
chr_atb <- function(x) {f0(!is_atm_tibble(x), F, xchr(x))}

#' @rdname mmm_atb
#' @export
ch1_atb <- function(x) {f0(!is_atm_tibble(x), F, xch1(x))}

#' @rdname mmm_atb
#' @export
clr_atb <- function(x) {f0(!is_atm_tibble(x), F, xclr(x))}

#' @rdname mmm_atb
#' @export
evn_atb <- function(x) {f0(!is_atm_tibble(x), F, xevn(x))}

#' @rdname mmm_atb
#' @export
fac_atb <- function(x) {f0(!is_atm_tibble(x), F, xfac(x))}

#' @rdname mmm_atb
#' @export
frc_atb <- function(x) {f0(!is_atm_tibble(x), F, xfrc(x))}

#' @rdname mmm_atb
#' @export
ind_atb <- function(x) {f0(!is_atm_tibble(x), F, xind(x))}

#' @rdname mmm_atb
#' @export
lgc_atb <- function(x) {f0(!is_atm_tibble(x), F, xlgc(x))}

#' @rdname mmm_atb
#' @export
neg_atb <- function(x) {f0(!is_atm_tibble(x), F, xneg(x))}

#' @rdname mmm_atb
#' @export
ngw_atb <- function(x) {f0(!is_atm_tibble(x), F, xngw(x))}

#' @rdname mmm_atb
#' @export
nng_atb <- function(x) {f0(!is_atm_tibble(x), F, xnng(x))}

#' @rdname mmm_atb
#' @export
nnw_atb <- function(x) {f0(!is_atm_tibble(x), F, xnnw(x))}

#' @rdname mmm_atb
#' @export
nps_atb <- function(x) {f0(!is_atm_tibble(x), F, xnps(x))}

#' @rdname mmm_atb
#' @export
npw_atb <- function(x) {f0(!is_atm_tibble(x), F, xnpw(x))}

#' @rdname mmm_atb
#' @export
nst_atb <- function(x) {f0(!is_atm_tibble(x), F, xnst(x))}

#' @rdname mmm_atb
#' @export
num_atb <- function(x) {f0(!is_atm_tibble(x), F, xnum(x))}

#' @rdname mmm_atb
#' @export
odd_atb <- function(x) {f0(!is_atm_tibble(x), F, xodd(x))}

#' @rdname mmm_atb
#' @export
ord_atb <- function(x) {f0(!is_atm_tibble(x), F, xord(x))}

#' @rdname mmm_atb
#' @export
pct_atb <- function(x) {f0(!is_atm_tibble(x), F, xpct(x))}

#' @rdname mmm_atb
#' @export
pos_atb <- function(x) {f0(!is_atm_tibble(x), F, xpos(x))}

#' @rdname mmm_atb
#' @export
psw_atb <- function(x) {f0(!is_atm_tibble(x), F, xpsw(x))}

#' @rdname mmm_atb
#' @export
ppn_atb <- function(x) {f0(!is_atm_tibble(x), F, xppn(x))}

#' @rdname mmm_atb
#' @export
srt_atb <- function(x) {f0(!is_atm_tibble(x), F, xsrt(x))}

#' @rdname mmm_atb
#' @export
str_atb <- function(x) {f0(!is_atm_tibble(x), F, xstr(x))}

#' @rdname mmm_atb
#' @export
uno_atb <- function(x) {f0(!is_atm_tibble(x), F, xuno(x))}

#' @rdname mmm_atb
#' @export
whl_atb <- function(x) {f0(!is_atm_tibble(x), F, xwhl(x))}
