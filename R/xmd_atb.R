#' @name xmd_atb
#' @family props
#' @title xmd + atomic tibble
#' @description See \code{\link{xmd}} and \code{\link{is_atm_tibble}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
xmd_atb_vals <- function() {
  x <- paste0(c("atm", xmd_vals()), "_atb")
  names(x) <- rep.int("xmd_atb", length(x))
  x
}

#' @rdname xmd_atb
#' @export
atm_atb <- function(x) {is_atm_tibble(x)}

#' @rdname xmd_atb
#' @export
chr_atb <- function(x) {f0(!is_atm_tibble(x), F, xchr(x))}

#' @rdname xmd_atb
#' @export
ch1_atb <- function(x) {f0(!is_atm_tibble(x), F, xch1(x))}

#' @rdname xmd_atb
#' @export
clr_atb <- function(x) {f0(!is_atm_tibble(x), F, xclr(x))}

#' @rdname xmd_atb
#' @export
evn_atb <- function(x) {f0(!is_atm_tibble(x), F, xevn(x))}

#' @rdname xmd_atb
#' @export
fac_atb <- function(x) {f0(!is_atm_tibble(x), F, xfac(x))}

#' @rdname xmd_atb
#' @export
frc_atb <- function(x) {f0(!is_atm_tibble(x), F, xfrc(x))}

#' @rdname xmd_atb
#' @export
ind_atb <- function(x) {f0(!is_atm_tibble(x), F, xind(x))}

#' @rdname xmd_atb
#' @export
lgc_atb <- function(x) {f0(!is_atm_tibble(x), F, xlgc(x))}

#' @rdname xmd_atb
#' @export
neg_atb <- function(x) {f0(!is_atm_tibble(x), F, xneg(x))}

#' @rdname xmd_atb
#' @export
ngw_atb <- function(x) {f0(!is_atm_tibble(x), F, xngw(x))}

#' @rdname xmd_atb
#' @export
nng_atb <- function(x) {f0(!is_atm_tibble(x), F, xnng(x))}

#' @rdname xmd_atb
#' @export
nnw_atb <- function(x) {f0(!is_atm_tibble(x), F, xnnw(x))}

#' @rdname xmd_atb
#' @export
nps_atb <- function(x) {f0(!is_atm_tibble(x), F, xnps(x))}

#' @rdname xmd_atb
#' @export
npw_atb <- function(x) {f0(!is_atm_tibble(x), F, xnpw(x))}

#' @rdname xmd_atb
#' @export
nst_atb <- function(x) {f0(!is_atm_tibble(x), F, xnst(x))}

#' @rdname xmd_atb
#' @export
num_atb <- function(x) {f0(!is_atm_tibble(x), F, xnum(x))}

#' @rdname xmd_atb
#' @export
odd_atb <- function(x) {f0(!is_atm_tibble(x), F, xodd(x))}

#' @rdname xmd_atb
#' @export
ord_atb <- function(x) {f0(!is_atm_tibble(x), F, xord(x))}

#' @rdname xmd_atb
#' @export
pct_atb <- function(x) {f0(!is_atm_tibble(x), F, xpct(x))}

#' @rdname xmd_atb
#' @export
pos_atb <- function(x) {f0(!is_atm_tibble(x), F, xpos(x))}

#' @rdname xmd_atb
#' @export
psw_atb <- function(x) {f0(!is_atm_tibble(x), F, xpsw(x))}

#' @rdname xmd_atb
#' @export
ppn_atb <- function(x) {f0(!is_atm_tibble(x), F, xppn(x))}

#' @rdname xmd_atb
#' @export
srt_atb <- function(x) {f0(!is_atm_tibble(x), F, xsrt(x))}

#' @rdname xmd_atb
#' @export
str_atb <- function(x) {f0(!is_atm_tibble(x), F, xstr(x))}

#' @rdname xmd_atb
#' @export
uno_atb <- function(x) {f0(!is_atm_tibble(x), F, xuno(x))}

#' @rdname xmd_atb
#' @export
whl_atb <- function(x) {f0(!is_atm_tibble(x), F, xwhl(x))}
