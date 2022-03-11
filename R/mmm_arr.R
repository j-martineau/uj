#' @name mmm_arr
#' @family props
#' @title Extended Mode + Atomic array (mmm + arr)
#' @description See \code{\link{mmm}} and \code{\link{is_atm_array}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_arr_vals <- function() {
  x <- paste0(c("atm", mmm_vals()), "_arr")
  names(x) <- rep.int("mmm_arr", length(x))
  x
}

#' @rdname mmm_arr
#' @export
atm_arr <- function(x) {is_atm_array(x)}

#' @rdname mmm_arr
#' @export
chr_arr <- function(x) {f0(!is_atm_array(x), F, xchr(x))}

#' @rdname mmm_arr
#' @export
ch1_arr <- function(x) {f0(!is_atm_array(x), F, xch1(x))}

#' @rdname mmm_arr
#' @export
clr_arr <- function(x) {f0(!is_atm_array(x), F, xclr(x))}

#' @rdname mmm_arr
#' @export
evn_arr <- function(x) {f0(!is_atm_array(x), F, xevn(x))}

#' @rdname mmm_arr
#' @export
fac_arr <- function(x) {f0(!is_atm_array(x), F, xfac(x))}

#' @rdname mmm_arr
#' @export
frc_arr <- function(x) {f0(!is_atm_array(x), F, xfrc(x))}

#' @rdname mmm_arr
#' @export
ind_arr <- function(x) {f0(!is_atm_array(x), F, xind(x))}

#' @rdname mmm_arr
#' @export
lgc_arr <- function(x) {f0(!is_atm_array(x), F, xlgc(x))}

#' @rdname mmm_arr
#' @export
neg_arr <- function(x) {f0(!is_atm_array(x), F, xneg(x))}

#' @rdname mmm_arr
#' @export
ngw_arr <- function(x) {f0(!is_atm_array(x), F, xngw(x))}

#' @rdname mmm_arr
#' @export
nng_arr <- function(x) {f0(!is_atm_array(x), F, xnng(x))}

#' @rdname mmm_arr
#' @export
nnw_arr <- function(x) {f0(!is_atm_array(x), F, xnnw(x))}

#' @rdname mmm_arr
#' @export
nps_arr <- function(x) {f0(!is_atm_array(x), F, xnps(x))}

#' @rdname mmm_arr
#' @export
npw_arr <- function(x) {f0(!is_atm_array(x), F, xnpw(x))}

#' @rdname mmm_arr
#' @export
nst_arr <- function(x) {f0(!is_atm_array(x), F, xnst(x))}

#' @rdname mmm_arr
#' @export
num_arr <- function(x) {f0(!is_atm_array(x), F, xnum(x))}

#' @rdname mmm_arr
#' @export
odd_arr <- function(x) {f0(!is_atm_array(x), F, xodd(x))}

#' @rdname mmm_arr
#' @export
ord_arr <- function(x) {f0(!is_atm_array(x), F, xord(x))}

#' @rdname mmm_arr
#' @export
pct_arr <- function(x) {f0(!is_atm_array(x), F, xpct(x))}

#' @rdname mmm_arr
#' @export
pos_arr <- function(x) {f0(!is_atm_array(x), F, xpos(x))}

#' @rdname mmm_arr
#' @export
psw_arr <- function(x) {f0(!is_atm_array(x), F, xpsw(x))}

#' @rdname mmm_arr
#' @export
ppn_arr <- function(x) {f0(!is_atm_array(x), F, xppn(x))}

#' @rdname mmm_arr
#' @export
srt_arr <- function(x) {f0(!is_atm_array(x), F, xsrt(x))}

#' @rdname mmm_arr
#' @export
str_arr <- function(x) {f0(!is_atm_array(x), F, xstr(x))}

#' @rdname mmm_arr
#' @export
uno_arr <- function(x) {f0(!is_atm_array(x), F, xuno(x))}

#' @rdname mmm_arr
#' @export
whl_arr <- function(x) {f0(!is_atm_array(x), F, xwhl(x))}
