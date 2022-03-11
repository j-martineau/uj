#' @name mmm_agn
#' @family props
#' @title Extended Mode + Atomic Generic (mmm + agn)
#' @description See \code{\link{mmm}}, and \code{\link{is_atm_generic}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_agn_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_agn")
  names(x) <- rep.int("mmm_agn", length(x))
  x
}

#' @rdname mmm_agn
#' @export
atm_agn <- function(x) {is_atm_generic(x)}

#' @rdname mmm_agn
#' @export
chr_agn <- function(x) {is_atm_generic(x, "chr")}

#' @rdname mmm_agn
#' @export
ch1_agn <- function(x) {is_atm_generic(x, "ch1")}

#' @rdname mmm_agn
#' @export
clr_agn <- function(x) {is_atm_generic(x, "clr")}

#' @rdname mmm_agn
#' @export
evn_agn <- function(x) {is_atm_generic(x, "evn")}

#' @rdname mmm_agn
#' @export
fac_agn <- function(x) {is_atm_generic(x, "fac")}

#' @rdname mmm_agn
#' @export
frc_agn <- function(x) {is_atm_generic(x, "frc")}

#' @rdname mmm_agn
#' @export
ind_agn <- function(x) {is_atm_generic(x, "ind")}

#' @rdname mmm_agn
#' @export
lgc_agn <- function(x) {is_atm_generic(x, "lgc")}

#' @rdname mmm_agn
#' @export
cnb_agn <- function(x) {is_atm_generic(x, "cnb")}

#' @rdname mmm_agn
#' @export
neg_agn <- function(x) {is_atm_generic(x, "neg")}

#' @rdname mmm_agn
#' @export
ngw_agn <- function(x) {is_atm_generic(x, "ngw")}

#' @rdname mmm_agn
#' @export
nng_agn <- function(x) {is_atm_generic(x, "nng")}

#' @rdname mmm_agn
#' @export
nnw_agn <- function(x) {is_atm_generic(x, "nnw")}

#' @rdname mmm_agn
#' @export
nps_agn <- function(x) {is_atm_generic(x, "nps")}

#' @rdname mmm_agn
#' @export
npw_agn <- function(x) {is_atm_generic(x, "npw")}

#' @rdname mmm_agn
#' @export
nst_agn <- function(x) {is_atm_generic(x, "nst")}

#' @rdname mmm_agn
#' @export
num_agn <- function(x) {is_atm_generic(x, "num")}

#' @rdname mmm_agn
#' @export
odd_agn <- function(x) {is_atm_generic(x, "odd")}

#' @rdname mmm_agn
#' @export
ord_agn <- function(x) {is_atm_generic(x, "ord")}

#' @rdname mmm_agn
#' @export
pct_agn <- function(x) {is_atm_generic(x, "pct")}

#' @rdname mmm_agn
#' @export
pos_agn <- function(x) {is_atm_generic(x, "pos")}

#' @rdname mmm_agn
#' @export
ppn_agn <- function(x) {is_atm_generic(x, "ppn")}

#' @rdname mmm_agn
#' @export
psw_agn <- function(x) {is_atm_generic(x, "psw")}

#' @rdname mmm_agn
#' @export
srt_agn <- function(x) {is_atm_generic(x, "srt")}

#' @rdname mmm_agn
#' @export
str_agn <- function(x) {is_atm_generic(x, "str")}

#' @rdname mmm_agn
#' @export
uno_agn <- function(x) {is_atm_generic(x, "uno")}

#' @rdname mmm_agn
#' @export
whl_agn <- function(x) {is_atm_generic(x, "whl")}
