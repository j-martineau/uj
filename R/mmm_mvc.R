#' @name mmm_mvc
#' @family props
#' @title Extended Mode + Atomic mvect (mmm + mvc)
#' @description See \code{\link{mmm}}, and \code{\link{is_atm_mvect}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_mvc_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_mvc")
  names(x) <- rep.int("mmm_mvc", length(x))
  x
}

#' @rdname mmm_mvc
#' @export
atm_mvc <- function(x) {is_atm_mvect(x)}

#' @rdname mmm_mvc
#' @export
chr_mvc <- function(x) {is_atm_mvect(x, "chr")}

#' @rdname mmm_mvc
#' @export
ch1_mvc <- function(x) {is_atm_mvect(x, "ch1")}

#' @rdname mmm_mvc
#' @export
clr_mvc <- function(x) {is_atm_mvect(x, "clr")}

#' @rdname mmm_mvc
#' @export
evn_mvc <- function(x) {is_atm_mvect(x, "evn")}

#' @rdname mmm_mvc
#' @export
fac_mvc <- function(x) {is_atm_mvect(x, "fac")}

#' @rdname mmm_mvc
#' @export
frc_mvc <- function(x) {is_atm_mvect(x, "frc")}

#' @rdname mmm_mvc
#' @export
ind_mvc <- function(x) {is_atm_mvect(x, "ind")}

#' @rdname mmm_mvc
#' @export
lgc_mvc <- function(x) {is_atm_mvect(x, "lgc")}

#' @rdname mmm_mvc
#' @export
cnb_mvc <- function(x) {is_atm_mvect(x, "cnb")}

#' @rdname mmm_mvc
#' @export
neg_mvc <- function(x) {is_atm_mvect(x, "neg")}

#' @rdname mmm_mvc
#' @export
ngw_mvc <- function(x) {is_atm_mvect(x, "ngw")}

#' @rdname mmm_mvc
#' @export
nng_mvc <- function(x) {is_atm_mvect(x, "nng")}

#' @rdname mmm_mvc
#' @export
nnw_mvc <- function(x) {is_atm_mvect(x, "nnw")}

#' @rdname mmm_mvc
#' @export
nps_mvc <- function(x) {is_atm_mvect(x, "nps")}

#' @rdname mmm_mvc
#' @export
npw_mvc <- function(x) {is_atm_mvect(x, "npw")}

#' @rdname mmm_mvc
#' @export
nst_mvc <- function(x) {is_atm_mvect(x, "nst")}

#' @rdname mmm_mvc
#' @export
num_mvc <- function(x) {is_atm_mvect(x, "num")}

#' @rdname mmm_mvc
#' @export
odd_mvc <- function(x) {is_atm_mvect(x, "odd")}

#' @rdname mmm_mvc
#' @export
ord_mvc <- function(x) {is_atm_mvect(x, "ord")}

#' @rdname mmm_mvc
#' @export
pct_mvc <- function(x) {is_atm_mvect(x, "pct")}

#' @rdname mmm_mvc
#' @export
pos_mvc <- function(x) {is_atm_mvect(x, "pos")}

#' @rdname mmm_mvc
#' @export
ppn_mvc <- function(x) {is_atm_mvect(x, "ppn")}

#' @rdname mmm_mvc
#' @export
psw_mvc <- function(x) {is_atm_mvect(x, "psw")}

#' @rdname mmm_mvc
#' @export
srt_mvc <- function(x) {is_atm_mvect(x, "srt")}

#' @rdname mmm_mvc
#' @export
str_mvc <- function(x) {is_atm_mvect(x, "str")}

#' @rdname mmm_mvc
#' @export
uno_mvc <- function(x) {is_atm_mvect(x, "uno")}

#' @rdname mmm_mvc
#' @export
whl_mvc <- function(x) {is_atm_mvect(x, "whl")}
