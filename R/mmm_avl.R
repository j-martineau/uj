#' @name mmm_avl
#' @family props
#' @title Extended Mode + Atomic vlist (mmm + avl)
#' @description See \code{\link{mmm}}, and \code{\link{is_atm_vlist}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_avl_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_avl")
  names(x) <- rep.int("mmm_avl", length(x))
  x
}

#' @rdname mmm_avl
#' @export
atm_avl <- function(x) {is_atm_vlist(x)}

#' @rdname mmm_avl
#' @export
chr_avl <- function(x) {is_atm_vlist(x, "chr")}

#' @rdname mmm_avl
#' @export
ch1_avl <- function(x) {is_atm_vlist(x, "ch1")}

#' @rdname mmm_avl
#' @export
clr_avl <- function(x) {is_atm_vlist(x, "clr")}

#' @rdname mmm_avl
#' @export
evn_avl <- function(x) {is_atm_vlist(x, "evn")}

#' @rdname mmm_avl
#' @export
fac_avl <- function(x) {is_atm_vlist(x, "fac")}

#' @rdname mmm_avl
#' @export
frc_avl <- function(x) {is_atm_vlist(x, "frc")}

#' @rdname mmm_avl
#' @export
ind_avl <- function(x) {is_atm_vlist(x, "ind")}

#' @rdname mmm_avl
#' @export
lgc_avl <- function(x) {is_atm_vlist(x, "lgc")}

#' @rdname mmm_avl
#' @export
cnb_avl <- function(x) {is_atm_vlist(x, "cnb")}

#' @rdname mmm_avl
#' @export
neg_avl <- function(x) {is_atm_vlist(x, "neg")}

#' @rdname mmm_avl
#' @export
ngw_avl <- function(x) {is_atm_vlist(x, "ngw")}

#' @rdname mmm_avl
#' @export
nng_avl <- function(x) {is_atm_vlist(x, "nng")}

#' @rdname mmm_avl
#' @export
nnw_avl <- function(x) {is_atm_vlist(x, "nnw")}

#' @rdname mmm_avl
#' @export
nps_avl <- function(x) {is_atm_vlist(x, "nps")}

#' @rdname mmm_avl
#' @export
npw_avl <- function(x) {is_atm_vlist(x, "npw")}

#' @rdname mmm_avl
#' @export
nst_avl <- function(x) {is_atm_vlist(x, "nst")}

#' @rdname mmm_avl
#' @export
num_avl <- function(x) {is_atm_vlist(x, "num")}

#' @rdname mmm_avl
#' @export
odd_avl <- function(x) {is_atm_vlist(x, "odd")}

#' @rdname mmm_avl
#' @export
ord_avl <- function(x) {is_atm_vlist(x, "ord")}

#' @rdname mmm_avl
#' @export
pct_avl <- function(x) {is_atm_vlist(x, "pct")}

#' @rdname mmm_avl
#' @export
pos_avl <- function(x) {is_atm_vlist(x, "pos")}

#' @rdname mmm_avl
#' @export
ppn_avl <- function(x) {is_atm_vlist(x, "ppn")}

#' @rdname mmm_avl
#' @export
psw_avl <- function(x) {is_atm_vlist(x, "psw")}

#' @rdname mmm_avl
#' @export
srt_avl <- function(x) {is_atm_vlist(x, "srt")}

#' @rdname mmm_avl
#' @export
str_avl <- function(x) {is_atm_vlist(x, "str")}

#' @rdname mmm_avl
#' @export
uno_avl <- function(x) {is_atm_vlist(x, "uno")}

#' @rdname mmm_avl
#' @export
whl_avl <- function(x) {is_atm_vlist(x, "whl")}
