#' @name xmd_avl
#' @family props
#' @title xmd + Atomic vlist
#' @description See \code{\link{xmd}}, and \code{\link{is_atm_vlist}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
xmd_avl_vals <- function() {
  x <- paste0("", c("atm", xmd_vals()), "_avl")
  names(x) <- rep.int("xmd_avl", length(x))
  x
}

#' @rdname xmd_avl
#' @export
atm_avl <- function(x) {is_atm_vlist(x)}

#' @rdname xmd_avl
#' @export
chr_avl <- function(x) {is_atm_vlist(x, "chr")}

#' @rdname xmd_avl
#' @export
ch1_avl <- function(x) {is_atm_vlist(x, "ch1")}

#' @rdname xmd_avl
#' @export
clr_avl <- function(x) {is_atm_vlist(x, "clr")}

#' @rdname xmd_avl
#' @export
evn_avl <- function(x) {is_atm_vlist(x, "evn")}

#' @rdname xmd_avl
#' @export
fac_avl <- function(x) {is_atm_vlist(x, "fac")}

#' @rdname xmd_avl
#' @export
frc_avl <- function(x) {is_atm_vlist(x, "frc")}

#' @rdname xmd_avl
#' @export
ind_avl <- function(x) {is_atm_vlist(x, "ind")}

#' @rdname xmd_avl
#' @export
lgc_avl <- function(x) {is_atm_vlist(x, "lgc")}

#' @rdname xmd_avl
#' @export
cnb_avl <- function(x) {is_atm_vlist(x, "cnb")}

#' @rdname xmd_avl
#' @export
neg_avl <- function(x) {is_atm_vlist(x, "neg")}

#' @rdname xmd_avl
#' @export
ngw_avl <- function(x) {is_atm_vlist(x, "ngw")}

#' @rdname xmd_avl
#' @export
nng_avl <- function(x) {is_atm_vlist(x, "nng")}

#' @rdname xmd_avl
#' @export
nnw_avl <- function(x) {is_atm_vlist(x, "nnw")}

#' @rdname xmd_avl
#' @export
nps_avl <- function(x) {is_atm_vlist(x, "nps")}

#' @rdname xmd_avl
#' @export
npw_avl <- function(x) {is_atm_vlist(x, "npw")}

#' @rdname xmd_avl
#' @export
nst_avl <- function(x) {is_atm_vlist(x, "nst")}

#' @rdname xmd_avl
#' @export
num_avl <- function(x) {is_atm_vlist(x, "num")}

#' @rdname xmd_avl
#' @export
odd_avl <- function(x) {is_atm_vlist(x, "odd")}

#' @rdname xmd_avl
#' @export
ord_avl <- function(x) {is_atm_vlist(x, "ord")}

#' @rdname xmd_avl
#' @export
pct_avl <- function(x) {is_atm_vlist(x, "pct")}

#' @rdname xmd_avl
#' @export
pos_avl <- function(x) {is_atm_vlist(x, "pos")}

#' @rdname xmd_avl
#' @export
ppn_avl <- function(x) {is_atm_vlist(x, "ppn")}

#' @rdname xmd_avl
#' @export
psw_avl <- function(x) {is_atm_vlist(x, "psw")}

#' @rdname xmd_avl
#' @export
srt_avl <- function(x) {is_atm_vlist(x, "srt")}

#' @rdname xmd_avl
#' @export
str_avl <- function(x) {is_atm_vlist(x, "str")}

#' @rdname xmd_avl
#' @export
uno_avl <- function(x) {is_atm_vlist(x, "uno")}

#' @rdname xmd_avl
#' @export
whl_avl <- function(x) {is_atm_vlist(x, "whl")}
