#' @name xmd_vec
#' @family props
#' @title xmd + Atomic vect
#' @description See \code{\link{xmd}}, and \code{\link{is_atm_vect}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
xmd_vec_vals <- function() {
  x <- paste0("", c("atm", xmd_vals()), "_vec")
  names(x) <- rep.int("xmd_vec", length(x))
  x
}

#' @rdname xmd_vec
#' @export
atm_vec <- function(x) {is_atm_vect(x)}

#' @rdname xmd_vec
#' @export
chr_vec <- function(x) {is_atm_vect(x, "chr")}

#' @rdname xmd_vec
#' @export
clr_vec <- function(x) {is_atm_vect(x, "clr")}

#' @rdname xmd_vec
#' @export
evn_vec <- function(x) {is_atm_vect(x, "evn")}

#' @rdname xmd_vec
#' @export
fac_vec <- function(x) {is_atm_vect(x, "fac")}

#' @rdname xmd_vec
#' @export
frc_vec <- function(x) {is_atm_vect(x, "frc")}

#' @rdname xmd_vec
#' @export
ind_vec <- function(x) {is_atm_vect(x, "ind")}

#' @rdname xmd_vec
#' @export
lgc_vec <- function(x) {is_atm_vect(x, "lgc")}

#' @rdname xmd_vec
#' @export
cnb_vec <- function(x) {is_atm_vect(x, "cnb")}

#' @rdname xmd_vec
#' @export
neg_vec <- function(x) {is_atm_vect(x, "neg")}

#' @rdname xmd_vec
#' @export
ngw_vec <- function(x) {is_atm_vect(x, "ngw")}

#' @rdname xmd_vec
#' @export
nng_vec <- function(x) {is_atm_vect(x, "nng")}

#' @rdname xmd_vec
#' @export
nnw_vec <- function(x) {is_atm_vect(x, "nnw")}

#' @rdname xmd_vec
#' @export
nps_vec <- function(x) {is_atm_vect(x, "nps")}

#' @rdname xmd_vec
#' @export
npw_vec <- function(x) {is_atm_vect(x, "npw")}

#' @rdname xmd_vec
#' @export
nst_vec <- function(x) {is_atm_vect(x, "nst")}

#' @rdname xmd_vec
#' @export
num_vec <- function(x) {is_atm_vect(x, "num")}

#' @rdname xmd_vec
#' @export
odd_vec <- function(x) {is_atm_vect(x, "odd")}

#' @rdname xmd_vec
#' @export
ord_vec <- function(x) {is_atm_vect(x, "ord")}

#' @rdname xmd_vec
#' @export
pct_vec <- function(x) {is_atm_vect(x, "pct")}

#' @rdname xmd_vec
#' @export
pos_vec <- function(x) {is_atm_vect(x, "pos")}

#' @rdname xmd_vec
#' @export
ppn_vec <- function(x) {is_atm_vect(x, "ppn")}

#' @rdname xmd_vec
#' @export
psw_vec <- function(x) {is_atm_vect(x, "psw")}

#' @rdname xmd_vec
#' @export
srt_vec <- function(x) {is_atm_vect(x, "srt")}

#' @rdname xmd_vec
#' @export
str_vec <- function(x) {is_atm_vect(x, "str")}

#' @rdname xmd_vec
#' @export
uno_vec <- function(x) {is_atm_vect(x, "uno")}

#' @rdname xmd_vec
#' @export
whl_vec <- function(x) {is_atm_vect(x, "whl")}
