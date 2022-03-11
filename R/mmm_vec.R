#' @name mmm_vec
#' @family props
#' @title Extended Mode + Atomic vect (mmm + vec)
#' @description See \code{\link{mmm}}, and \code{\link{is_atm_vect}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_vec_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_vec")
  names(x) <- rep.int("mmm_vec", length(x))
  x
}

#' @rdname mmm_vec
#' @export
atm_vec <- function(x) {is_atm_vect(x)}

#' @rdname mmm_vec
#' @export
chr_vec <- function(x) {is_atm_vect(x, "chr")}

#' @rdname mmm_vec
#' @export
clr_vec <- function(x) {is_atm_vect(x, "clr")}

#' @rdname mmm_vec
#' @export
evn_vec <- function(x) {is_atm_vect(x, "evn")}

#' @rdname mmm_vec
#' @export
fac_vec <- function(x) {is_atm_vect(x, "fac")}

#' @rdname mmm_vec
#' @export
frc_vec <- function(x) {is_atm_vect(x, "frc")}

#' @rdname mmm_vec
#' @export
ind_vec <- function(x) {is_atm_vect(x, "ind")}

#' @rdname mmm_vec
#' @export
lgc_vec <- function(x) {is_atm_vect(x, "lgc")}

#' @rdname mmm_vec
#' @export
cnb_vec <- function(x) {is_atm_vect(x, "cnb")}

#' @rdname mmm_vec
#' @export
neg_vec <- function(x) {is_atm_vect(x, "neg")}

#' @rdname mmm_vec
#' @export
ngw_vec <- function(x) {is_atm_vect(x, "ngw")}

#' @rdname mmm_vec
#' @export
nng_vec <- function(x) {is_atm_vect(x, "nng")}

#' @rdname mmm_vec
#' @export
nnw_vec <- function(x) {is_atm_vect(x, "nnw")}

#' @rdname mmm_vec
#' @export
nps_vec <- function(x) {is_atm_vect(x, "nps")}

#' @rdname mmm_vec
#' @export
npw_vec <- function(x) {is_atm_vect(x, "npw")}

#' @rdname mmm_vec
#' @export
nst_vec <- function(x) {is_atm_vect(x, "nst")}

#' @rdname mmm_vec
#' @export
num_vec <- function(x) {is_atm_vect(x, "num")}

#' @rdname mmm_vec
#' @export
odd_vec <- function(x) {is_atm_vect(x, "odd")}

#' @rdname mmm_vec
#' @export
ord_vec <- function(x) {is_atm_vect(x, "ord")}

#' @rdname mmm_vec
#' @export
pct_vec <- function(x) {is_atm_vect(x, "pct")}

#' @rdname mmm_vec
#' @export
pos_vec <- function(x) {is_atm_vect(x, "pos")}

#' @rdname mmm_vec
#' @export
ppn_vec <- function(x) {is_atm_vect(x, "ppn")}

#' @rdname mmm_vec
#' @export
psw_vec <- function(x) {is_atm_vect(x, "psw")}

#' @rdname mmm_vec
#' @export
srt_vec <- function(x) {is_atm_vect(x, "srt")}

#' @rdname mmm_vec
#' @export
str_vec <- function(x) {is_atm_vect(x, "str")}

#' @rdname mmm_vec
#' @export
uno_vec <- function(x) {is_atm_vect(x, "uno")}

#' @rdname mmm_vec
#' @export
whl_vec <- function(x) {is_atm_vect(x, "whl")}
