#' @name mmm_vec
#' @family props
#' @title Extended Mode + Atomic vec (mmm + vec)
#' @details See \code{\link{mmm}}, and \code{\link{atm_vec}}.
#' @description Get all possible extended mode + atomic vec properties.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_vec_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_vec")
  names(x) <- rep.int("mmm_vec", length(x))
  x
}

#' @describeIn mmm_vec Is \code{x} a character atomic vec?
#' @export
chr_vec <- function(x) {atm_vec(x, "chr")}

#' @describeIn mmm_vec Is \code{x} a onechar atomic vec?
#' @export
clr_vec <- function(x) {atm_vec(x, "clr")}

#' @describeIn mmm_vec Is \code{x} an even-number atomic vec?
#' @export
evn_vec <- function(x) {atm_vec(x, "evn")}

#' @describeIn mmm_vec Is \code{x} a factor atomic vec?
#' @export
fac_vec <- function(x) {atm_vec(x, "fac")}

#' @describeIn mmm_vec Is \code{x} a fractional-number atomic vec?
#' @export
frc_vec <- function(x) {atm_vec(x, "frc")}

#' @describeIn mmm_vec Is \code{x} an indexer atomic vec?
#' @export
ind_vec <- function(x) {atm_vec(x, "ind")}

#' @describeIn mmm_vec Is \code{x} a logical atomic vec?
#' @export
lgl_vec <- function(x) {atm_vec(x, "lgl")}

#' @describeIn mmm_vec Is \code{x} a negative-number atomic vec?
#' @export
neg_vec <- function(x) {atm_vec(x, "neg")}

#' @describeIn mmm_vec Is \code{x} a negative-whole-number atomic vec?
#' @export
ngw_vec <- function(x) {atm_vec(x, "ngw")}

#' @describeIn mmm_vec Is \code{x} a non-negative-number atomic vec?
#' @export
nng_vec <- function(x) {atm_vec(x, "nng")}

#' @describeIn mmm_vec Is \code{x} a non-negative-whole-number atomic vec?
#' @export
nnw_vec <- function(x) {atm_vec(x, "nnw")}

#' @describeIn mmm_vec Is \code{x} a non-positive-number atomic vec?
#' @export
nps_vec <- function(x) {atm_vec(x, "nps")}

#' @describeIn mmm_vec Is \code{x} a non-positive-whole-number atomic vec?
#' @export
npw_vec <- function(x) {atm_vec(x, "npw")}

#' @describeIn mmm_vec Is \code{x} a non-sortable atomic vec?
#' @export
nst_vec <- function(x) {atm_vec(x, "nst")}

#' @describeIn mmm_vec Is \code{x} a number atomic vec?
#' @export
num_vec <- function(x) {atm_vec(x, "num")}

#' @describeIn mmm_vec Is \code{x} an odd-number atomic vec?
#' @export
odd_vec <- function(x) {atm_vec(x, "odd")}

#' @describeIn mmm_vec Is \code{x} an ordered-factor atomic vec?
#' @export
ord_vec <- function(x) {atm_vec(x, "ord")}

#' @describeIn mmm_vec Is \code{x} a percent-number atomic vec?
#' @export
pct_vec <- function(x) {atm_vec(x, "pct")}

#' @describeIn mmm_vec Is \code{x} a positive-number atomic vec?
#' @export
pos_vec <- function(x) {atm_vec(x, "pos")}

#' @describeIn mmm_vec Is \code{x} a atomic proportion-number vec?
#' @export
ppn_vec <- function(x) {atm_vec(x, "ppn")}

#' @describeIn mmm_vec Is \code{x} a positive-whole-number atomic vec?
#' @export
psw_vec <- function(x) {atm_vec(x, "psw")}

#' @describeIn mmm_vec Is \code{x} a sortable atomic vec?
#' @export
srt_vec <- function(x) {atm_vec(x, "srt")}

#' @describeIn mmm_vec Is \code{x} a string atomic vec?
#' @export
str_vec <- function(x) {atm_vec(x, "str")}

#' @describeIn mmm_vec Is \code{x} a unordered-factor atomic vec?
#' @export
uno_vec <- function(x) {atm_vec(x, "uno")}

#' @describeIn mmm_vec Is \code{x} a whole-number atomic vec?
#' @export
whl_vec <- function(x) {atm_vec(x, "whl")}
