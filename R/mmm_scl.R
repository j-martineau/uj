#' @name mmm_scl
#' @family props
#' @title Extended Mode + Atomic scalar (mmm + scl)
#' @description Get all possible extended mode + atomic scalar properties.
#' @details See \code{\link{mmm}}, and \code{\link{atm_scl}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_scl_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_scl")
  names(x) <- rep.int("mmm_scl", length(x))
  x
}

#' @describeIn mmm_scl Is \code{x} a character scalar?
#' @export
chr_scl <- function(x) {atm_scl(x, "chr")}

#' @describeIn mmm_scl Is \code{x} a onechar scalar?
#' @export
ch1_scl <- function(x) {atm_scl(x, "ch1")}

#' @describeIn mmm_scl Is \code{x} a color scalar?
#' @export
clr_scl <- function(x) {atm_scl(x, "clr")}

#' @describeIn mmm_scl Is \code{x} an even-numeric scalar?
#' @export
evn_scl <- function(x) {atm_scl(x, "evn")}

#' @describeIn mmm_scl Is \code{x} a factor scalar?
#' @export
fac_scl <- function(x) {atm_scl(x, "fac")}

#' @describeIn mmm_scl Is \code{x} a fractional-numeric scalar?
#' @export
frc_scl <- function(x) {atm_scl(x, "frc")}

#' @describeIn mmm_scl Is \code{x} an indexer scalar?
#' @export
ind_scl <- function(x) {atm_scl(x, "ind")}

#' @describeIn mmm_scl Is \code{x} a logical scalar?
#' @export
lgl_scl <- function(x) {atm_scl(x, "lgl")}

#' @describeIn mmm_scl Is \code{x} a negative-numeric scalar?
#' @export
neg_scl <- function(x) {atm_scl(x, "neg")}

#' @describeIn mmm_scl Is \code{x} a negative-whole-numeric scalar?
#' @export
ngw_scl <- function(x) {atm_scl(x, "ngw")}

#' @describeIn mmm_scl Is \code{x} a non-negative-numeric scalar?
#' @export
nng_scl <- function(x) {atm_scl(x, "nng")}

#' @describeIn mmm_scl Is \code{x} an non-negative-whole-numeric scalar?
#' @export
nnw_scl <- function(x) {atm_scl(x, "nnw")}

#' @describeIn mmm_scl Is \code{x} a non-positive-numeric scalar?
#' @export
nps_scl <- function(x) {atm_scl(x, "nps")}

#' @describeIn mmm_scl Is \code{x} a non-positive-whole-numeric scalar?
#' @export
npw_scl <- function(x) {atm_scl(x, "npw")}

#' @describeIn mmm_scl Is \code{x} a non-sortable scalar?
#' @export
nst_scl <- function(x) {atm_scl(x, "nst")}

#' @describeIn mmm_scl Is \code{x} a numeric scalar?
#' @export
num_scl <- function(x) {atm_scl(x, "num")}

#' @describeIn mmm_scl Is \code{x} an odd-numeric scalar?
#' @export
odd_scl <- function(x) {atm_scl(x, "odd")}

#' @describeIn mmm_scl Is \code{x} an ordered-factor scalar?
#' @export
ord_scl <- function(x) {atm_scl(x, "ord")}

#' @describeIn mmm_scl Is \code{x} a percent-numeric scalar?
#' @export
pct_scl <- function(x) {atm_scl(x, "pct")}

#' @describeIn mmm_scl Is \code{x} a positive-numeric scalar?
#' @export
pos_scl <- function(x) {atm_scl(x, "pos")}

#' @describeIn mmm_scl Is \code{x} a proportion-numeric scalar?
#' @export
ppn_scl <- function(x) {atm_scl(x, "ppn")}

#' @describeIn mmm_scl Is \code{x} a positive-whole-numeric scalar?
#' @export
psw_scl <- function(x) {atm_scl(x, "psw")}

#' @describeIn mmm_scl Is \code{x} a sortable scalar?
#' @export
srt_scl <- function(x) {atm_scl(x, "srt")}

#' @describeIn mmm_scl Is \code{x} a string scalar?
#' @export
chr_str <- function(x) {atm_scl(x, "str")}

#' @describeIn mmm_scl Is \code{x} an unordered-factor scalar?
#' @export
uno_scl <- function(x) {atm_scl(x, "uno")}

#' @describeIn mmm_scl Is \code{x} a whole-numeric scalar?
#' @export
whl_scl <- function(x) {atm_scl(x, "whl")}
