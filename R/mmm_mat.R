#' @name mmm_mat
#' @family props
#' @title Extended Mode + Atomic matrix (mmm + mat)
#' @description Get all possible extended mode + atomic matrix properties.
#' @details See \code{\link{mmm}}, and \code{\link{atm_mat}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_mat_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_mat")
  names(x) <- rep.int("mmm_mat", length(x))
  x
}

#' @describeIn mmm_mat Is \code{x} a character matrix?
#' @export
chr_mat <- function(x) {atm_mat(x, "chr")}

#' @describeIn mmm_mat Is \code{x} a onechar matrix?
#' @export
ch1_mat <- function(x) {atm_mat(x, "ch1")}

#' @describeIn mmm_mat Is \code{x} a color matrix?
#' @export
clr_mat <- function(x) {atm_mat(x, "clr")}

#' @describeIn mmm_mat Is \code{x} an even-numeric matrix?
#' @export
evn_mat <- function(x) {atm_mat(x, "evn")}

#' @describeIn mmm_mat Is \code{x} a factor matrix?
#' @export
fac_mat <- function(x) {atm_mat(x, "fac")}

#' @describeIn mmm_mat Is \code{x} a fractional-numeric matrix?
#' @export
frc_mat <- function(x) {atm_mat(x, "frc")}

#' @describeIn mmm_mat Is \code{x} an indexer matrix?
#' @export
ind_mat <- function(x) {atm_mat(x, "ind")}

#' @describeIn mmm_mat Is \code{x} a logical matrix?
#' @export
lgl_mat <- function(x) {atm_mat(x, "lgl")}

#' @describeIn mmm_mat Is \code{x} a negative-numeric matrix?
#' @export
neg_mat <- function(x) {atm_mat(x, "neg")}

#' @describeIn mmm_mat Is \code{x} a negative-whole-numeric matrix?
#' @export
ngw_mat <- function(x) {atm_mat(x, "ngw")}

#' @describeIn mmm_mat Is \code{x} a non-negative-numeric matrix?
#' @export
nng_mat <- function(x) {atm_mat(x, "nng")}

#' @describeIn mmm_mat Is \code{x} a non-negative-whole-numeric matrix?
#' @export
nnw_mat <- function(x) {atm_mat(x, "nnw")}

#' @describeIn mmm_mat Is \code{x} a non-positive-numeric matrix?
#' @export
nps_mat <- function(x) {atm_mat(x, "nps")}

#' @describeIn mmm_mat Is \code{x} a non-positive-whole-numeric matrix?
#' @export
npw_mat <- function(x) {atm_mat(x, "npw")}

#' @describeIn mmm_mat Is \code{x} a non-sortable matrix?
#' @export
nst_mat <- function(x) {atm_mat(x, "nst")}

#' @describeIn mmm_mat Is \code{x} a numeric matrix?
#' @export
num_mat <- function(x) {atm_mat(x, "num")}

#' @describeIn mmm_mat Is \code{x} an odd-numeric matrix?
#' @export
odd_mat <- function(x) {atm_mat(x, "odd")}

#' @describeIn mmm_mat Is \code{x} an ordered-factor matrix?
#' @export
ord_mat <- function(x) {atm_mat(x, "ord")}

#' @describeIn mmm_mat Is \code{x} a percent-numeric matrix?
#' @export
pct_mat <- function(x) {atm_mat(x, "pct")}

#' @describeIn mmm_mat Is \code{x} a positive-numeric matrix?
#' @export
pos_mat <- function(x) {atm_mat(x, "pos")}

#' @describeIn mmm_mat Is \code{x} a proportion-numeric matrix?
#' @export
ppn_mat <- function(x) {atm_mat(x, "ppn")}

#' @describeIn mmm_mat Is \code{x} a positive-whole-numeric matrix?
#' @export
psw_mat <- function(x) {atm_mat(x, "psw")}

#' @describeIn mmm_mat Is \code{x} a sortable matrix?
#' @export
srt_mat <- function(x) {atm_mat(x, "srt")}

#' @describeIn mmm_mat Is \code{x} a string matrix?
#' @export
str_mat <- function(x) {atm_mat(x, "str")}

#' @describeIn mmm_mat Is \code{x} an unordered-factor matrix?
#' @export
uno_mat <- function(x) {atm_mat(x, "uno")}

#' @describeIn mmm_mat Is \code{x} whole-numeric matrix?
#' @export
whl_mat <- function(x) {atm_mat(x, "whl")}
