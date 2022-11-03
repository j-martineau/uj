#' @name mmm_mvc
#' @family props
#' @title Extended Mode + Atomic mvect (mmm + mvc)
#' @description Get all possible extended mode + atomic mvect properties.
#' @details See \code{\link{mmm}}, and \code{\link{atm_mvc}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_mvc_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_mvc")
  names(x) <- rep.int("mmm_mvc", length(x))
  x
}

#' @describeIn mmm_mvc Is \code{x} a character mvect?
#' @export
chr_mvc <- function(x) {atm_mvc(x, "chr")}

#' @describeIn mmm_mvc Is \code{x} a onechar mvect?
#' @export
ch1_mvc <- function(x) {atm_mvc(x, "ch1")}

#' @describeIn mmm_mvc Is \code{x} a color mvect?
#' @export
clr_mvc <- function(x) {atm_mvc(x, "clr")}

#' @describeIn mmm_mvc Is \code{x} an even-numeric mvect?
#' @export
evn_mvc <- function(x) {atm_mvc(x, "evn")}

#' @describeIn mmm_mvc Is \code{x} a factor mvect?
#' @export
fac_mvc <- function(x) {atm_mvc(x, "fac")}

#' @describeIn mmm_mvc Is \code{x} a fractional-numeric mvect?
#' @export
frc_mvc <- function(x) {atm_mvc(x, "frc")}

#' @describeIn mmm_mvc Is \code{x} an indexer mvect?
#' @export
ind_mvc <- function(x) {atm_mvc(x, "ind")}

#' @describeIn mmm_mvc Is \code{x} a logical mvect?
#' @export
lgl_mvc <- function(x) {atm_mvc(x, "lgl")}

#' @describeIn mmm_mvc Is \code{x} a negative-numeric mvect?
#' @export
neg_mvc <- function(x) {atm_mvc(x, "neg")}

#' @describeIn mmm_mvc Is \code{x} a negative-whole-numeric mvect?
#' @export
ngw_mvc <- function(x) {atm_mvc(x, "ngw")}

#' @describeIn mmm_mvc Is \code{x} a non-negative-numeric mvect?
#' @export
nng_mvc <- function(x) {atm_mvc(x, "nng")}

#' @describeIn mmm_mvc Is \code{x} a non-negative-whole-numeric mvect?
#' @export
nnw_mvc <- function(x) {atm_mvc(x, "nnw")}

#' @describeIn mmm_mvc Is \code{x} a non-positive-numeric mvect?
#' @export
nps_mvc <- function(x) {atm_mvc(x, "nps")}

#' @describeIn mmm_mvc Is \code{x} a non-positive-whole-numeric mvect?
#' @export
npw_mvc <- function(x) {atm_mvc(x, "npw")}

#' @describeIn mmm_mvc Is \code{x} a non-sortable atomic mvect?
#' @export
nst_mvc <- function(x) {atm_mvc(x, "nst")}

#' @describeIn mmm_mvc Is \code{x} a numeric mvect?
#' @export
num_mvc <- function(x) {atm_mvc(x, "num")}

#' @describeIn mmm_mvc Is \code{x} an odd-numeric mvect?
#' @export
odd_mvc <- function(x) {atm_mvc(x, "odd")}

#' @describeIn mmm_mvc Is \code{x} an ordered-factor mvect?
#' @export
ord_mvc <- function(x) {atm_mvc(x, "ord")}

#' @describeIn mmm_mvc Is \code{x} a percent-numeric mvect?
#' @export
pct_mvc <- function(x) {atm_mvc(x, "pct")}

#' @describeIn mmm_mvc Is \code{x} a positive-numeric mvect?
#' @export
pos_mvc <- function(x) {atm_mvc(x, "pos")}

#' @describeIn mmm_mvc Is \code{x} a proportion-numeric mvect?
#' @export
ppn_mvc <- function(x) {atm_mvc(x, "ppn")}

#' @describeIn mmm_mvc Is \code{x} a positive-whole-numeric mvect?
#' @export
psw_mvc <- function(x) {atm_mvc(x, "psw")}

#' @describeIn mmm_mvc Is \code{x} a sortable atomic mvect?
#' @export
srt_mvc <- function(x) {atm_mvc(x, "srt")}

#' @describeIn mmm_mvc Is \code{x} a string mvect?
#' @export
str_mvc <- function(x) {atm_mvc(x, "str")}

#' @describeIn mmm_mvc Is \code{x} an unordered-factor mvect?
#' @export
uno_mvc <- function(x) {atm_mvc(x, "uno")}

#' @describeIn mmm_mvc Is \code{x} a whole-numeric mvect?
#' @export
whl_mvc <- function(x) {atm_mvc(x, "whl")}
