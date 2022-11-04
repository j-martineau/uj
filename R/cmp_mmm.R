#' @name cmp_mmm_uj
#' @family props
#' @title Complete + Extended Mode
#' @description Check with an object is complete (\code{\link{icmp}}) and of a
#'   specific extended mode \code{\link{mmm}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE} except for \code{cmp_mmm_vals}, which
#'   returns a character vector.
#' @export
cmp_mmm_uj <- function() {help("cmp_mmm_uj", package = "uj")}

#' @describeIn cmp_mmm_uj Get a character vector of all possible complete +
#'   extended mode values.
#' @export
cmp_mmm_vals <- function() {x <- paste0("", c("cmp", mmm_vals())); names(x) <- rep.int("cmp_mmm", length(x)); x}

#' @describeIn cmp_mmm_uj Is \code{x} a complete atomic object?
#'   (\code{\link{icmp}}, \code{link{iatm}})
#' @export
cmp_atm <- function(x) {is_atm(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete onechar object?
#'   (\code{\link{icmp}}, \code{link{ich1}})
#' @export
cmp_ch1 <- function(x) {icmp(x) & ich1(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete character object?
#'   (\code{\link{icmp}}, \code{link{ichr}})
#' @export
cmp_chr <- function(x) {icmp(x) & ichr(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete color object?
#'   (\code{\link{icmp}}, \code{link{iclr}})
#' @export
cmp_clr <- function(x) {icmp(x) & iclr(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete even-valued whole-number
#'   object? (\code{\link{icmp}}, \code{link{ievn}})
#' @export
cmp_evn <- function(x) {icmp(x) & ievn(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete factor object?
#'   (\code{\link{icmp}}, \code{link{ifac}})
#' @export
cmp_fac <- function(x) {icmp(x) & ifac(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete fractional-valued numeric
#'   object? (\code{\link{icmp}}, \code{link{ifrc}})
#' @export
cmp_frc <- function(x) {icmp(x) & ifrc(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete indexer object?
#'   (\code{\link{icmp}}, \code{link{iind}})
#' @export
cmp_ind <- function(x) {icmp(x) & iind(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete logical object?
#'   (\code{\link{icmp}}, \code{link{ilgl}})
#' @export
cmp_lgl <- function(x) {icmp(x) & ilgl(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete negative-valued numeric object?
#'   (\code{\link{icmp}}, \code{link{ineg}})
#' @export
cmp_neg <- function(x) {icmp(x) & ineg(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete negative-valued whole-number
#'   object? (\code{\link{icmp}}, \code{link{ingw}})
#' @export
cmp_ngw <- function(x) {icmp(x) & ingw(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete non-negative-valued
#'   numeric object? (\code{\link{icmp}}, \code{link{inng}})
#' @export
cmp_nng <- function(x) {icmp(x) & inng(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete non-negative-valued
#'   whole-number object? (\code{\link{icmp}}, \code{link{innw}})
#' @export
cmp_nnw <- function(x) {icmp(x) & innw(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete non-positive-valued
#'   numeric object? (\code{\link{icmp}}, \code{link{inps}})
#' @export
cmp_nps <- function(x) {icmp(x) & inps(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete non-positive-valued
#'   whole-number object? (\code{\link{icmp}}, \code{link{inpw}})
#' @export
cmp_npw <- function(x) {icmp(x) & inpw(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete non-sortable object?
#'   (\code{\link{icmp}}, \code{link{inst}})
#' @export
cmp_nst <- function(x) {icmp(x) & inst(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete numeric object?
#'   (\code{\link{icmp}}, \code{link{inum}})
#' @export
cmp_num <- function(x) {icmp(x) & inum(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete odd-valued whole-number object?
#'   (\code{\link{icmp}}, \code{link{iodd}})
#' @export
cmp_odd <- function(x) {icmp(x) & iodd(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete ordered factor object?
#'   (\code{\link{icmp}}, \code{link{iord}})
#' @export
cmp_ord <- function(x) {icmp(x) & iord(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete percent-valued (0-100) numeric
#'   object? (\code{\link{icmp}}, \code{link{ipct}})
#' @export
cmp_pct <- function(x) {icmp(x) & ipct(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete positive-valued numeric object?
#'   (\code{\link{icmp}}, \code{link{ipos}})
#' @export
cmp_pos <- function(x) {icmp(x) & ipos(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete proportion-valued (0-1) numeric
#'   object? (\code{\link{icmp}}, \code{link{ippn}})
#' @export
cmp_ppn <- function(x) {icmp(x) & ippn(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete positive-valued whole-number
#'   object? (\code{\link{icmp}}, \code{link{ipsw}})
#' @export
cmp_psw <- function(x) {icmp(x) & ipsw(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete sortable object?
#'   (\code{\link{icmp}}, \code{link{isrt}})
#' @export
cmp_srt <- function(x) {icmp(x) & isrt(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete string object?
#'   (\code{\link{icmp}}, \code{link{istr}})
#' @export
cmp_str <- function(x) {icmp(x) & istr(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete unordered factor object?
#'   (\code{\link{icmp}}, \code{link{iuno}})
#' @export
cmp_uno <- function(x) {icmp(x) & iuno(x)}

#' @describeIn cmp_mmm_uj Is \code{x} a complete whole-number object?
#'   (\code{\link{icmp}}, \code{link{iwhl}})
#' @export
cmp_whl <- function(x) {icmp(x) & iwhl(x)}
