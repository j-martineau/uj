#' @name u2u.
#' @title Convert among plotting units
#' @param x \link[atm_num]{Atomic, numeric object}.
#' @param old,new \link[cmp_chr_scl]{Complete character scalars} giving old and
#'   new units of length. Valid values are \code{c('cm', 'in', 'mm', 'pt')} for
#'   centimeters, inches, millimeters, and points, respectively.
#' @return An atomic, numeric object.
#' @export
u2u. <- function() {help("u2u.", package = "uj")}

#' @describeIn u2u. Convert units specifying both original and new units.
#' @export
u2u <- function(x, old, new) {
  bank_funs(cmp_num, x = x)
  bank_vals(old = old, new = new, "cm", "in", "mm", "pt")
  err_check()
  x * c(cc = 1, ci = 0.393701, cm = 10, cp = 28.3465, ic = 2.54, ii = 1, im = 25.4, ip = 72, mc = 0.1, mi = 0.0393701, mm = 1, mp = 2.83465, pc = 0.0352778, pi = 0.0138889, pm = 0.352778, pp = 1)[[da0(mid(old, 1, 1), mid(new, 1, 1))]]
}

#' @describeIn u2u. Convert to centimeter units.
#' @export
u2cm <- function(x, old) {u2u(x, old, "cm")}

#' @describeIn u2u. Convert to inch units.
#' @export
u2in <- function(x, old) {u2u(x, old, "in")}

#' @describeIn u2u. Convert to millimeter units.
#' @export
u2mm <- function(x, old) {u2u(x, old, "mm")}

#' @describeIn u2u. Convert to point units.
#' @export
u2pt <- function(x, old) {u2u(x, old, "pt")}

#' @describeIn u2u. Convert centimeters to some other units.
#' @export
cm2u <- function(x, new) {u2u(x, "cm", new)}

#' @describeIn u2u. Convert inches to some other units.
#' @export
in2u <- function(x, new) {u2u(x, "in", new)}

#' @describeIn u2u. Convert millimeters to some other units.
#' @export
mm2u <- function(x, new) {u2u(x, "mm", new)}

#' @describeIn u2u. Convert points to some other units.
#' @export
pt2u <- function(x, new) {u2u(x, "pt", new)}

#' @describeIn u2u. Convert centimeters to inches.
#' @export
cm2in <- function(x) {u2u(x, "cm", "in")}

#' @describeIn u2u. Convert centimeters to millimeters.
#' @export
cm2mm <- function(x) {u2u(x, "cm", "mm")}

#' @describeIn u2u. Convert centimeters to points.
#' @export
cm2pt <- function(x) {u2u(x, "cm", "pt")}

#' @describeIn u2u. Convert inches to centimeters
#' @export
in2cm <- function(x) {u2u(x, "in", "cm")}

#' @describeIn u2u. Convert inches to millimeters.
#' @export
in2mm <- function(x) {u2u(x, "in", "mm")}

#' @describeIn u2u. Convert inches to points.
#' @export
in2pt <- function(x) {u2u(x, "in", "pt")}

#' @describeIn u2u. Convert millimeters to centimeters.
#' @export
mm2cm <- function(x) {u2u(x, "mm", "cm")}

#' @describeIn u2u. Convert millimeters to inches.
#' @export
mm2in <- function(x) {u2u(x, "mm", "in")}

#' @describeIn u2u. Convert millimeters to points.
#' @export
mm2pt <- function(x) {u2u(x, "mm", "pt")}

#' @describeIn u2u. Convert points to centimeters.
#' @export
pt2cm <- function(x) {u2u(x, "pt", "cm")}

#' @describeIn u2u. Convert points to inches.
#' @export
pt2in <- function(x) {u2u(x, "pt", "in")}

#' @describeIn u2u. Convert points to millimeters.
#' @export
pt2mm <- function(x) {u2u(x, "pt", "mm")}
