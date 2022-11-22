#' @name u2u
#' @title Convert units from among plotting units
#' @param x \link[=atm_num]{Atomic, numeric object}.
#' @param old,new \link[=cmp_chr_scl]{Complete character scalars} giving old and
#'   new units of length, respectively. Valid values are \code{c('cm', 'in',
#'   'mm', 'pt')} for centimeters, inches, millimeters, and points,
#'   respectively.
#' @return An atomic, numeric object.
#' @export
u2u <- function(x, old, new) {
  units <- c("cm", "in", "mm", "pt")
  conv  <- c(cm2cm = 1        , cm2in = 0.393701 , cm2mm = 10      , cm2pt = 28.3465,
             in2cm = 2.54     , in2in = 1        , in2mm = 25.4    , in2pt = 72     ,
             mm2cm = 0.1      , mm2in = 0.0393701, mm2mm = 1       , mm2pt = 2.83465,
             pt2cm = 0.0352778, pt2in = 0.0138889, pt2mm = 0.352778, pt2pt = 1      )
  errs <- c(f0(cmp_num(x)                          , NULL, " \u2022 [x] must be a complete numeric object (?cmp_num)."),
            f0(cmp_chr_scl(old) & !isIN(old, units), NULL, " \u2022 [old] must be a character scalar in c('cm', 'in', 'mm', 'pt')."),
            f0(cmp_chr_scl(new) & !isIN(new, units), NULL, " \u2022 [new] must be a character scalar in c('cm', 'in', 'mm', 'pt')."))
  if (!is.null(errs)) {stop(errs)}
  x * conv[[paste0(old, "2", new)]]
}

#' @describeIn u2u Convert from \code{old} units to centimeters.
#' @export
u2cm <- function(x, old) {u2u(x, old, "cm")}

#' @describeIn u2u Convert from \code{old} units to inches.
#' @export
u2in <- function(x, old) {u2u(x, old, "in")}

#' @describeIn u2u Convert from \code{old} units to millimeters.
#' @export
u2mm <- function(x, old) {u2u(x, old, "mm")}

#' @describeIn u2u Convert from \code{old} units to points.
#' @export
u2pt <- function(x, old) {u2u(x, old, "pt")}

#' @describeIn u2u Convert from centimeters to \code{new} units.
#' @export
cm2u <- function(x, new) {u2u(x, "cm", new)}

#' @describeIn u2u Convert from inches to \code{new} units.
#' @export
in2u <- function(x, new) {u2u(x, "in", new)}

#' @describeIn u2u Convert from millimeters to \code{new} units.
#' @export
mm2u <- function(x, new) {u2u(x, "mm", new)}

#' @describeIn u2u Convert from points to \code{new} units.
#' @export
pt2u <- function(x, new) {u2u(x, "pt", new)}

#' @describeIn u2u Convert from centimeters to inches.
#' @export
cm2in <- function(x) {u2u(x, "cm", "in")}

#' @describeIn u2u Convert from centimeters to millimeters.
#' @export
cm2mm <- function(x) {u2u(x, "cm", "mm")}

#' @describeIn u2u Convert from centimeters to points.
#' @export
cm2pt <- function(x) {u2u(x, "cm", "pt")}

#' @describeIn u2u Convert from inches to centimeters
#' @export
in2cm <- function(x) {u2u(x, "in", "cm")}

#' @describeIn u2u Convert from inches to millimeters.
#' @export
in2mm <- function(x) {u2u(x, "in", "mm")}

#' @describeIn u2u Convert from inches to points.
#' @export
in2pt <- function(x) {u2u(x, "in", "pt")}

#' @describeIn u2u Convert from millimeters to centimeters.
#' @export
mm2cm <- function(x) {u2u(x, "mm", "cm")}

#' @describeIn u2u Convert from millimeters to inches.
#' @export
mm2in <- function(x) {u2u(x, "mm", "in")}

#' @describeIn u2u Convert from millimeters to points.
#' @export
mm2pt <- function(x) {u2u(x, "mm", "pt")}

#' @describeIn u2u Convert from points to centimeters.
#' @export
pt2cm <- function(x) {u2u(x, "pt", "cm")}

#' @describeIn u2u Convert from points to inches.
#' @export
pt2in <- function(x) {u2u(x, "pt", "in")}

#' @describeIn u2u Convert from points to millimeters.
#' @export
pt2mm <- function(x) {u2u(x, "pt", "mm")}
