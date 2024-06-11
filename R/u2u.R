#' @encoding UTF-8
#' @family conversions
#' @family plots
#' @title Convert plotting units
#' @description Functions in this family convert among common plotting units. Available unit codes are:
#' \tabular{ll}{  `'cm'`   \tab centimeters    \cr
#'                `'in'`   \tab inches         \cr
#'                `'mm'`   \tab millimeters    \cr
#'                `'pt'`   \tab points (72/inch) }
#' @param x An \link[=atm_num]{atomic, numeric object}.
#' @param old,new \link[=cmp_chr_scl]{Complete character scalars} giving old and new units of distance, respectively: `'cm'` for centimeters, `'in'` for inches, `'mm'` for millimeters, and `'pt'` for points.
#' @return An atomic numeric object.
#' @examples
#' u2u(1:3, "cm", "pt")
#' u2u(1:3, "in", "mm")
#' u2cm(1:3, "in")
#' u2in(1:3, "cm")
#' u2pt(1:3, "cm")
#' cm2u(1:3, "in")
#' in2u(1:3, "cm")
#' pt2u(1:3, "cm")
#' cm2in(1:3)
#' cm2mm(1:3)
#' cm2pt(1:3)
#' in2cm(1:3)
#' in2mm(1:3)
#' in2pt(1:3)
#' mm2cm(1:3)
#' mm2in(1:3)
#' mm2pt(1:3)
#' pt2cm(1:3)
#' pt2in(1:3)
#' pt2mm(1:3)
#' @export
u2u_help <- function() {utils::help("u2u_help", package = "uj")}

#' @describeIn u2u_help Converts `x` from `old` units to `new` units
#' @export
u2u <- function(x, old, new) {
  units <- base::c("cm", "in", "mm", "pt")
  convs <- base::c(cm2cm = 1        , cm2in = 0.393701 , cm2mm = 10      , cm2pt = 28.3465,
                   in2cm = 2.54     , in2in = 1        , in2mm = 25.4    , in2pt = 72     ,
                   mm2cm = 0.1      , mm2in = 0.0393701, mm2mm = 1       , mm2pt = 2.83465,
                   pt2cm = 0.0352778, pt2in = 0.0138889, pt2mm = 0.352778, pt2pt = 1      )
  okX   <- uj::.cmp_num(x)
  okOld <- uj::.cmp_chr_scl(old, valid = units)
  okNew <- uj::.cmp_chr_scl(new, valid = units)
  errs <- NULL
  if (!okX  ) {errs <- base::c(errs, "[x] must be a complete numeric object (?cmp_num).")}
  if (!okOld) {errs <- base::c(errs, "[old] must be a character scalar in c('cm', 'in', 'mm', 'pt').")}
  if (!okNew) {errs <- base::c(errs, "[new] must be a character scalar in c('cm', 'in', 'mm', 'pt').")}
  if (!base::is.null(errs)) {uj::stopperr(errs, .fun = "u2u")}
  x * convs[[base::paste0(old, "2", new)]]
}

#' @describeIn u2u_help Converts `x` from `old` units to centimeters.
#' @export
u2cm <- function(x, old) {uj::u2u(x, old, "cm")}

#' @describeIn u2u_help Converts `x` from `old` units to inches.
#' @export
u2in <- function(x, old) {uj::u2u(x, old, "in")}

#' @describeIn u2u_help Converts `x` from `old` units to millimeters.
#' @export
u2mm <- function(x, old) {uj::u2u(x, old, "mm")}

#' @describeIn u2u_help Converts `x` from `old` units to points.
#' @export
u2pt <- function(x, old) {uj::u2u(x, old, "pt")}

#' @describeIn u2u_help Converts `x` from centimeters to `new` units.
#' @export
cm2u <- function(x, new) {uj::u2u(x, "cm", new)}

#' @describeIn u2u_help Converts `x` from inches to `new` units.
#' @export
in2u <- function(x, new) {uj::u2u(x, "in", new)}

#' @describeIn u2u_help Converts `x` from millimeters to `new` units.
#' @export
mm2u <- function(x, new) {uj::u2u(x, "mm", new)}

#' @describeIn u2u_help Converts `x` from points to `new` units.
#' @export
pt2u <- function(x, new) {uj::u2u(x, "pt", new)}

#' @describeIn u2u_help Converts `x` from centimeters to inches.
#' @export
cm2in <- function(x) {uj::u2u(x, "cm", "in")}

#' @describeIn u2u_help Converts `x` from centimeters to millimeters.
#' @export
cm2mm <- function(x) {uj::u2u(x, "cm", "mm")}

#' @describeIn u2u_help Converts `x` from centimeters to points.
#' @export
cm2pt <- function(x) {uj::u2u(x, "cm", "pt")}

#' @describeIn u2u_help Converts `x` from inches to centimeters.
#' @export
in2cm <- function(x) {uj::u2u(x, "in", "cm")}

#' @describeIn u2u_help Converts `x` from inches to millimeters.
#' @export
in2mm <- function(x) {uj::u2u(x, "in", "mm")}

#' @describeIn u2u_help Converts `x` from inches to points.
#' @export
in2pt <- function(x) {uj::u2u(x, "in", "pt")}

#' @describeIn u2u_help Converts `x` from millimeters to centimeters.
#' @export
mm2cm <- function(x) {uj::u2u(x, "mm", "cm")}

#' @describeIn u2u_help Converts `x` from millimeters to inches.
#' @export
mm2in <- function(x) {uj::u2u(x, "mm", "in")}

#' @describeIn u2u_help Converts `x` from millimeters to points.
#' @export
mm2pt <- function(x) {uj::u2u(x, "mm", "pt")}

#' @describeIn u2u_help Converts `x` from points to centimeters.
#' @export
pt2cm <- function(x) {uj::u2u(x, "pt", "cm")}

#' @describeIn u2u_help Converts `x` from points to inches.
#' @export
pt2in <- function(x) {uj::u2u(x, "pt", "in")}

#' @describeIn u2u_help Converts `x` from points to millimeters.
#' @export
pt2mm <- function(x) {uj::u2u(x, "pt", "mm")}
