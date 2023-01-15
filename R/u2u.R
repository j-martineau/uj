#' @encoding UTF-8
#' @family conversions
#' @family plots
#' @title Convert plotting units
#' @description Unit functions take the following forms, where `AA` and `BB` are wildcard unit codes and `old` and `new` are user-supplied argument values.
#' \tabular{rl}{
#'      `BB2BB`   \tab Converts units *from* `'AA'` *to* `'BB'`.
#'   \cr `AA2u`   \tab Converts units *from* `'AA'` *to* `new`.
#'   \cr `u2BB`   \tab Converts units *from* `old` *to* `'BB'`.
#'   \cr  `u2u`   \tab Converts units *from* `old` *to* `new`.
#' }
#' Available units/unit codes are: \tabular{rl}{
#'       `'cm'`   \tab centimeters.
#'   \cr `'in'`   \tab inches.
#'   \cr `'mm'`   \tab millimeters.
#'   \cr `'pt'`   \tab points (72/inch).
#' }
#' @param x \link[=atm_num]{Atomic, numeric object}.
#' @param old,new \link[=cmp_chr_scl]{Complete character scalars} giving old and new units of distance, respectively: `'cm'`, `'in'`, `'mm'`, `'pt'` for centimeters, inches, millimeters, and points, respectively.
#' @return An atomic numeric object.
#' @examples
#' u2u(1:3, "cm", "pt")
#' u2u(1:3, "in", "mm")
#' u2cm(1:3, "in")
#' in2u(1:3, "mm")
#' in2cm(1:3)
#' pt2mm(1:3)
#' @export
u2u <- function(x, old, new) {
  units <- base::c("cm", "in", "mm", "pt")
  conv  <- base::c(cm2cm = 1        , cm2in = 0.393701 , cm2mm = 10      , cm2pt = 28.3465,
                   in2cm = 2.54     , in2in = 1        , in2mm = 25.4    , in2pt = 72     ,
                   mm2cm = 0.1      , mm2in = 0.0393701, mm2mm = 1       , mm2pt = 2.83465,
                   pt2cm = 0.0352778, pt2in = 0.0138889, pt2mm = 0.352778, pt2pt = 1      )
  errs <- base::c(uj::f0(uj::cmp_num(x)                             , NULL, "[x] must be a complete numeric object (?cmp_num)."),
                  uj::f0(uj::cmp_chr_scl(old) & uj::isIN(old, units), NULL, "[old] must be a character scalar in c('cm', 'in', 'mm', 'pt')."),
                  uj::f0(uj::cmp_chr_scl(new) & uj::isIN(new, units), NULL, "[new] must be a character scalar in c('cm', 'in', 'mm', 'pt')."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  x * conv[[base::paste0(old, "2", new)]]
}

#' @rdname u2u
#' @export
u2cm <- function(x, old) {uj::u2u(x, old, "cm")}

#' @rdname u2u
#' @export
u2in <- function(x, old) {uj::u2u(x, old, "in")}

#' @rdname u2u
#' @export
u2mm <- function(x, old) {uj::u2u(x, old, "mm")}

#' @rdname u2u
#' @export
u2pt <- function(x, old) {uj::u2u(x, old, "pt")}

#' @rdname u2u
#' @export
cm2u <- function(x, new) {uj::u2u(x, "cm", new)}

#' @rdname u2u
#' @export
in2u <- function(x, new) {uj::u2u(x, "in", new)}

#' @rdname u2u
#' @export
mm2u <- function(x, new) {uj::u2u(x, "mm", new)}

#' @rdname u2u
#' @export
pt2u <- function(x, new) {uj::u2u(x, "pt", new)}

#' @rdname u2u
#' @export
cm2in <- function(x) {uj::u2u(x, "cm", "in")}

#' @rdname u2u
#' @export
cm2mm <- function(x) {uj::u2u(x, "cm", "mm")}

#' @rdname u2u
#' @export
cm2pt <- function(x) {uj::u2u(x, "cm", "pt")}

#' @rdname u2u
#' @export
in2cm <- function(x) {uj::u2u(x, "in", "cm")}

#' @rdname u2u
#' @export
in2mm <- function(x) {uj::u2u(x, "in", "mm")}

#' @rdname u2u
#' @export
in2pt <- function(x) {uj::u2u(x, "in", "pt")}

#' @rdname u2u
#' @export
mm2cm <- function(x) {uj::u2u(x, "mm", "cm")}

#' @rdname u2u
#' @export
mm2in <- function(x) {uj::u2u(x, "mm", "in")}

#' @rdname u2u
#' @export
mm2pt <- function(x) {uj::u2u(x, "mm", "pt")}

#' @rdname u2u
#' @export
pt2cm <- function(x) {uj::u2u(x, "pt", "cm")}

#' @rdname u2u
#' @export
pt2in <- function(x) {uj::u2u(x, "pt", "in")}

#' @rdname u2u
#' @export
pt2mm <- function(x) {uj::u2u(x, "pt", "mm")}
