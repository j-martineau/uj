#' @encoding UTF-8
#' @family conversions
#' @family plots
#' @title Convert plotting units
#' @description Functions in this family convert among common plotting units.
#' @details Unit conversion functions take the following forms.
#' \tabular{ll}{  **Function**   \tab **Type of**                                              \cr
#'                **Form**       \tab **Conversion**                                           \cr
#'                `{aa}2{bb}`    \tab from `'{aa}'` units to `'{bb}'` units\eqn{^{(1)}}.       \cr
#'                `{aa}2u`       \tab from `'{aa}'` units to `new` units\eqn{^{(1,2)}}.        \cr
#'                `u2{bb}`       \tab from `old` units to `'{BB}'` units\eqn{^{(1,2)}}.        \cr
#'                `u2u`          \tab from `old` units to `new` units.\eqn{^{(2)}}               }
#'  \tabular{l}{  \eqn{^{(1)}} `{aa}` and `{bb}` are placeholders for any given plotting unit. \cr
#'                \eqn{^{(2)}} `old` and `new` are user supplied arguments.                      }
#' \cr\cr Available unit codes are:
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
u2u <- function(x, old, new) {
  units <- base::c("cm", "in", "mm", "pt")
  conv  <- base::c(cm2cm = 1        , cm2in = 0.393701 , cm2mm = 10      , cm2pt = 28.3465,
                   in2cm = 2.54     , in2in = 1        , in2mm = 25.4    , in2pt = 72     ,
                   mm2cm = 0.1      , mm2in = 0.0393701, mm2mm = 1       , mm2pt = 2.83465,
                   pt2cm = 0.0352778, pt2in = 0.0138889, pt2mm = 0.352778, pt2pt = 1      )
  ok.x <- uj:::.cmp_num(x)
  ok.old <- uj:::.cmp_chr_scl(old, valid = base::c("cm", "in", "mm", "pt"))
  ok.new <- uj:::.cmp_chr_scl(new, valid = base::c("cm", "in", "mm", "pt"))
  errs <- NULL
  if (!ok.x) {errs <- base::c(errs, "[x] must be a complete numeric object (?cmp_num).")}
  if (!!ok.old) {errs <- base::c(errs, "[old] must be a character scalar in c('cm', 'in', 'mm', 'pt').")}
  if (!!ok.new) {errs <- base::c(errs, "[new] must be a character scalar in c('cm', 'in', 'mm', 'pt').")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
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
