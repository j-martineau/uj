#' @encoding UTF-8
#' @family conversions
#' @family plots
#' @title Convert plotting units
#' @description Functions in this family convert among common plotting units.
#' @details Unit conversion functions take the following forms.
#' \tabular{ll}{  **Function**   \tab **Type of**                                              \cr
#'                **Form**       \tab **Conversion**                                           \cr
#'                `{aa}2{bb}`    \tab from `'{aa}'` units to `'{bb}'` units\eqn{^{(1)}}.       \cr
#'                `{aa}2u`       \tab from `'{aa}'` units to `New` units\eqn{^{(1,2)}}.        \cr
#'                `u2{bb}`       \tab from `Old` units to `'{bb}'` units\eqn{^{(1,2)}}.        \cr
#'                `u2u`          \tab from `Old` units to `New` units.\eqn{^{(2)}}               }
#'  \tabular{l}{  \eqn{^{(1)}} `{aa}` and `{bb}` are placeholders for any given plotting unit. \cr
#'                \eqn{^{(2)}} `Old` and `New` are user supplied arguments.                      }
#' \cr\cr Available unit codes are:
#' \tabular{ll}{  `'cm'`   \tab centimeters    \cr
#'                `'in'`   \tab inches         \cr
#'                `'mm'`   \tab millimeters    \cr
#'                `'pt'`   \tab points (72/inch) }
#' @param X An \link[=atm_num]{atomic, numeric object}.
#' @param Old,New \link[=cmp_chr_scl]{Complete character scalars} giving Old and New units of distance, respectively: `'cm'` for centimeters, `'in'` for inches, `'mm'` for millimeters, and `'pt'` for points.
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
u2u <- function(X, Old, New) {
  Units <- base::c("cm", "in", "mm", "pt")
  Conversions <- base::c(cm2cm = 1        , cm2in = 0.393701 , cm2mm = 10      , cm2pt = 28.3465,
                         in2cm = 2.54     , in2in = 1        , in2mm = 25.4    , in2pt = 72     ,
                         mm2cm = 0.1      , mm2in = 0.0393701, mm2mm = 1       , mm2pt = 2.83465,
                         pt2cm = 0.0352778, pt2in = 0.0138889, pt2mm = 0.352778, pt2pt = 1      )
  OkX <- uj:::.cmp_num(X)
  OkOld <- uj:::.cmp_chr_scl(Old, Valid = Units)
  OkNew <- uj:::.cmp_chr_scl(New, Valid = Units)
  Errors <- NULL
  if (!OkX) {Errors <- base::c(Errors, "[X] must be a complete numeric object (?cmp_num).")}
  if (!OkOld) {Errors <- base::c(Errors, "[Old] must be a character scalar in c('cm', 'in', 'mm', 'pt').")}
  if (!OkNew) {Errors <- base::c(Errors, "[New] must be a character scalar in c('cm', 'in', 'mm', 'pt').")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  X * Conversions[[base::paste0(Old, "2", New)]]
}

#' @rdname u2u
#' @export
u2cm <- function(X, Old) {uj::u2u(X, Old, "cm")}

#' @rdname u2u
#' @export
u2in <- function(X, Old) {uj::u2u(X, Old, "in")}

#' @rdname u2u
#' @export
u2mm <- function(X, Old) {uj::u2u(X, Old, "mm")}

#' @rdname u2u
#' @export
u2pt <- function(X, Old) {uj::u2u(X, Old, "pt")}

#' @rdname u2u
#' @export
cm2u <- function(X, New) {uj::u2u(X, "cm", New)}

#' @rdname u2u
#' @export
in2u <- function(X, New) {uj::u2u(X, "in", New)}

#' @rdname u2u
#' @export
mm2u <- function(X, New) {uj::u2u(X, "mm", New)}

#' @rdname u2u
#' @export
pt2u <- function(X, New) {uj::u2u(X, "pt", New)}

#' @rdname u2u
#' @export
cm2in <- function(X) {uj::u2u(X, "cm", "in")}

#' @rdname u2u
#' @export
cm2mm <- function(X) {uj::u2u(X, "cm", "mm")}

#' @rdname u2u
#' @export
cm2pt <- function(X) {uj::u2u(X, "cm", "pt")}

#' @rdname u2u
#' @export
in2cm <- function(X) {uj::u2u(X, "in", "cm")}

#' @rdname u2u
#' @export
in2mm <- function(X) {uj::u2u(X, "in", "mm")}

#' @rdname u2u
#' @export
in2pt <- function(X) {uj::u2u(X, "in", "pt")}

#' @rdname u2u
#' @export
mm2cm <- function(X) {uj::u2u(X, "mm", "cm")}

#' @rdname u2u
#' @export
mm2in <- function(X) {uj::u2u(X, "mm", "in")}

#' @rdname u2u
#' @export
mm2pt <- function(X) {uj::u2u(X, "mm", "pt")}

#' @rdname u2u
#' @export
pt2cm <- function(X) {uj::u2u(X, "pt", "cm")}

#' @rdname u2u
#' @export
pt2in <- function(X) {uj::u2u(X, "pt", "in")}

#' @rdname u2u
#' @export
pt2mm <- function(X) {uj::u2u(X, "pt", "mm")}
