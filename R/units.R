#' @name u2u
#' @title Convert Plotting Units
#' @description Available units are:\tabular{ll}{
#'     CODE   \tab NAME                                                      \cr
#'     `cm`   \tab Centimeters                                               \cr
#'     `in`   \tab Inches                                                    \cr
#'     `mm`   \tab Millimeters                                               \cr
#'     `pt`   \tab Points (A)                                                  }
#'   (A) Points are `1/72` of an inch.
#'   \cr\cr
#'   Unit functions take the following forms, where `aa` and `bb` are wildcard
#'   units and `old` and `new` are user-supplied argument values:\tabular{ll}{
#'     FUNCTION   \tab CONVERTS UNITS                                        \cr
#'     `aa2bb`    \tab From `aa` to `bb`.                                    \cr
#'     `aa2u`     \tab From `aa` to `new`.                                   \cr
#'     `u2bb`     \tab From `old` to `bb`.                                   \cr
#'     `u2u`      \tab From `old` to `new`.                                    }
#' @param x \link[=atm_num]{Atomic, numeric object}.
#' @param old,new \link[=cmp_chr_scl]{Complete character scalars} giving old and
#'   new units of distance, respectively: `c('cm', 'in', 'mm', 'pt')` for
#'   centimeters, inches, millimeters, and points, respectively.
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

#' @rdname u2u
#' @export
u2cm <- function(x, old) {u2u(x, old, "cm")}

#' @rdname u2u
#' @export
u2in <- function(x, old) {u2u(x, old, "in")}

#' @rdname u2u
#' @export
u2mm <- function(x, old) {u2u(x, old, "mm")}

#' @rdname u2u
#' @export
u2pt <- function(x, old) {u2u(x, old, "pt")}

#' @rdname u2u
#' @export
cm2u <- function(x, new) {u2u(x, "cm", new)}

#' @rdname u2u
#' @export
in2u <- function(x, new) {u2u(x, "in", new)}

#' @rdname u2u
#' @export
mm2u <- function(x, new) {u2u(x, "mm", new)}

#' @rdname u2u
#' @export
pt2u <- function(x, new) {u2u(x, "pt", new)}

#' @rdname u2u
#' @export
cm2in <- function(x) {u2u(x, "cm", "in")}

#' @rdname u2u
#' @export
cm2mm <- function(x) {u2u(x, "cm", "mm")}

#' @rdname u2u
#' @export
cm2pt <- function(x) {u2u(x, "cm", "pt")}

#' @rdname u2u
#' @export
in2cm <- function(x) {u2u(x, "in", "cm")}

#' @rdname u2u
#' @export
in2mm <- function(x) {u2u(x, "in", "mm")}

#' @rdname u2u
#' @export
in2pt <- function(x) {u2u(x, "in", "pt")}

#' @rdname u2u
#' @export
mm2cm <- function(x) {u2u(x, "mm", "cm")}

#' @rdname u2u
#' @export
mm2in <- function(x) {u2u(x, "mm", "in")}

#' @rdname u2u
#' @export
mm2pt <- function(x) {u2u(x, "mm", "pt")}

#' @rdname u2u
#' @export
pt2cm <- function(x) {u2u(x, "pt", "cm")}

#' @rdname u2u
#' @export
pt2in <- function(x) {u2u(x, "pt", "in")}

#' @rdname u2u
#' @export
pt2mm <- function(x) {u2u(x, "pt", "mm")}
