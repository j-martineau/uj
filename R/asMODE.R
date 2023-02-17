#' @name asMODE
#' @encoding UTF-8
#' @family properties
#' @title Coerce to base mode or `xmode`
#' @description Functions to coerce objects to \link[base:mode]{base mode} or \code{\link[=mmm]{xmode}}.
#' @details Functions in this family are as follows:
#' \tabular{ll}{  `asCHR`   \tab Thinly wraps \code{\link[base]{as.character}}.                                                       \cr
#'                `asINT`   \tab Thinly wraps \code{\link[base]{as.integer}}.                                                          \cr
#'                `asNUM`   \tab Thinly wraps \code{\link[base]{as.numeric}}.                                                          \cr
#'                `asLGL`   \tab Thinly wraps \code{\link[base]{as.logical}}.                                            \cr   \tab    \cr
#'                `asORD`   \tab As xmode `'ord'` (ordered factor). Wraps `factor(x, levels = levs, ordered = TRUE)`.    \cr   \tab    \cr
#'                `asUNO`   \tab As xmode `'uno'` (unordered factor). Wraps `factor(x, levels = levs, ordered = FALSE)`. \cr   \tab    \cr
#'                `asCLR`   \tab Coerces character color values to hexadecimal.                                          \cr   \tab    \cr
#'                `asFUN`   \tab If `x` is a character scalar function return that function, but if `x` is a function object, return it. }
#' @param x For `asCLR`, an object of mode character; for `asFUN`, a character scalar function name or a function object; for `asORD` and `asUNO`, an atomic object; and for all others, any R object.
#' @param na `TRUE` or `FALSE` indicating whether `NA` values are acceptable.
#' @param levs A \link[=cmp_vec]{complete atomic vec} of factor levels (ordered factor levels for `asORD`).
#' @param ... Further arguments passed to or from other methods.
#' @return **An object of base mode** `'character'`                         \cr `asCHR`
#' \cr\cr  **An object of base mode** `'integer'`                           \cr `asINT`
#' \cr\cr  **An object of base mode** `'logical'`                           \cr `asLGL`
#' \cr\cr  **An object of base mode** `'numeric'`                           \cr `asNUM`
#' \cr\cr  **An object of \link[=mmm]{xmode}** `'ord'` (ordered factor)     \cr `asORD`
#' \cr\cr  **An object of xmode** `'uno'` (unordered factor)                \cr `asUNO`
#' \cr\cr  **An object of xmode** `'clr'` (hex color in form `'#RRGGBBAA'`) \cr `asCLR`
#' @examples
#' bins. <- sample(c(0, 1), 10, replace = T)
#' chrs. <- c("3.14", "2.72", "1.41")
#' clrs. <- c("red", "#AABBCC", "#AABBCCDD", "blue")
#' nums. <- c(pi, exp(1), sqrt(2))
#'
#' asFUN(unique)
#' asFUN("unique")
#'
#' bins.
#' asLGL(bins.)
#'
#' chrs.
#' asNUM(chrs.)
#' asINT(chrs.)
#'
#' clrs.
#' asCLR(clrs.)
#'
#' nums.
#' asCHR(nums.)
#'
#' asUNO(clrs., levs = unique(clrs.))
#' asORD(clrs., levs = sort(unique(clrs.)))
#' @export
asMODE <- function(x, mode, levs = NULL, na = FALSE) {
  if (uj::isIN1(mode, "clr" , "Clr", "CLR")) {
    uj::errs_if_nots(uj::CHR(x)                   , "[x] is not of mode character."           ,
                   uj::isTF1(na)                , "[na] must be TRUE or FALSE."             ,
                   uj::isT1(na) | uj::noneNAS(x), "[x] contains NA values but [na = FALSE].", PKG = "uj")
    if (uj::anyOKS(x)) {
      y <- tryCatch(grDevices::col2rgb(x[uj::ok(x)], T), error = function(e) e, finally = NULL)
      uj::err_if(uj::isERR(y), "[x] does not contain only valid color values.", PKG = "uj")
      y <- y / 255
      y <- grDevices::rgb(y[1, ], y[2, ], y[3, ], y[4, ])
      x[!uj::na(x)] <- y
    }
  } else if (uj::isIN1(mode, "fun", "Fun", "FUN")) {
    if (uj::notFUN(x)) {
      x <- tryCatch(base::match.fun(x), error = function(e) e, finally = NULL)
      uj::err_if(uj::isERR(x), "[x] is neither a function nor a character scalar name of a function.", PKG = "uj")
    }
  } else if (uj::isIN1(mode, "chr", "Chr", "CHR")) {x <- base::as.character(x)}
  else if (uj::isIN1(mode, "int", "Int", "INT")) {x <- base::as.integer(x)}
  else if (uj::isIN1(mode, "lgl", "Lgl", "LGL")) {x <- base::as.logical(x)}
  else if (uj::isIN1(mode, "num", "Num", "NUM")) {x <- base::as.numeric(x)}
  else if (uj::isIN1(mode, "ord", "Ord", "ORD")) {x <- base::factor(x, levels = levs, ordered = T)}
  else if (uj::isIN1(mode, "uno", "Uno", "UNO")) {x <- base::factor(x, levels = levs, ordered = F)}
  else {uj::stopper("Unrecognized mode.", pkg = "uj", fun = "asMODE")}
}

#' @rdname asMODE
#' @export
asCHR <- function(x, na = FALSE) {uj::asMODE(x, "chr")}

#' @rdname asMODE
#' @export
asCLR <- function(x, na = FALSE) {uj::asMODE(x, "clr", na = na)}

#' @rdname asMODE
#' @export
asFUN <- function(x) {uj::asMODE(x, "fun")}

#' @rdname asMODE
#' @export
asFUN <- function(x) {uj::asMODE(x, "chr")}

#' @rdname asMODE
#' @export
asINT <- function(x) {uj::asMODE(x, "int")}

#' @rdname asMODE
#' @export
asNUM <- function(x) {uj::asMODE(x, "num")}

#' @rdname asMODE
#' @export
asLGL <- function(x) {uj::asMODE(x, "lgl")}

#' @rdname asMODE
#' @export
asORD <- function(x, levs) {uj::asMODE(x, "ord", levs = levs)}

#' @rdname asMODE
#' @export
asUNO <- function(x, levs) {uj::asMODE(x, "uno", levs = levs)}
