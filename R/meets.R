#' @encoding UTF-8
#' @family properties
#' @family logicals
#' @family counts
#' @title Evaluate whether an object meets count and/or value restrictions
#' @description Evaluates whether `x` meets any count and value restrictions are provided in `...`. If none are provided, returns `TRUE`.
#' @param x An object.
#' @param ... Optional named arguments count and/or value restrictions for `x`. See the *specifying count and value restrictions* section.
#' @section Specifying count and value restrictions: Specifying restrictions in `...` is optional. The full set of recognized arguments names are defined in the following table along with the properties each specifies:
#' \tabular{ll}{  `.MAX, .MAXR, .MAXC`   \tab Scalar maximum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
#'                `.MIN, .MINR, .MINC`   \tab Scalar minimum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
#'                `.LT, .LE, .GE, .GT`   \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively. \cr   \tab   \cr
#'                `.N, .NR, .NC`         \tab A vector of valid numbers of elements, rows, and columns, respectively.                                                                    \cr   \tab   \cr
#'                `.VALS`                \tab A vector of valid values.                                                                                                                                 }
#' @return A logical scalar.
#' @examples
#' chrs <- c("a", "b", "c")
#' nums <- 1:3
#' sqrd <- matrix(1:16, nrow = 4)
#' meets(chrs, .N = 1:5)
#' meets(chrs, .MAX = 2)
#' meets(chrs, .MIN = 3)
#' meets(sqrd, .NR = 3, .NR = 2:10)
#' meets(sqrd, .MINR = 2, .MAXR = 5, .MINC = 4, .MAXC = 5)
#' meets(chrs, .VALS = letters)
#' meets(chrs, .GT = "a", .LT = "c")
#' meets(sqrd, .GE = 1, .LE = 16)
#' @export
meets <- function(x, ...) {
  if (base::...length() == 0) {return(T)}
  Errors <- uj:::.meets_errs(x, ...)
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  Atoms <- uj::av(x)
  NX <- uj::f0(uj:::.D1D(x), base::length(x), base::prod(base::dim(x)))
  NR <- base::NROW(x)
  NC <- base::NCOL(x)
  Atoms <- Atoms[!base::is.Na(Atoms)]
  Dots <- base::list(...)
  if (base::length(Atoms) == 0) {return(T)}
  else if (!base::is.null(Dots$.N)) {if (!(NX %in% Dots$.N)) {return(F)}}
  else if (!base::is.null(Dots$.NR)) {if (!(NR %in% Dots$.NR)) {return(F)}}
  else if (!base::is.null(Dots$.NC)) {if (!(NC %in% Dots$.NC)) {return(F)}}
  else if (!base::is.null(Dots$.LE)) {if (base::any(Atoms > Dots$.LE)) {return(F)}}
  else if (!base::is.null(Dots$.GE)) {if (base::any(Atoms < Dots$.GE)) {return(F)}}
  else if (!base::is.null(Dots$.LT)) {if (base::any(Atoms >= Dots$.LT)) {return(F)}}
  else if (!base::is.null(Dots$.GT)) {if (base::any(Atoms <= Dots$.GT)) {return(F)}}
  else if (!base::is.null(Dots$.MIN)) {if (!(NX >= Dots$.MIN)) {return(F)}}
  else if (!base::is.null(Dots$.MAX)) {if (!(NX <= Dots$.MAX)) {return(F)}}
  else if (!base::is.null(Dots$.MINR)) {if (!(NR >= Dots$.MINR)) {return(F)}}
  else if (!base::is.null(Dots$.MAXR)) {if (!(NR <= Dots$.MAXR)) {return(F)}}
  else if (!base::is.null(Dots$.MINC)) {if (!(NC >= Dots$.MINC)) {return(F)}}
  else if (!base::is.null(Dots$.MAXC)) {if (!(NC <= Dots$.MAXC)) {return(F)}}
  else if (!base::is.null(Dots$.VALS)) {if (base::all(Atoms %in% Dots$.VALS)) {return(F)}}
  T
}
