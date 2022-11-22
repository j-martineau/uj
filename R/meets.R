.meets_errs <- function(x, ...) {
  if (...length() == 0) {return(T)}
  vars <- c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt')
  dots <- list(...)
  dot.names  <- names(dots)
  pop.names  <- dot.names[dot.names != ""]
  unq.names  <- unique(pop.names[pop.names != ""])
  all.unique <- f0(is.null(dot.names), T, setequal(pop.names, unq.names))
  no.blanks  <- f0(is.null(dot.names), T, !any(dot.names == ""))
  all.valid  <- f0(length(pop.names) == 0, T, all(dot.names %in% vars))
  errs <- c(f0(!no.blanks, NULL, "\n \u2022 All arguments in [...] must be named."),
            f0(all.unique, NULL, "\n \u2022 Arguments in [...] must be named uniquely."),
            f0(all.valid , NULL, "\n \u2022 Names of arguments in [...] must be in c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt')."))
  if (!is.null(errs)) {return(errs)}
  ok_vec <- function(dot) {
    f0(is.null(dot), T,
       f0(!is.atomic(dot) | length(dot) == 0, F,
         f0(is.vector(dot), T,
           f0(!is.array(dot), F,
              length(which(dim(dot) > 1)) < 2))))
  }
  ok_scl <- function(dot) {
    f0(is.null(dot), T,
      f0(!ok_vec(dot), F,
         length(dot) == 1))
  }
  ok_ind_vec <- function(dot) {
    f0(is.null(dot), T,
      f0(!ok_vec(dot), F,
         !any(is.na(dot)) & all(dot >= 0 & round(dot) == dot)))
  }
  ok_ind_scl <- function(dot) {
    f0(is.null(dot), T,
      f0(!ok_scl(dot), F,
         !is.na(dot) & dot >= 0 & round(dot) == dot ))
  }
  ok_srt_vec <- function(obj, dot) {
    obj.wild <- f0(is.null(obj), T, f0(!is.atomic, F, f0(length(obj) == 0, T, all(is.na(obj)))))
    obj.chr <- is.character(obj); dot.chr <- is.character(dot)
    obj.num <- is.numeric(obj); dot.num <- is.numeric(dot)
    obj.lgl <- is.logical(obj); dot.lgl <- is.logical(dot)
    obj.ord <- is.ordered(obj); dot.ord <- is.ordered(dot)
    obj.levs <- f0(!obj.ord, NULL, levels(obj)); obj.nlevs <- length(obj.levs)
    dot.levs <- f0(!dot.ord, NULL, levels(dot)); dot.nlevs <- length(dot.levs)
    f0(is.null(dot), T,
       f0(!ok_vec(dot), F,
          f0(!dot.num & !dot.lgl & !dot.chr & !dot.ord, F,
             f0(!is.atomic(obj), T,
                f0(obj.wild, T,
                   f0(obj.chr & dot.chr, T,
                      f0(obj.num & dot.num, T,
                         f0(obj.lgl & dot.lgl, T,
                            f0(!obj.ord | !dot.ord, F,
                               f0(obj.nlevs != dot.nlevs, F,
                                  all(obj.levs == dot.levs)))))))))))
  }
  ok_srt_scl <- function(obj, dot) {
    f0(is.null(dot), T,
      f0(!ok_srt_vec(obj, dot), F,
        length(dot) == 1))
  }
  lt <- dots$lt; min  <- dots$min ; max  <- dots$max ; n  <- dots$n
  le <- dots$le; minr <- dots$minr; maxr <- dots$maxr; nr <- dots$nr
  ge <- dots$ge; minc <- dots$minc; maxc <- dots$maxc; nc <- dots$nc
  gt <- dots$gt; vals <- dots$vals
  c(f0(ok_ind_vec(n ), NULL, "\n \u2022 [n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
    f0(ok_ind_vec(nr), NULL, "\n \u2022 [nr] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
    f0(ok_ind_vec(nc), NULL, "\n \u2022 [nc] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
    f0(ok_ind_scl(min), NULL, "\n \u2022 [min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    f0(ok_ind_scl(max), NULL, "\n \u2022 [max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    f0(ok_ind_scl(minr), NULL, "\n \u2022 [minr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    f0(ok_ind_scl(minc), NULL, "\n \u2022 [minc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    f0(ok_ind_scl(maxr), NULL, "\n \u2022 [maxr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    f0(ok_ind_scl(maxc), NULL, "\n \u2022 [maxc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    f0(ok_srt_scl(x, lt), NULL, "\n \u2022 [lt] must be NULL or a non-NA atomic scalar (?cmp_scl) comparable with [x] (?comparable)."),
    f0(ok_srt_scl(x, le), NULL, "\n \u2022 [le] must be NULL or a non-NA atomic scalar (?cmp_scl) comparable with [x] (?comparable)."),
    f0(ok_srt_scl(x, ge), NULL, "\n \u2022 [ge] must be NULL or a non-NA atomic scalar (?cmp_scl) comparable with [x] (?comparable)."),
    f0(ok_srt_scl(x, gt), NULL, "\n \u2022 [gt] must be NULL or a non-NA atomic scalar (?cmp_scl) comparable with [x] (?comparable)."),
    f0(ok_srt_vec(x, vals), NULL, "\n \u2022 [vals] must be NULL or a complete atomic vec (?cmp_vec) comparable with [x] (?comparable)."))
}

#' @name meets
#' @family props
#' @title Does an Object Meet Count and/or Value Restrictions?
#' @description Count and value restrictions are provided in \code{...}. If none
#'   are provided
#' @param x An object.
#' @param ... Optional named arguments specifying (additional) property
#'   requirements for \code{x} in terms of element values and/or
#'   element/row/column counts. See the \emph{Specifying Count and Value
#'   Restrictions} section.
#' @section Specifying Count and Value Restrictions: Specifying additional
#' requirements in \code{...} is optional. The full set of recognized
#' arguments names are defined in the following table along with the
#' properties each specifies:\tabular{ll}{
#' NAME          \tab WHAT IT SPECIFIES                                      \cr
#' \code{n}      \tab Vector of valid lengths/numbers of elements.           \cr
#' \code{nr}     \tab Vector of valid numbers of rows.                       \cr
#' \code{nc}     \tab Vector of valid numbers of columns.                    \cr
#' \code{min}    \tab Scalar minimum valid length/number of element.         \cr
#' \code{minr}   \tab Scalar minimum valid number of rows.                   \cr
#' \code{minc}   \tab Scalar minimum valid number of columns.                \cr
#' \code{max}    \tab Scalar maximum valid length/number of element.         \cr
#' \code{maxr}   \tab Scalar maximum valid number of rows.                   \cr
#' \code{maxc}   \tab Scalar maximum valid number of columns.                \cr
#' \code{vals}   \tab Vector of valid values.                                \cr
#' \code{lt}     \tab Scalar less-than (exclusive upper) bound.              \cr
#' \code{le}     \tab Scalar less-than-or-equal (inclusive upper) bound      \cr
#' \code{ge}     \tab Scalar greater-than-or-equal (inclusive lower) bound.  \cr
#' \code{gt}     \tab Scalar greater-than bound (exclusive lower) bound.       }
#' @return A logical scalar.
#' @export
meets <- function(x, ...) {
  errs <- .meets_errs(x, ...)
  if (!inll(errs)) {stop(errs)}
  av <- av(x)                                                                    # atomic values from {x}
  nx <- f0(id1D(x), length(x), prod(dim(x)))                                     # length of {x}
  nr <- NROW(x)                                                                  # number of rows in {x}
  nc <- NCOL(x)                                                                  # number of columns in {x}
  av <- av[!is.na(av)]                                                           # remove na values of {x}
  if (length(av) == 0) {return(T)}                                               # if 0-length and has passed validation, meets requirements
  if (!is.null(n   )) {if (   !(nx %in% n   )) {return(F)}}                      # check for not meeting element count requirements
  if (!is.null(min )) {if (   !(nx  >=  min )) {return(F)}}
  if (!is.null(max )) {if (   !(nx  <=  max )) {return(F)}}
  if (!is.null(nr  )) {if (   !(nr %in% nr  )) {return(F)}}                      # check for not meeting row count requirements
  if (!is.null(minr)) {if (   !(nr  >=  minr)) {return(F)}}
  if (!is.null(maxr)) {if (   !(nr  <=  maxr)) {return(F)}}
  if (!is.null(nc  )) {if (   !(nc %in% nc  )) {return(F)}}                      # check for not meeting column count requirements
  if (!is.null(minc)) {if (   !(nc  >=  minc)) {return(F)}}
  if (!is.null(maxc)) {if (   !(nc  <=  maxc)) {return(F)}}
  if (!is.null(lt  )) {if ( any(av  >=  lt  )) {return(F)}}                      # check for not meeting value requirements
  if (!is.null(gt  )) {if ( any(av  <=  gt  )) {return(F)}}
  if (!is.null(le  )) {if ( any(av  >   le  )) {return(F)}}
  if (!is.null(ge  )) {if ( any(av  >   ge  )) {return(F)}}
  if (!is.null(vals)) {if (!all(av %in% vals)) {return(F)}}
  T                                                                              # meets all requirements
}
