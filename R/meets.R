#' @name meets.
#' @family props
#' @title Does an object meet count and/or value restrictions?
#' @param x An object.
#' @param ... Additional restrictive arguments. See details.
#' @details Additional restrictive arguments are optional. When no additional
#'   arguments are supplied \code{TRUE} is returned. Possible arguments are as
#'   follows:\tabular{ll}{
#'   NAME          \tab WHAT IT SPECIFIES                                    \cr
#'   \code{n}      \tab The set of valid lengths/numbers of elements         \cr
#'   \code{nr}     \tab The set of valid numbers of rows                     \cr
#'   \code{nc}     \tab The set of valid numbers of columns                  \cr
#'   \code{min}    \tab The minimum valid length/number of element           \cr
#'   \code{minr}   \tab The minimum valid number of rows                     \cr
#'   \code{minc}   \tab The minimum valid number of columns                  \cr
#'   \code{max}    \tab The maximum valid length/number of element           \cr
#'   \code{maxr}   \tab The maximum valid number of rows                     \cr
#'   \code{maxc}   \tab The maximum valid number of columns                  \cr
#'   \code{vals}   \tab The set of valid values                              \cr
#'   \code{lt}     \tab The value all values of \code{x.} must be less than  \cr
#'   \code{le}     \tab The value all values of \code{x.} must be less than or
#'                      equal to                                             \cr
#'   \code{ge}     \tab The value all values of \code{x.} must be less than or
#'                      equal to                                             \cr
#'   \code{gt}     \tab The value all values of \code{x.} must be greater than }
#' @return \code{TRUE} or \code{FALSE}.
#' @export
meets. <- function() {help("meets.", package = "uj")}

#' @describeIn meets. Checks whether \code{x.} meets a variety of count and
#'   value restrictions supplied through \code{...}.
meets <- function(x, ...) {
  if (...length() == 0) {return(T)}
  vars <- 'n.nr.nc.min.minr.minc.max.maxr.maxc.vals.lt.le.le.ge.gt'
  vars <- av(strsplit(vars, ".", T))
  dots <- list(...)
  labs <- names(dots)
  ok.labs <- f0(is_unq(labs), all(labs %in% vars), F)
  if (!ok.labs) {stop("\n \u2022 Arguments in [...] must be uniquely named with names in c('n', 'min', 'max', 'nr', 'minr', 'maxr', 'nc', 'minc', 'maxc', 'vals', 'lt', 'le', 'ge', 'gt').")}
  lt   <- dots$lt; min  <- dots$min ; max  <- dots$max ; n  <- dots$n
  le   <- dots$le; minr <- dots$minr; maxr <- dots$maxr; nr <- dots$nr
  ge   <- dots$ge; minc <- dots$minc; maxc <- dots$maxc; nc <- dots$nc
  gt   <- dots$gt; vals <- dots$vals
  x2d  <- id2D(x)
  ok.vals <- nll_or(vals, 'pop_atm')
  ok.lt <- nll_or(lt, 'srt_scl')  ;  ok.le <- nll_or(le, 'srt_scl')
  ok.ge <- nll_or(ge, 'srt_scl')  ;  ok.gt <- nll_or(gt, 'srt_scl')
  errs <- c(f0(f0(x2d, T, length(c(nr, minr, maxr, nc, minc, maxc)) == 0), NULL, "\n \u2022 [nr], [minr], [maxr], [nc], [minc], and [maxc] must be NULL when [x.] is not a matrix or dft (?is_dtf)."),
            f0(nll_or(n   , 'cmp_nnw_vec')                               , NULL, "\n \u2022 [n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(nll_or(nr  , 'cmp_nnw_vec')                               , NULL, "\n \u2022 [nr] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(nll_or(nc  , 'cmp_nnw_vec')                               , NULL, "\n \u2022 [nc] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(nll_or(min , 'cmp_nnw_scl')                               , NULL, "\n \u2022 [min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(nll_or(minr, 'cmp_nnw_scl')                               , NULL, "\n \u2022 [minr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(nll_or(minc, 'cmp_nnw_scl')                               , NULL, "\n \u2022 [minc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(nll_or(max , 'cmp_nnw_scl')                               , NULL, "\n \u2022 [max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(nll_or(maxr, 'cmp_nnw_scl')                               , NULL, "\n \u2022 [maxr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(nll_or(maxc, 'cmp_nnw_scl')                               , NULL,  "\n \u2022 [maxc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(ok.vals                                                   , NULL, "\n \u2022 [vals] must be NULL or a complete atomic object (?cmp)."),
            f0(ok.lt                                                     , NULL, "\n \u2022 [lt] must be NULL or a complete sortable atomic scalar (?cmp_scl)."),
            f0(ok.le                                                     , NULL, "\n \u2022 [le] must be NULL or a complete sortable atomic scalar (?cmp_scl)."),
            f0(ok.ge                                                     , NULL, "\n \u2022 [ge] must be NULL or a complete sortable atomic scalar (?cmp_scl)."),
            f0(ok.gt                                                     , NULL, "\n \u2022 [gt] must be NULL or a complete sortable atomic scalar (?cmp_scl)."),
            f0(f0(inll(vals) | !ok.vals , T, compatible(x, vals))        , NULL, "\n \u2022 Modes of [x.] and [vals] are not compatible."),
            f0(f0(inll(lt) | !ok.lt, T, comparable(x, lt))               , NULL, "\n \u2022 Modes of [x.] and [lt] are not comparable."),
            f0(f0(inll(le) | !ok.le, T, comparable(x, le))               , NULL, "\n \u2022 Modes of [x.] and [le] are not comparable."),
            f0(f0(inll(ge) | !ok.ge, T, comparable(x, ge))               , NULL, "\n \u2022 Modes of [x.] and [ge] are not comparable."),
            f0(f0(inll(gt) | !ok.gt, T, comparable(x, gt))               , NULL, "\n \u2022 Modes of [x.] and [gt] are not comparable."))
  if (!inll(errs)) {stop(errs)}
  av <- av(x)                                                                    # atomic values from {x}
  nx <- f0(id1D(x), length(x), prod(dim(x)))                                     # length of {x}
  nr <- NROW(x)                                                                  # number of rows in {x}
  nc <- NCOL(x)                                                                  # number of columns in {x}
  av <- av[!is.na(av)]                                                           # remove na values of {x}
  if (length(av) == 0) {return(T)}                                               # if 0-length and has passed validation, meets requirements
  if (idef(n   )) {if (   !(nx %in% n   )) {return(F)}}                          # check for not meeting element count requirements
  if (idef(min )) {if (   !(nx  >=  min )) {return(F)}}
  if (idef(max )) {if (   !(nx  <=  max )) {return(F)}}
  if (idef(nr  )) {if (   !(nr %in% nr  )) {return(F)}}                          # check for not meeting row count requirements
  if (idef(minr)) {if (   !(nr  >=  minr)) {return(F)}}
  if (idef(maxr)) {if (   !(nr  <=  maxr)) {return(F)}}
  if (idef(nc  )) {if (   !(nc %in% nc  )) {return(F)}}                          # check for not meeting column count requirements
  if (idef(minc)) {if (   !(nc  >=  minc)) {return(F)}}
  if (idef(maxc)) {if (   !(nc  <=  maxc)) {return(F)}}
  if (idef(lt  )) {if ( any(av  >=  lt  )) {return(F)}}                          # check for not meeting value requirements
  if (idef(gt  )) {if ( any(av  <=  gt  )) {return(F)}}
  if (idef(le  )) {if ( any(av  >   le  )) {return(F)}}
  if (idef(ge  )) {if ( any(av  >   ge  )) {return(F)}}
  if (idef(vals)) {if (!all(av %in% vals)) {return(F)}}
  T                                                                              # meets all requirements
}
