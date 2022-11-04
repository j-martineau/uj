#' @name meets_uj
#' @family props
#' @title Does an object meet count and/or value restrictions
#' @param x. An object.
#' @param ... Additional restrictive arguments. See details.
#' @details Additional restrictive arguments are optional. When no additional
#'   arguments are supplied \code{TRUE} is returned. Possible arguments are as
#'   follows:
#'   \tabular{ll}{
#'     NAME        \tab WHAT IT SPECIFIES                                    \cr
#'     \code{n}    \tab The set of valid lengths/numbers of elements         \cr
#'     \code{nr}   \tab The set of valid numbers of rows                     \cr
#'     \code{nc}   \tab The set of valid numbers of columns                  \cr
#'     \code{min}  \tab The minimum valid length/number of element           \cr
#'     \code{minr} \tab The minimum valid number of rows                     \cr
#'     \code{minc} \tab The minimum valid number of columns                  \cr
#'     \code{max}  \tab The maximum valid length/number of element           \cr
#'     \code{maxr} \tab The maximum valid number of rows                     \cr
#'     \code{maxc} \tab The maximum valid number of columns                  \cr
#'     \code{vals} \tab The set of valid values                              \cr
#'     \code{lt}   \tab The value all values of \code{x.} must be less than  \cr
#'     \code{le}   \tab The value all values of \code{x.} must be less than or
#'                      equal to                                             \cr
#'     \code{ge}   \tab The value all values of \code{x.} must be less than or
#'                      equal to                                             \cr
#'     \code{gt}   \tab The value all values of \code{x.} must be greater than }
#' @return \code{TRUE} or \code{FALSE}.
#' @export
meets_uj <- function() {help("meets_uj", package = "uj")}

#' @describeIn meets_uj Checks whether \code{x.} meets a variety of count and
#'   value restrictions supplied through \code{...}.
meets <- function(x., ...) {
  if (...length() == 0) {return(T)}
  vars. <- 'n.nr.nc.min.minr.minc.max.maxr.maxc.vals.lt.le.le.ge.gt'
  vars. <- av(strsplit(vars., ".", T))
  dots. <- list(...)
  labs. <- names(dots.)
  vd. <- f0(is_unique(labs.), all(labs. %in% vars.), F)
  if (!vd.) {stop("\n • Arguments in [...] must be uniquely named with names in c('n', 'min', 'max', 'nr', 'minr', 'maxr', 'nc', 'minc', 'maxc', 'vals', 'lt', 'le', 'ge', 'gt').")}
  lt   <- dots.$lt; min  <- dots.$min ; max  <- dots.$max ; n  <- dots.$n
  le   <- dots.$le; minr <- dots.$minr; maxr <- dots.$maxr; nr <- dots.$nr
  ge   <- dots.$ge; minc <- dots.$minc; maxc <- dots.$maxc; nc <- dots.$nc
  gt   <- dots.$gt; vals <- dots.$vals
  x2d.  <- id2D(x.)
  v2d.  <- f0(x2d., T, length(c(nr, minr, maxr, nc, minc, maxc)) == 0)
  vn.   <- nll_or(n   , 'cmp_nnw_vec')
  vnr.  <- nll_or(nr  , 'cmp_nnw_vec')
  vnc.  <- nll_or(nc  , 'cmp_nnw_vec')
  vmn.  <- nll_or(min , 'cmp_nnw_scl')
  vmnr. <- nll_or(minr, 'cmp_nnw_scl')
  vmnc. <- nll_or(minc, 'cmp_nnw_scl')
  vmx.  <- nll_or(max , 'cmp_nnw_scl')
  vmxr. <- nll_or(maxr, 'cmp_nnw_scl')
  vmxc. <- nll_or(maxc, 'cmp_nnw_scl')
  vv.   <- nll_or(vals, 'pop_atm')
  vlt.  <- nll_or(lt  , 'srt_scl')
  vle.  <- nll_or(le  , 'srt_scl')
  vge.  <- nll_or(ge  , 'srt_scl')
  vgt.  <- nll_or(gt  , 'srt_scl')
  vv2.  <- f0(inll(vals) | !vv. , T, compatible(x., vals))
  vlt2. <- f0(inll(lt) | !vlt., T, comparable(x., lt))
  vle2. <- f0(inll(le) | !vle., T, comparable(x., le))
  vge2. <- f0(inll(ge) | !vge., T, comparable(x., ge))
  vgt2. <- f0(inll(gt) | !vgt., T, comparable(x., gt))
  err. <- NULL
  if (!vn.  ) {err. <- c(err., "\n • [n] must be NULL or a complete, non-negative, whole-number vector")}
  if (!vnr. ) {err. <- c(err., "\n • [nr] must be NULL or a complete, non-negative, whole-number vector")}
  if (!vnc. ) {err. <- c(err., "\n • [nc] must be NULL or a complete, non-negative, whole-number vector")}
  if (!vmn. ) {err. <- c(err., "\n • [min] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!vmx. ) {err. <- c(err., "\n • [max] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!vmnr.) {err. <- c(err., "\n • [minr] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!vmxr.) {err. <- c(err., "\n • [maxr] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!vmnc.) {err. <- c(err., "\n • [minc] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!vmxc.) {err. <- c(err., "\n • [maxc] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!v2d. ) {err. <- c(err., "\n • [nr], [minr], [maxr], [nc], [minc], and [maxc] must be NULL when [x.] is not a matrix or tibble.")}
  if (!vv.  ) {err. <- c(err., "\n • [vals] must be NULL or a complete, atomic object.")}
  if (!vlt. ) {err. <- c(err., "\n • [lt] must be NULL or a complete, sortable atomic scalar.")}
  if (!vle. ) {err. <- c(err., "\n • [le] must be NULL or a complete, sortable atomic scalar.")}
  if (!vge. ) {err. <- c(err., "\n • [ge] must be NULL or a complete, sortable atomic scalar.")}
  if (!vgt. ) {err. <- c(err., "\n • [gt] must be NULL or a complete, sortable atomic scalar.")}
  if (!vv2. ) {err. <- c(err., "\n • Modes of [x.] and [vals] are not compatible.")}
  if (!vlt2.) {err. <- c(err., "\n • Modes of [x.] and [lt] are not comparable.")}
  if (!vle2.) {err. <- c(err., "\n • Modes of [x.] and [le] are not comparable.")}
  if (!vge2.) {err. <- c(err., "\n • Modes of [x.] and [ge] are not comparable.")}
  if (!vgt2.) {err. <- c(err., "\n • Modes of [x.] and [gt] are not comparable.")}
  if (!inll(err.)) {stop(err.)}
  av.  <- av(x.)                                                                 # atomic values from {x}
  nx. <- f0(id1D(x.), length(x.), prod(dim(x.)))                                 # length of {x}
  nr. <- NROW(x.)                                                                # number of rows in {x}
  nc. <- NCOL(x.)                                                                # number of columns in {x}
  av.  <- av.[!is.na(av.)]                                                       # remove na values of {x}
  if (length(av.) == 0) {return(T)}                                              # if 0-length and has passed validation, meets requirements
  if (idef(n   )) {if (   !(nx. %in% n   )) {return(F)}}                         # check for not meeting element count requirements
  if (idef(min )) {if (   !(nx.  >=  min )) {return(F)}}
  if (idef(max )) {if (   !(nx.  <=  max )) {return(F)}}
  if (idef(nr  )) {if (   !(nr. %in% nr  )) {return(F)}}                         # check for not meeting row count requirements
  if (idef(minr)) {if (   !(nr.  >=  minr)) {return(F)}}
  if (idef(maxr)) {if (   !(nr.  <=  maxr)) {return(F)}}
  if (idef(nc  )) {if (   !(nc. %in% nc  )) {return(F)}}                         # check for not meeting column count requirements
  if (idef(minc)) {if (   !(nc.  >=  minc)) {return(F)}}
  if (idef(maxc)) {if (   !(nc.  <=  maxc)) {return(F)}}
  if (idef(lt  )) {if ( any(av.  >=  lt  )) {return(F)}}                         # check for not meeting value requirements
  if (idef(gt  )) {if ( any(av.  <=  gt  )) {return(F)}}
  if (idef(le  )) {if ( any(av.  >   le  )) {return(F)}}
  if (idef(ge  )) {if ( any(av.  >   ge  )) {return(F)}}
  if (idef(vals)) {if (!all(av. %in% vals)) {return(F)}}
  T                                                                              # meets all requirements
}
