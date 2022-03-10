#' @name meets
#' @family props
#' @title Does an object meet count and/or value restrictions
#' @description Checks whether \code{x} meets a variety of count and value
#'   restrictions supplied through \code{...}.
#' @param x An object.
#' @param ... Additional restrictive arguments. See details.
#' @details Additional restrictive arguments are optional. When no additional
#'   arguments are supplied \code{TRUE} is returned. Possible arguments are as
#'   follows:
#'   \tabular{ll}{
#'        \code{n}   \tab The set of valid lengths/numbers of elements.
#'     \cr\code{nr}  \tab The set of valid numbers of rows.
#'     \cr\code{nc}  \tab The set of valid numbers of columns.
#'     \cr\code{min} \tab The minimum valid length/number of element.
#'     \cr\code{minr}\tab The minimum valid number of rows.
#'     \cr\code{minc}\tab The minimum valid number of columns.
#'     \cr\code{max} \tab The maximum valid length/number of element.
#'     \cr\code{maxr}\tab The maximum valid number of rows.
#'     \cr\code{maxc}\tab The maximum valid number of columns.
#'     \cr\code{vals}\tab The set of valid values.
#'     \cr\code{lt}  \tab The value all values of \code{x} must be less than.
#'     \cr\code{le}  \tab The value all values of \code{x} must be less than or
#'                        equal to.
#'     \cr\code{ge}  \tab The value all values of \code{x} must be less than or
#'                       equal to.
#'     \cr\code{gt}  \tab The value all values of \code{x} must be greater than.
#'   }
#' @return \code{TRUE} or \code{FALSE}.
#' @export
meets <- function(x, ...) {
  if (...length() == 0) {return(T)}
  V    <- 'n.nr.nc.min.minr.minc.max.maxr.maxc.vals.lt.le.le.ge.gt'
  V    <- av(strsplit(V, ".", T))
  D    <- list(...)
  N    <- names(D)
  VD   <- f0(is_unique(N), all(N %in% V), F)
  if (!VD) {stop("\n  * Arguments in [...] must be uniquely named with names in c('n', 'min', 'max', 'nr', 'minr', 'maxr', 'nc', 'minc', 'maxc', 'vals', 'lt', 'le', 'ge', 'gt').")}
  lt   <- D$lt; min  <- D$min ; max  <- D$max ; n  <- D$n
  le   <- D$le; minr <- D$minr; maxr <- D$maxr; nr <- D$nr
  ge   <- D$ge; minc <- D$minc; maxc <- D$maxc; nc <- D$nc
  gt   <- D$gt; vals <- D$vals
  X2D  <- xd2D(x)
  V2D  <- f0(X2D, T, length(c(nr, minr, maxr, nc, minc, maxc)) == 0)
  VN   <- nll_or(n   , 'cmp_nnw_vec')
  VNR  <- nll_or(nr  , 'cmp_nnw_vec')
  VNC  <- nll_or(nc  , 'cmp_nnw_vec')
  VMN  <- nll_or(min , 'cmp_nnw_scl')
  VMNR <- nll_or(minr, 'cmp_nnw_scl')
  VMNC <- nll_or(minc, 'cmp_nnw_scl')
  VMX  <- nll_or(max , 'cmp_nnw_scl')
  VMXR <- nll_or(maxr, 'cmp_nnw_scl')
  VMXC <- nll_or(maxc, 'cmp_nnw_scl')
  VV   <- nll_or(vals, 'pop_atm')
  VLT  <- nll_or(lt  , 'srt_scl')
  VLE  <- nll_or(le  , 'srt_scl')
  VGE  <- nll_or(ge  , 'srt_scl')
  VGT  <- nll_or(gt  , 'srt_scl')
  VV2  <- f0(xnll(vals) | !VV , T, compatible(x, vals))
  VLT2 <- f0(xnll(lt) | !VLT, T, comparable(x, lt))
  VLE2 <- f0(xnll(le) | !VLE, T, comparable(x, le))
  VGE2 <- f0(xnll(ge) | !VGE, T, comparable(x, ge))
  VGT2 <- f0(xnll(gt) | !VGT, T, comparable(x, gt))
  E <- NULL
  if (!VN  ) {E <- c(E, "\n  * [n] must be NULL or a complete, non-negative, whole-number vector")}
  if (!VNR ) {E <- c(E, "\n  * [nr] must be NULL or a complete, non-negative, whole-number vector")}
  if (!VNC ) {E <- c(E, "\n  * [nc] must be NULL or a complete, non-negative, whole-number vector")}
  if (!VMN ) {E <- c(E, "\n  * [min] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!VMX ) {E <- c(E, "\n  * [max] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!VMNR) {E <- c(E, "\n  * [minr] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!VMXR) {E <- c(E, "\n  * [maxr] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!VMNC) {E <- c(E, "\n  * [minc] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!VMXC) {E <- c(E, "\n  * [maxc] must be NULL or a complete, non-negative, whole-number scalar.")}
  if (!V2D ) {E <- c(E, "\n  * [nr], [minr], [maxr], [nc], [minc], and [maxc] must be NULL when [x] is not a matrix or tibble.")}
  if (!VV  ) {E <- c(E, "\n  * [vals] must be NULL or a complete, atomic object.")}
  if (!VLT ) {E <- c(E, "\n  * [lt] must be NULL or a complete, sortable atomic scalar.")}
  if (!VLE ) {E <- c(E, "\n  * [le] must be NULL or a complete, sortable atomic scalar.")}
  if (!VGE ) {E <- c(E, "\n  * [ge] must be NULL or a complete, sortable atomic scalar.")}
  if (!VGT ) {E <- c(E, "\n  * [gt] must be NULL or a complete, sortable atomic scalar.")}
  if (!VV2 ) {E <- c(E, "\n  * Modes of [x] and [vals] are not compatible.")}
  if (!VLT2) {E <- c(E, "\n  * Modes of [x] and [lt] are not comparable.")}
  if (!VLE2) {E <- c(E, "\n  * Modes of [x] and [le] are not comparable.")}
  if (!VGE2) {E <- c(E, "\n  * Modes of [x] and [ge] are not comparable.")}
  if (!VGT2) {E <- c(E, "\n  * Modes of [x] and [gt] are not comparable.")}
  if (!xnll(E)) {stop(E)}
  X  <- av(x)                                                                    # atomic values from {x}
  NX <- f0(xd1D(x), length(x), prod(dim(x)))                                     # length of {x}
  NR <- NROW(x)                                                                  # number of rows in {x}
  NC <- NCOL(x)                                                                  # number of columns in {x}
  X  <- X[!is.na(X)]                                                             # remove na values of {x}
  if (length(X) == 0) {return(T)}                                                # if 0-length and has passed validation, meets requirements
  if (xdef(n   )) {if (   !(NX %in% n   )) {return(F)}}                          # check for not meeting element count requirements
  if (xdef(min )) {if (   !(NX  >=  min )) {return(F)}}
  if (xdef(max )) {if (   !(NX  <=  max )) {return(F)}}
  if (xdef(nr  )) {if (   !(NR %in% nr  )) {return(F)}}                          # check for not meeting row count requirements
  if (xdef(minr)) {if (   !(NR  >=  minr)) {return(F)}}
  if (xdef(maxr)) {if (   !(NR  <=  maxr)) {return(F)}}
  if (xdef(nc  )) {if (   !(NC %in% nc  )) {return(F)}}                          # check for not meeting column count requirements
  if (xdef(minc)) {if (   !(NC  >=  minc)) {return(F)}}
  if (xdef(maxc)) {if (   !(NC  <=  maxc)) {return(F)}}
  if (xdef(lt  )) {if ( any(X   >=  lt  )) {return(F)}}                          # check for not meeting value requirements
  if (xdef(gt  )) {if ( any(X   <=  gt  )) {return(F)}}
  if (xdef(le  )) {if ( any(X   >   le  )) {return(F)}}
  if (xdef(ge  )) {if ( any(X   >   ge  )) {return(F)}}
  if (xdef(vals)) {if (!all(X  %in% vals)) {return(F)}}
  T                                                                              # meets all requirements
}
