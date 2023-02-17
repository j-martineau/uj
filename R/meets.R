.meets_errs <- function(x, ...) {
  if (uj::ND0()) {return(NULL)}
  vars <- base::c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt')
  dots <- base::list(...)
  dot.names  <- uj::EN(dots)
  pop.names  <- dot.names[dot.names != ""]
  unq.names  <- uj::UV(pop.names[pop.names != ""])
  all.unique <- uj::f0(uj::NLL(dot.names), T, uj::isSEQ(pop.names, unq.names))
  no.blanks  <- uj::f0(uj::NLL(dot.names), T, uj::noneBL(dot.names))
  all.valid  <- uj::f0(uj::N0(pop.names), T, uj::allIN(dot.names, vars))
  errs <- base::c(uj::f0(no.blanks , NULL, "All arguments in [...] must be named."),
                  uj::f0(all.unique, NULL, "Arguments in [...] must be named uniquely."),
                  uj::f0(all.valid , NULL, "Names of arguments in [...] must be in c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt')."))
  if (uj::DEF(errs)) {return(errs)}
  okVec <- function(dot) {uj::f0(uj::NLL(dot), T, uj::f0(uj::notATM(dot) | uj::N0(dot), F, uj::f0(uj::isVEC(dot), T, uj::f0(uj::notARR(dot), F, uj::notNDIM2P(dot)))))}
  okScl <- function(dot) {uj::f0(uj::NLL(dot), T, uj::f0(!okVec(dot), F, uj::N1(dot)))}
  okIndVec <- function(dot) {uj::f0(uj::NLL(dot), T, uj::f0(!okVec(dot), F, uj::noneNA(dot) & base::all(dot >= 0 & uj::rounded(dot))))}
  okIndScl <- function(dot) {uj::f0(uj::NLL(dot), T, uj::f0(!okScl(dot), F, uj::ok(dot) & dot >= 0 & uj::rounded(dot)))}
  okSrtVec <- function(obj, dot) {
    obj.wild <- uj::f0(uj::NLL(obj), T, uj::f0(uj::notATM(obj), F, uj::f0(uj::N0(obj), T, base::allNAS(obj))))
    obj.chr <- uj::isCHR(obj); dot.chr <- uj::isCHR(dot)
    obj.num <- uj::isNUM(obj); dot.num <- uj::isNUM(dot)
    obj.lgl <- uj::isLGL(obj); dot.lgl <- uj::isLGL(dot)
    obj.ord <- uj::isORD(obj); dot.ord <- uj::isORD(dot)
    obj.levs <- uj::f0(!obj.ord, NULL, uj::LEVS(obj)); obj.nlevs <- uj::N(obj.levs)
    dot.levs <- uj::f0(!dot.ord, NULL, uj::LEVS(dot)); dot.nlevs <- uj::N(dot.levs)
    uj::f0(uj::NLL(dot), T,
           uj::f0(!okVec(dot), F,
                  uj::f0(!dot.num & !dot.lgl & !dot.chr & !dot.ord, F,
                         uj::f0(uj::notATM(obj), T,
                                uj::f0(obj.wild, T,
                                       uj::f0(obj.chr & dot.chr, T,
                                              uj::f0(obj.num & dot.num, T,
                                                     uj::f0(obj.lgl & dot.lgl, T,
                                                            uj::f0(!obj.ord | !dot.ord, F,
                                                                   uj::f0(obj.nlevs != dot.nlevs, F,
                                                                          base::all(obj.levs == dot.levs)))))))))))
  }
  okSrtScl <- function(obj, dot) {uj::f0(uj::NLL(dot), T, uj::f0(!okSrtVec(obj, dot), F, uj::N1(dot)))}
  lt <- dots$lt; min  <- dots$min ; max  <- dots$max ; n  <- dots$n
  le <- dots$le; minr <- dots$minr; maxr <- dots$maxr; nr <- dots$nr
  ge <- dots$ge; minc <- dots$minc; maxc <- dots$maxc; nc <- dots$nc
  gt <- dots$gt; vals <- dots$vals
  base::c(
    uj::f0(okIndVec(n      ), NULL, "[n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
    uj::f0(okIndVec(nr     ), NULL, "[nr] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
    uj::f0(okIndVec(nc     ), NULL, "[nc] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
    uj::f0(okIndScl(min    ), NULL, "[min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    uj::f0(okIndScl(max    ), NULL, "[max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    uj::f0(okIndScl(minr   ), NULL, "[minr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    uj::f0(okIndScl(minc   ), NULL, "[minc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    uj::f0(okIndScl(maxr   ), NULL, "[maxr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    uj::f0(okIndScl(maxc   ), NULL, "[maxc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
    uj::f0(okSrtScl(x, lt  ), NULL, "[lt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
    uj::f0(okSrtScl(x, le  ), NULL, "[le] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
    uj::f0(okSrtScl(x, ge  ), NULL, "[ge] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
    uj::f0(okSrtScl(x, gt  ), NULL, "[gt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
    uj::f0(okSrtVec(x, vals), NULL, "[vals] must be NULL or a complete atomic vec (?cmp_vec) comparable with [x] (?comparable).")
  )
}

#' @encoding UTF-8
#' @family properties
#' @family logicals
#' @family counts
#' @title Evaluate whether an object meets count and/or value restrictions
#' @description Evaluates whether `x` meets any count and value restrictions are provided in `...`. If none are provided, returns `TRUE`.
#' @param x An object.
#' @param ... Optional named arguments count and/or value restrictions for `x`. See the *specifying count and value restrictions* section.
#' @section Specifying count and value restrictions: Specifying restrictions in `...` is optional. The full set of recognized arguments names are defined in the following table along with the properties each specifies:
#' \tabular{ll}{  `max, maxr, maxc`   \tab Scalar maximum valid numbers of element, rows, and columns, respectively.                                                                  \cr   \tab   \cr
#'                `min, minr, minc`   \tab Scalar minimum valid numbers of element, rows, and columns, respectively.                                                                  \cr   \tab     }
#' \tabular{ll}{  `lt, le, ge, gt`    \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively. \cr   \tab     }
#' \tabular{ll}{  `n, nr, nc`         \tab A vector of valid numbers of elements, rows, and columns, respectively.                                                                    \cr   \tab     }
#' \tabular{ll}{  `vals`              \tab A vector of valid values.                                                                                                                                 }
#' @return A logical scalar.
#' @examples
#' chrs <- c("a", "b", "c")
#' nums <- 1:3
#' sqrd <- matrix(1:16, nrow = 4)
#' meets(chrs, n = 1:5)
#' meets(chrs, max = 2)
#' meets(chrs, min = 3)
#' meets(sqrd, nr = 3, nr = 2:10)
#' meets(sqrd, minr = 2, maxr = 5, minc = 4, maxc = 5)
#' meets(chrs, vals = letters)
#' meets(chrs, gt = "a", lt = "c")
#' meets(sqrd, ge = 1, le = 16)
#' @export
meets <- function(x, ...) {
  if (uj::ND0()) {return(T)}
  uj::errs_if_pop(uj:::.meets_errs(x, ...), PKG = "uj")
  atoms <- uj::av(x)                                                             # atomic values from {x}
  nx <- uj::f0(uj::D1D(x), uj::N(x), base::prod(base::dim(x)))                  # length of {x}
  nr <- uj::NR(x)                                                                # number of rows in {x}
  nc <- uj::NC(x)                                                                # number of columns in {x}
  atoms <- atoms[uj::ok(atoms)]                                                 # remove na values of {x}
  d <- base::list(...)
  if (uj::N0(atoms)) {return(T)}
  else if (uj::DEF(d$n)) {if (uj::isMF1(nx, d$n)) {return(F)}}
  else if (uj::DEF(d$nr)) {if (uj::isMF1(nr, d$nr)) {return(F)}}
  else if (uj::DEF(d$nc)) {if (uj::isMF1(nc, d$nc)) {return(F)}}
  else if (uj::DEF(d$le)) {if (base::any(atoms > d$le)) {return(F)}}
  else if (uj::DEF(d$ge)) {if (base::any(atoms < d$ge)) {return(F)}}
  else if (uj::DEF(d$lt)) {if (base::any(atoms >= d$lt)) {return(F)}}
  else if (uj::DEF(d$gt)) {if (base::any(atoms <= d$gt)) {return(F)}}
  else if (uj::DEF(d$min)) {if (!(nx >= d$min)) {return(F)}}
  else if (uj::DEF(d$max)) {if (!(nx <= d$max)) {return(F)}}
  else if (uj::DEF(d$minr)) {if (!(nr >= d$minr)) {return(F)}}
  else if (uj::DEF(d$maxr)) {if (!(nr <= d$maxr)) {return(F)}}
  else if (uj::DEF(d$minc)) {if (!(nc >= d$minc)) {return(F)}}
  else if (uj::DEF(d$maxc)) {if (!(nc <= d$maxc)) {return(F)}}
  else if (uj::DEF(d$vals)) {if (uj::allIN(atoms, d$vals)) {return(F)}}
  T
}
