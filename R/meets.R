.meets_errs <- function(x, ...) {
  if (base::...length() == 0) {return(NULL)}
  vars <- base::c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt')
  dots <- base::list(...)
  dot.names  <- base::names(dots)
  pop.names  <- dot.names[dot.names != ""]
  unq.names  <- base::unique(pop.names[pop.names != ""])
  all.unique <- uj::f0(base::is.null(dot.names), T, base::setequal(pop.names, unq.names))
  no.blanks  <- uj::f0(base::is.null(dot.names), T, !base::any(dot.names == ""))
  all.valid  <- uj::f0(base::length(pop.names) == 0, T, base::all(dot.names %in% vars))
  errs <- base::c(uj::f0(no.blanks , NULL, "\n \u2022 All arguments in [...] must be named."),
                  uj::f0(all.unique, NULL, "\n \u2022 Arguments in [...] must be named uniquely."),
                  uj::f0(all.valid , NULL, "\n \u2022 Names of arguments in [...] must be in c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt')."))
  if (!base::is.null(errs)) {return(errs)}
  ok_vec <- function(dot) {
    uj::f0(base::is.null(dot), T,
       uj::f0(!base::is.atomic(dot) | base::length(dot) == 0, F,
         uj::f0(base::is.vector(dot), T,
           uj::f0(!base::is.array(dot), F,
              length(base::which(base::dim(dot) > 1)) < 2))))
  }
  ok_scl <- function(dot) {uj::f0(base::is.null(dot), T, uj::f0(!ok_vec(dot), F, base::length(dot) == 1))}
  ok_ind_vec <- function(dot) {uj::f0(base::is.null(dot), T, uj::f0(!ok_vec(dot), F, !base::any(base::is.na(dot)) & base::all(dot >= 0 & base::round(dot) == dot)))}
  ok_ind_scl <- function(dot) {uj::f0(base::is.null(dot), T, uj::f0(!ok_scl(dot), F, !base::is.na(dot) & dot >= 0 & base::round(dot) == dot ))}
  ok_srt_vec <- function(obj, dot) {
    obj.wild <- uj::f0(base::is.null(obj), T, uj::f0(!base::is.atomic(obj), F, uj::f0(base::length(obj) == 0, T, base::all(base::is.na(obj)))))
    obj.chr <- base::is.character(obj); dot.chr <- base::is.character(dot)
    obj.num <- base::is.numeric(obj); dot.num <- base::is.numeric(dot)
    obj.lgl <- base::is.logical(obj); dot.lgl <- base::is.logical(dot)
    obj.ord <- base::is.ordered(obj); dot.ord <- base::is.ordered(dot)
    obj.levs <- uj::f0(!obj.ord, NULL, base::levels(obj)); obj.nlevs <- base::length(obj.levs)
    dot.levs <- uj::f0(!dot.ord, NULL, base::levels(dot)); dot.nlevs <- lbase::ength(dot.levs)
    uj::f0(base::is.null(dot), T,
       uj::f0(!ok_vec(dot), F,
          uj::f0(!dot.num & !dot.lgl & !dot.chr & !dot.ord, F,
             uj::f0(!base::is.atomic(obj), T,
                uj::f0(obj.wild, T,
                   uj::f0(obj.chr & dot.chr, T,
                      uj::f0(obj.num & dot.num, T,
                         uj::f0(obj.lgl & dot.lgl, T,
                            uj::f0(!obj.ord | !dot.ord, F,
                               uj::f0(obj.nlevs != dot.nlevs, F, base::all(obj.levs == dot.levs)))))))))))
  }
  ok_srt_scl <- function(obj, dot) {uj::f0(base::is.null(dot), T, uj::f0(!ok_srt_vec(obj, dot), F, base::length(dot) == 1))}
  lt <- dots$lt; min  <- dots$min ; max  <- dots$max ; n  <- dots$n
  le <- dots$le; minr <- dots$minr; maxr <- dots$maxr; nr <- dots$nr
  ge <- dots$ge; minc <- dots$minc; maxc <- dots$maxc; nc <- dots$nc
  gt <- dots$gt; vals <- dots$vals
  base::c(uj::f0(ok_ind_vec(n      ), NULL, "\n \u2022 [n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
          uj::f0(ok_ind_vec(nr     ), NULL, "\n \u2022 [nr] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
          uj::f0(ok_ind_vec(nc     ), NULL, "\n \u2022 [nc] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
          uj::f0(ok_ind_scl(min    ), NULL, "\n \u2022 [min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
          uj::f0(ok_ind_scl(max    ), NULL, "\n \u2022 [max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
          uj::f0(ok_ind_scl(minr   ), NULL, "\n \u2022 [minr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
          uj::f0(ok_ind_scl(minc   ), NULL, "\n \u2022 [minc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
          uj::f0(ok_ind_scl(maxr   ), NULL, "\n \u2022 [maxr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
          uj::f0(ok_ind_scl(maxc   ), NULL, "\n \u2022 [maxc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
          uj::f0(ok_srt_scl(x, lt  ), NULL, "\n \u2022 [lt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
          uj::f0(ok_srt_scl(x, le  ), NULL, "\n \u2022 [le] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
          uj::f0(ok_srt_scl(x, ge  ), NULL, "\n \u2022 [ge] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
          uj::f0(ok_srt_scl(x, gt  ), NULL, "\n \u2022 [gt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
          uj::f0(ok_srt_vec(x, vals), NULL, "\n \u2022 [vals] must be NULL or a complete atomic vec (?cmp_vec) comparable with [x] (?comparable)."))
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
#' \tabular{rl}{
#'       `max, maxr, maxc`   \tab Scalar maximum valid numbers of element, rows, and columns, respectively.
#'   \cr                     \tab  
#'   \cr `min, minr, minc`   \tab Scalar minimum valid numbers of element, rows, and columns, respectively.
#'   \cr                     \tab  
#'   \cr  `lt, le, ge, gt`   \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively.
#'   \cr                     \tab  
#'   \cr       `n, nr, nc`   \tab A vector of valid numbers of elements, rows, and columns, respectively.
#'   \cr                     \tab  
#'   \cr            `vals`   \tab A vector of valid values.
#' }
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
  if (base::...length() == 0) {return(T)}
  errs <- uj:::.meets_errs(x, ...)
  if (!uj::inll(errs)) {stop(errs)}
  atoms <- uj::av(x)                                                                 # atomic values from {x}
  nx <- uj::f0(uj::id1D(x), base::length(x), base::prod(base::dim(x)))                   # length of {x}
  nr <- base::NROW(x)                                                                  # number of rows in {x}
  nc <- base::NCOL(x)                                                                  # number of columns in {x}
  atoms <- atoms[!base::is.na(atoms)]                                                  # remove na values of {x}
  d <- base::list(...)
  if (base::length(atoms) == 0) {return(T)}                                            # if 0-length and has passed validation, meets requirements
  if (!base::is.null(d$n   )) {if (   !(nx %in% d$n   )) {return(F)}}                  # check for not meeting element count requirements
  if (!base::is.null(d$min )) {if (   !(nx  >=  d$min )) {return(F)}}
  if (!base::is.null(d$max )) {if (   !(nx  <=  d$max )) {return(F)}}
  if (!base::is.null(d$nr  )) {if (   !(nr %in% d$nr  )) {return(F)}}                  # check for not meeting row count requirements
  if (!base::is.null(d$minr)) {if (   !(nr  >=  d$minr)) {return(F)}}
  if (!base::is.null(d$maxr)) {if (   !(nr  <=  d$maxr)) {return(F)}}
  if (!base::is.null(d$nc  )) {if (   !(nc %in% d$nc  )) {return(F)}}                  # check for not meeting column count requirements
  if (!base::is.null(d$minc)) {if (   !(nc  >=  d$minc)) {return(F)}}
  if (!base::is.null(d$maxc)) {if (   !(nc  <=  d$maxc)) {return(F)}}
  if (!base::is.null(d$lt  )) {if ( base::any(atoms  >=  d$lt  )) {return(F)}}               # check for not meeting value requirements
  if (!base::is.null(d$gt  )) {if ( base::any(atoms  <=  d$gt  )) {return(F)}}
  if (!base::is.null(d$le  )) {if ( base::any(atoms  >   d$le  )) {return(F)}}
  if (!base::is.null(d$ge  )) {if ( base::any(atoms  <   d$ge  )) {return(F)}}
  if (!base::is.null(d$vals)) {if (!base::all(atoms %in% d$vals)) {return(F)}}
  T                                                                              # meets all requirements
}
