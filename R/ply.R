#' @encoding UTF-8
#' @family meta
#' @title Variations on `apply` functions
#' @description Apply a function over elements, rows, columns, or all dimensions; to an atomic object, a \link[=MVC]{multivec}, \link[=VEC]{vec}, \link[=VLS]{vlist}, matrix, or data.frame; and/or check the number of `TRUE` values in the result.
#' @details
#' \tabular{ll}{  `none_ply`   \tab Checks for `0` resulting `TRUE` values\eqn{^{(1)}}.                                \cr
#'                `one_ply`    \tab Checks for `1` resulting `TRUE` values\eqn{^{(1)}}.                                \cr
#'                `two_ply`    \tab Checks for `2+` resulting `TRUE` values\eqn{^{(1)}}.                               \cr
#'                `any_ply`    \tab Checks for `1+` resulting `TRUE` values\eqn{^{(1)}}.                               \cr
#'                `all_ply`    \tab Checks for *only* resulting `TRUE` values\eqn{^{(1)}}.                             \cr   \tab   \cr
#'                `atm_ply`    \tab Applies `FUN` to \link[=av]{atomized} `x`.                                         \cr
#'                `mvc_ply`    \tab Applies `FUN` to elements of \link[=atm_mvc]{atomic multivec} `x`.                 \cr
#'                `vec_ply`    \tab Applies `FUN` to elements of \link[=atm_vec]{atomic vec} `x`.                      \cr
#'                `vls_ply`    \tab Applies `FUN` to elements of \link[=atm_vls]{atomic vlist} `x`.                    \cr   \tab   \cr
#'                `row_ply`    \tab Applies `FUN` to rows of `x`.                                                      \cr
#'                `col_ply`    \tab Applies `FUN` to columns of `x`.                                                   \cr
#'                `dim_ply`    \tab Applies `FUN` to cells of `x`.                                                     \cr   \tab   \cr
#'                `ply`        \tab Generalized `ply` function.                                                        \cr
#'                             \tab \eqn{^{(1)}} These functions assume that applying `FUN` produces `'logical'` results. }
#' @section The `PROC` argument: When not `NULL`, the `PROC` argument is an optional list with up to seven named elements, which give processing instructions as follows:
#' \tabular{ll}{  **Name and value**   \tab **Processing instructions**
#'                `$arg = '{spec}'`    \tab Checks `x` for match to \link[=ppp]{property spec} `'{spec}'`\eqn{^{(1)}}. \cr
#'                `$out = '{spec}'`    \tab Checks result for match to property spec `'{spec}'`.\eqn{^{(1)}}           \cr   \tab   \cr
#'                `$agg = 'none'`      \tab Inspect result for `0` `TRUE` values.                                      \cr
#'                `$agg = 'one'`       \tab Inspect result for `1` `TRUE` values.                                      \cr
#'                `$agg = 'two'`       \tab Inspect result for `2+` `TRUE` values.                                     \cr
#'                `$agg = 'any'`       \tab Inspect result for *any* `TRUE` values.                                    \cr
#'                `$agg = 'all'`       \tab Inspect result for *only* `TRUE` values.                                   \cr   \tab     }
#' \tabular{ll}{  `$na = 'err'`        \tab Throw error if result has any `NA` values.                                 \cr
#'                `$na = FALSE`        \tab Replace resulting `NA`s with `FALSE`.                                      \cr
#'                `$na = TRUE`         \tab Replace resulting `NA`s with `TRUE`.                                       \cr   \tab   \cr
#'                `$a1 = TRUE`         \tab \link[=av]{Atomize} `x`.                                                   \cr
#'                `$a2 = TRUE`         \tab Atomize the result.                                                        \cr   \tab     }
#' \tabular{ll}{  `$s = TRUE`          \tab \link[base:simplify2array]{Simplify} the result.                           \cr
#'                                     \tab \eqn{^{(1)}} `{spec}` is a placeholder for a \link[=is_prop_spec]{valid property spec}.   }
#'
#' @param x An object to apply `FUN` to.
#' @param FUN Function or character scalar name of a function to apply to `x`.
#' @param ... An arbitrary number of additional arguments to be passed to the function `FUN`.
#' @param DIM A \link[=cmp_nnw_vec]{complete non-negative whole-number vec} giving dimension(s) of `x` to apply the function `FUN` to (`0` indicates applying to elements of a vector or \link[=VLS]{vlist} vs. applying to every cell for arrays and data.frames).
#' @param PROC Either `NULL` or a list of named elements with processing instructions. See *the* `PROC` *argument*.
#' @examples
#' egNumVec <- 1:5
#' egNumMat <- matrix(1:25, nrow = 5)
#' egChrDtf <- dtf(az = letters[1:10], AZ = LETTERS[1:10], nm = as.character(0:9))
#' egChrVls <- list(az = letters, AZ = LETTERS, nm = as.character(0:9))
#' egComp <- function(x, comp, value) {if (comp == "<") {x < value} else {x > value}}
#'
#' egNumVec
#' egNumMat
#' egChrDtf
#' egChrVls
#'
#' none_ply(egNumVec, egComp, ">", value = 6)
#' none_ply(egNumVec, egComp, ">", value = 4)
#' any_ply(egNumVec, egComp, ">", value = 6)
#' any_ply(egNumVec, egComp, ">", value = 4)
#' all_ply(egNumVec, egComp, "<", value = 6)
#' all_ply(egNumVec, egComp, "<", value = 4)
#' one_ply(egNumVec, egComp, ">", value = 6)
#' one_ply(egNumVec, egComp, ">", value = 4)
#' two_ply(egNumVec, egComp, ">", value = 2)
#' two_ply(egNumVec, egComp, ">", value = 5)
#' two_ply(egNumMat, egComp, ">", value = 15, DIM = 2)
#' dim_ply(egNumMat, egComp, ">", value = 15)
#' dim_ply(egChrDtf, toupper)
#' row_ply(egNumMat, sum)
#' col_ply(egChrDtf, paste0, collapse = "")
#' vls_ply(egChrVls, paste0, collapse = "")
#' @export
ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  if (uj::N0(x)) {uj::stopper("[x] is empty.", PKG = "uj")}
  vnames <- base::c("a1", "a2", "s", "na", "arg", "out", "agg")
  vaggs <- base::c("none", "any", "all", "one", "two")
  names <- uj::EN(PROC)
  procn <- uj::N(PROC)
  namen <- uj::N(names)
  namev <- uj::f0(procn == 0, T, procn == namen & uj::allIN(names, vnames))
  d0 <- uj::isEQ1(DIM, 0)
  s <- uj::f0(uj::DEF(PROC$s), PROC$s, F)
  a1 <- uj::f0(uj::DEF(PROC$a1), PROC$a1, F)
  a2 <- uj::f0(uj::DEF(PROC$a1), PROC$a2, F)
  na <- uj::f0(uj::DEF(PROC$na), PROC$na, F)
  agg <- PROC$agg
  arg <- PROC$arg
  out <- PROC$out
  ok.dim <- d0 | uj::cmp_psw_vec(DIM)
  ok.xdim <- uj::f0(d0 | !ok.dim, T, uj::allIN(DIM, 1:uj::NDIM(x)))
  ok.proc <- uj::f0(uj::NLL(PROC), T, uj::isLST(PROC) & namev)
  ok.na <- uj::f0(uj::isLG1(na), T, uj::isEQ1(na, "err"))
  ok.agg <- uj::f0(uj::NLL(agg), T, uj::isIN1(agg, vaggs))
  ok.arg <- uj::f0(uj::NLL(arg), T, uj::is_prop_spec(arg))
  ok.out <- uj::f0(uj::NLL(out), T, uj::is_prop_spec(out))
  uj::errs_if_nots(uj::FUN(FUN) , "[FUN] is not a function or the name of function."                                                 ,
                   ok.dim       , "[DIM] must be 0 or a complete positive whole-number vec (?cmp_psw_vec)."                            ,
                   ok.xdim      , "[DIM] contains a value larger than the number of defined dimensions of [x]."                      ,
                   ok.proc      , "Elements of [PROC] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out').",
                   ok.na        , "When supplied, [PROC$na] must be TRUE, FALSE, NA, or 'err'."                                      ,
                   ok.agg       , "When supplied, [PROC$agg] must be 'none', 'any', 'all', 'one', or 'two'."                         ,
                   ok.arg       , "When supplied, [PROC$arg] must be a valid property specification as validated by isVALIDspec(.)." ,
                   ok.out       , "When supplied, [PROC$out] must be a valid property specification as validated by isVALIDspec(.)." ,
                   uj::isTF1(s) , "When supplied, [PROC$s] must be TRUE or FALSE."                                                   ,
                   uj::isTF1(a1), "When supplied, [PROC$a1] must be TRUE or FALSE."                                                  ,
                   uj::isTF1(a2), "When supplied, [PROC$a2] must be TRUE or FALSE."                                                  , PKG = "uj" )
  if (a1) {x <- uj::av(x)}
  uj::err_if_not(uj::f0(uj::DEF(arg), uj::PPP(x, arg), F), "[x] does not match [PROC$arg = '", arg, "'].", PKG = "uj")
  if (uj::isEQ1(DIM, 0)) {x <- uj::f0(uj::isARR(x) | uj::DTF(x), base::apply(x, 1:uj::N(base::dim(x)), FUN, ...), uj::f0(uj::NLL(x) | uj::VEC(x), base::sapply(x, FUN, ...), uj::f0(uj::VLS(x), base::lapply(x, FUN, ...), x)))}
  else {x <- base::apply(x, DIM, FUN, ...)}
  if (s) {x <- base::simplify2array(x)}
  if (a2) {x <- uj::av(x)}
  uj::err_if_not(uj::f0(uj::DEF(out), uj::PPP(x, out), T), "[x] does not match [PROC$out = '", out, "'].", PKG = "uj")
  if (uj::DEF(agg)) {
    err <- uj::isEQ1(na, 'err')
    uj::errs_if_nots(uj::LGL(x)          , "[PROC$agg] is not NULL, but results of applying [FUN] are not of mode logical.",
                     err | uj::noneNAS(x), "Applying [FUN] produced NA values, but [PROC$na = 'err']."                     , PKG = "uj" )
    if (!err) {x[uj::na(x)] <- na}
    x <- uj::f0(agg == "none", uj::noneT(x), uj::f0(agg == "any", uj::anyT(x), uj::f0(agg == "all", uj::allT(x), uj::f0(agg == "one", uj::oneT(x), uj::f0(agg == "two", uj::twoT(x), x)))))
  }
  x
}

#' @rdname ply
#' @export
none_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "none"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
any_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "any"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
all_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "all"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
one_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "one"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
two_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "two"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
atm_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$a1 <- TRUE
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}

#' @rdname ply
#' @export
mvc_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'mvc'
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}

#' @rdname ply
#' @export
vec_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'vec'
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}

#' @rdname ply
#' @export
row_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'd2d'
  uj::ply(x, FUN, ..., DIM = 1, PROC = PROC)
}

#' @rdname ply
#' @export
col_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'd2d'
  uj::ply(x, FUN, ..., DIM = 2, PROC = PROC)
}

#' @rdname ply
#' @export
dim_ply <- function(x, FUN, ..., PROC = NULL) {
  d <- uj::f0(uj::nDDD(x) < 2, 0, 1:uj::NDIM(x))
  uj::ply(x, FUN, ..., DIM = d, PROC = PROC)
}

#' @rdname ply
#' @export
vls_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'pop_vls'
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}
