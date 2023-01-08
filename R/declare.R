# internal ####

.labs_ok <- function(x) {
  alpha <- base::c(letters, LETTERS)
  nums <- base::as.character(0:9)
  ok <- base::substr(x, 1, 1) %in% alpha
  ok[!ok] <- base::substr(x[!ok], 1, 1) == "." & base::substr(x[!ok], 2, 2) %in% alpha
  if (base::all(ok)) {
    ok <- ok & base::substr(x, base::nchar(x), base::nchar(x)) %in% base::c(alpha, nums)
    uj::f0(base::all(ok), base::all(uj::av(base::strsplit(x, "", fixed = T)) %in% base::c(alpha, nums, ".", "_")), F)
  } else {F}
}

# exported ####

#' @name declare
#' @encoding UTF-8
#' @family extensions
#' @title Declare basic R objects with extended functionality
#' @description This family of functions enforces the restrictions that names must (a) contain only ASCII letters, numerals, `'.'`, and `'_'`; (b) begin with a letter or `'.'` followed by a letter; and (c) end with a letter or numeral.
#' \cr
#' \cr Functions declaring \link[=atm_dtf]{atomic data.frames}:\tabular{rl}{
#'            `dtf` \tab   Declares a data.frame.
#'   \cr     `dtf.` \tab   Declares a data.frame concisely\eqn{^a}.
#'   \cr     `dtf0` \tab   Declares a `0`-row data.frame.
#'   \cr   `dtf_na` \tab   Declares a data.frame of `NA` values.
#' }
#' Functions declaring matrices:\tabular{rl}{
#'          `mat` \tab   Declares a matrix.
#'   \cr   `matd` \tab   Declares a square diagonal matrix\eqn{^b}.
#' }
#' Functions declaring atomic vectors:\tabular{rl}{
#'            `vec` \tab   Declares a vector.
#'   \cr     `vec.` \tab   Declares a named atomic vector concisely\eqn{^c}.
#'   \cr   `vec_na` \tab   Declares a vector of `NA` values.
#' }
#' Functions declaring \link[=ivls]{vlists}:\tabular{rl}{
#'          `vls` \tab   Declares a \link[=ivls]{vlist}.
#'   \cr   `vls.` \tab   Declares a named \link[=ivls]{vlist} concisely\eqn{^d}.
#' }
#'    \eqn{^{a.}} `dtf.(a, b)` is identical to `data.frame(a = a, b = b, stringsAsFactors = F)`.
#' \cr
#' \cr    \eqn{^{b.}} Off-diags are `0`, `FALSE`, `""` for `x` of mode `'numeric'`, `'logical'`, `'character'`, respectively.
#' \cr
#' \cr    \eqn{^{c.}} `vec.(a, b)` is identical to `c(a = a, b = b)`.
#' \cr
#' \cr    \eqn{^{d.}} `vls.(a, b)` is identical to `list(a = a, b = b)`.
#' @param ... Objects to placed in an atomic vec, atomic matrix, atomic data.frame, vlist, or square atomic diagonal matrix. \link[=a]{Atomized} for vec and matrix creation.
#' @param x A non-empty vector of mode `'numeric'`, `'character'`, or `'logical'`.
#' @param r A non-`NA` numeric scalar number of replications.
#' @param rn Possibly pipe-delimited complete character vec of matrix row names.
#' @param cn Possibly pipe-delimited complete character vec of matrix column names.
#' @param nr Complete positive whole-number scalars giving number of matrix rows.
#' @param nc Complete positive whole-number scalars giving number of matrix columns.
#' @param br A non-`NA` logical scalar indicating whether to fill matrices by row.
#' @param en. A possibly pipe-delimited, \link[=cmp_chr_vec]{complete character vec} of names to apply to vector or list elements
#' @param cn. A possibly pipe-delimited complete character vec of column names (respectively) for a data.frame.
#' @return *An atomic data.frame*
#'  \cr   `dtf`
#'  \cr   `dtf.`
#'  \cr   `dtf0`
#'  \cr   `dtf_na`
#'  \cr
#'  \cr *An atomic vector*
#'  \cr   `vec`
#'  \cr   `vec.`
#'  \cr   `vec_na`
#'  \cr
#'  \cr *A square matrix*
#'  \cr   `dmat`
#'  \cr
#'  \cr *A matrix*
#'  \cr   `mat`
#'  \cr
#'  \cr *A \link[=ivls]{vlist}*
#'  \cr   `vls.`
#'  \cr   `vls`
#' @examples
#' a <- "a"
#' b <- "b"
#' c <- "c"
#' na <- NA
#' abc <- c(a, b, c)
#' nums <- 0:9
#' NUMS <- 1:9
#' labs1 <- c("abc", "nums", "na")
#' labs2 <- c("abc", "NUMS", "na")
#'
#' vec(a, b, c)
#' vec(a, b, c, en. = abc)
#' vec(a = a, b = b, c = c)
#' vec(a, b, c, en. = "a|b|c")
#' vec.(a, b, c)
#'
#' mat(1:9)
#' mat(1:9, nr = 3)
#' mat(1:9, nc = 3)
#' mat(1:9, nr = 3, rn = "a|b|c", cn = abc, br = TRUE)
#'
#' vls(abc, nums, na)
#' vls(abc, nums, na, en. = labs1)
#' vls(abc = abc, nums = nums, na = na)
#' vls(abc, nums, na, en. = "abc|nums|na")
#' vls.(abc, nums, na)
#'
#' dtf(abc, NUMS, na, cn. = labs2)
#' dtf(abc = abc, NUMS = NUMS, na = na)
#' dtf(abc, NUMS, na, cn. = "abc|NUMS|na")
#' dtf.(abc, NUMS, na)
#'
#' vec_na(3)
#'
#' dmat(abc)
#' dmat(1, 3)
#' dmat(TRUE, 3)
#'
#' dtf0(abc)
#' dtf0("a|b|c")
#' dtf_na(abc, 3)
#' @export
dtf <- function(..., cn. = NULL) {
  labs1 <- uj::f0(base::is.null(cn.), NULL, uj::f0(uj::cmp_chr_scl(cn.), uj::av(base::strsplit(cn., "|", fixed = T)), cn.))
  labs2 <- uj::f0(base::is.null(base::...names()), NULL, base::...names())
  labs <- uj::f0(!base::is.null(labs1), labs1, labs2)
  x <- base::list(...)
  n <- base::length(x)
  errs <- NULL
  ok.0 <- n > 0
  ok.lb1 <- uj::f0(base::is.null(labs1), T, base::length(labs) == n & !base::any(labs == ""))
  ok.lb2 <- uj::f0(base::is.null(labs2), T, !base::any(labs2 == ""))
  ok.lbs <- uj::f0(!ok.lb1 | !ok.lb2, T, uj:::.labs_ok(labs))
  if (!(ok.0 & ok.lb1 & ok.lb2 & ok.lbs)) {
    stop(.errs(base::c(uj::f0(ok.0  , NULL, "[...] is empty."),
                             uj::f0(ok.lb1, NULL, "the number of column names in [cn.] must equal ...length() when [cn.] is not NULL."),
                             uj::f0(ok.lb2, NULL, "[...] arguments must be named when [cn. = NULL]."),
                             uj::f0(ok.lbs, NULL, "column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))))
  }
  ns <- base::lengths(x)
  mx <- base::max(ns)
  ok1 <- base::all(base::sapply(x, uj::atm_vec))
  ok2 <- base::all(ns > 0)
  ok3 <- uj::f0(!ok1, T, base::all(mx / ns == base::round(mx / ns)))
  ok4 <- base::length(labs) == base::length(base::unique(labs))
  errs <- base::c(uj::f0(ok1, NULL, "[...] arguments must be atomic vecs (?atm_vec)."),
                  uj::f0(ok2, NULL, "[...] arguments may not be of length 0."        ),
                  uj::f0(ok3, NULL, "[...] arguments are not recyclable."            ),
                  uj::f0(ok4, NULL, "column names must be unique."                   ))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x <- base::data.frame(..., stringsAsFactors = FALSE)
  base::colnames(x) <- labs
  x
}

#' @rdname declare
#' @export
dtf. <- function(...) {
  labs <- uj::as_chr(base::match.call())                                                   # get a function call object and convert to character
  labs <- labs[2:base::length(labs)]                                                   # remove the function call leaving the variables as named in the calling function
  nok <- base::...length() > 0
  avc <- uj::f0(!nok, T, base::all(base::sapply(base::list(...), uj::atm_vec)))
  unq <- uj::f0(!nok, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  if (nok & avc) {
    ns <- base::lengths(base::list(...))
    nr <- base::max(ns) / ns
    rec <- base::all(nr == base::round(nr))
  } else {rec <- TRUE}
  errs <- base::c(uj::f0(nok, NULL, "[...] is empty."),
                  uj::f0(unq, NULL, "[...] arguments must be uniquely named."),
                  uj::f0(avc, NULL, "[...] arguments must be atomic vecs (?atm_vec)."),
                  uj::f0(rec, NULL, "[...] arguments are not recyclable."),
                  uj::f0(lab, NULL, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  code <- base::paste0("base::data.frame(", base::paste0(base::paste(labs, "=", labs), collapse = ", "), ", stringsAsFactors = FALSE)")
  base::eval.parent(base::parse(text = code, n = 1))                                         # evaluate the call in the environment of the calling function
}

#' @rdname declare
#' @export
dtf0 <- function(cn) {
  if (!uj::cmp_chr_vec(cn)) {stop(uj:::.errs("[cn] must be a complete character vector (?cmp_chr_vec)."))}
  cn <- uj::av(base::strsplit(cn, "|", fixed = T))
  if (!uj:::.labs_ok(cn)) {stop(uj:::.errs("column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}
  cmd <- base::paste0("tibble::tibble(", base::paste0(base::paste0(cn, " = NA"), collapse = ", "), ", .rows = 0)")
  uj::run(cmd)
}

#' @rdname declare
#' @export
dtf_na <- function(cn, nr) {
  errs <- base::c(uj::f0(uj::cmp_chr_vec(cn), NULL, "[cn] must be a complete character vector (?cmp_chr_vec)."),
                  uj::f0(uj::cmp_psw_scl(nr), NULL, "[nr] must be NULL or a positive whole number scalar (?cmp_psw_scl)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  cn <- uj::av(base::strsplit(cn, "|", fixed = T))
  if (!uj:::.labs_ok(cn)) {stop(.errs("column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}
  cmd <- base::paste0("base::data.frame(", base::paste0(base::paste0(cn, " = base::rep.int(NA, nr)"), collapse = ", "), ", stringsAsFactor = FALSE)")
  uj::run(cmd)
}


#' @rdname declare
#' @export
mat <- function(..., r = 1, nr = NULL, nc = NULL, br = F, rn = NULL, cn = NULL) {
  x <- av(...)
  if (base::length(x) == 0) {x <- NA}
  errs <- base::c(uj::f0(cmp_psw_scl(r)                         , NULL, "[r] must be a positive whole number scalar (?cmp_psw_scl)."),
                  uj::f0(base::is.null(nr) | uj::cmp_nnw_scl(nr), NULL, "[nr] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl)."),
                  uj::f0(base::is.null(nc) | uj::cmp_nnw_scl(nc), NULL, "[nc] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl)."),
                  uj::f0(base::isTRUE(br)  | base::isFALSE(br)  , NULL, "[br] must be TRUE or FALSE."),
                  uj::f0(base::is.null(rn) | uj::cmp_chr_vec(rn), NULL, "[rn] must be NULL or a complete character vector (?cmp_chr_vec)."),
                  uj::f0(base::is.null(cn) | uj::cmp_chr_vec(cn), NULL, "[cn] must be NULL or a complete character vector (?cmp_chr_vec)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x <- base::rep.int(x, r)
  if (base::length(x) == 1) {x <- base::rep.int(x, nr * nc)}
  if (base::is.null(nr) & base::is.null(nc)) {nr <- 1; nc <- base::length(x)}
  else if (base::is.null(nr)) {nr <- base::length(x) / nc}
  else if (base::is.null(nc)) {nc <- base::length(x) / nr}
  if (nr * nc != base::length(x) | base::round(nr) != nr | base::round(nc) != nc) {stop(uj:::.errs("[r * length(av(...))] is not divisible by [nr] and/or [nc]."))}
  rn <- uj::f0(base::is.null(rn), NULL, uj::av(base::strsplit(rn, "|", fixed = T)))
  cn <- uj::f0(base::is.null(cn), NULL, uj::av(base::strsplit(cn, "|", fixed = T)))
  ok.rn <- uj::f0(base::is.null(rn), T, base::length(rn) == nr)
  ok.cn <- uj::f0(base::is.null(cn), T, base::length(cn) == nc)
  if (!ok.rn | !ok.cn) {
    stop(base::c(uj::f0(ok.rn, NULL, "The number of rownames in [rn] does not match the number of rows."),
                       uj::f0(ok.cn, NULL, "The number of colnames in [cn] does not match the number of cols.")))
  }
  errs <- NULL
  if (!base::is.null(rn)) {if (!uj:::.labs_ok(rn)) {errs <- base::c(errs, "row names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}}
  if (!base::is.null(cn)) {if (!uj:::.labs_ok(cn)) {errs <- base::c(errs, "col names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}}
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x <- base::matrix(x, nrow = nr, ncol = nc, byrow = br)
  base::rownames(x) <- rn
  base::colnames(x) <- cn
  x
}

#' @rdname declare
#' @export
matd <- function(x = 1, r = 1) {
  ok.x <- uj::inum(x) | uj::ilgl(x) | uj::ichr(x)
  ok.r <- uj::cmp_psw_scl(r)
  ok.ge1 <- uj::f0(base::length(x) > 1, T, uj::f0(ok.r, T, r > 1))
  errs <- base::c(uj::f0(ok.x  , NULL, "[x] must be an numeric, logical, or character."),
                  uj::f0(ok.r  , NULL, "[r] must a positive whole-number scalar (?cmp_psw_scl)."),
                  uj::f0(ok.ge1, NULL, "Neither [length(x)] nor [r] is greater than 1."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  nr <- base::nrow(x)
  nc <- base::ncol(x)
  if (base::is.matrix(x)) {if (nr > 0 & nr == nc) {x <- base::diag(x, nr, nc)}}
  x <- uj::av(x)
  if (r > 1) {x <- base::rep(x, r)}
  if (base::all(base::is.na(x))) {x[base::is.na(x)] <- NA_real_}
  n <- base::length(x)
  blank <- uj::f0(uj::inum(x), 0, uj::f0(uj::ilgl(x), F, uj::f0(uj::ichr(x), "", NA)))
  out <- base::matrix(blank, nrow = n, ncol = n)
  base::diag(out) <- x
  out
}

#' @rdname declare
#' @export
vec <- function(..., r. = 1, en. = NULL) {
  x <- uj::av(...)
  if (base::length(x) == 0 | uj::isEQ(r, 0)) {return(base::vector())}
  errs <- base::c(uj::f0(uj::cmp_nnw_scl(r. ), NULL, "[r.] must be a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(base::is.null(en.) | uj::cmp_chr_vec(en.), NULL, "[en.] must be NULL or a complete character vec (?cmp_chr_vec)." ))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (r. > 1) {x <- base::rep.int(x, r.)}
  if (uj::idef(en.)) {
    en. <- uj::av(base::strsplit(en., "|", fixed = T))
    if (base::length(x) != base::length(en.)) {stop(uj:::.errs("[en.] must be the same length as the vector resulting from atomizing (?av) [...]."))}
    if (base::any(en. == "")) {stop(uj:::.errs("[en.] contains a blank string (after splitting along pipes)."))}
    if (!uj:::.labs_ok(en.)) {stop(uj:::.errs("[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}
    base::names(x) <- en.
  }
  x
}

#' @rdname declare
#' @export
vec. <- function(...) {
  labs <- uj::as_chr(base::match.call())                                                   # get a function call object and convert to character
  labs <- labs[2:uj::nx(labs)]                                                       # remove the function call leaving the variables as named in the calling function
  nok <- base::...length() > 0
  asc <- uj::f0(!nok, T, base::all(base::sapply(base::list(...), uj::atm_scl)))
  unq <- uj::f0(!nok, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  errs <- base::c(uj::f0(nok, NULL, "[...] is empty."),
                  uj::f0(unq, NULL, "[...] argument names must be uniquely."),
                  uj::f0(asc, NULL, "[...] arguments must be atomic and scalar (?atm_scl)."),
                  uj::f0(lab, NULL, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  code <- base::paste0("base::c(", base::paste0(base::paste(labs, "=", labs), collapse = ", "), ")")    # create the call [c(<var1> = <var1>, <var2> = <var2>, ...)]
  base::eval.parent(base::parse(text = code, n = 1))                                         # and evaluate it in the environment of the calling function
}

#' @rdname declare
#' @export
vec_na <- function(r) {base::rep.int(NA, r)}

#' @rdname declare
#' @export
vls <- function(..., en. = NULL) {
  en. <- uj::f0(uj::cmp_chr_vec(en.), uj::av(base::strsplit(en., "|", fixed = T)), en.)
  dots <- base::list(...)
  n.dots <- base::length(dots)
  ok.en <- uj::f0(uj::inll(en.), T, uj::f0(!uj::cmp_chr_vec(en.), F, base::length(en.) == n.dots))
  errs <- base::c(uj::f0(n.dots > 0, NULL, "[...] is empty."),
                  uj::f0(ok.en     , NULL, "[en.] must be NULL or match the number of arguments in [...]."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (!base::is.null(en.)) {if (!uj:::.labs_ok(en.)) {stop(uj:::.errs("element names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}}
  base::names(dots) <- en.
  dots
}

#' @rdname declare
#' @export
vls. <- function(...) {
  labs <- uj::as_chr(base::match.call())                                                   # get a function call object and convert to character
  labs <- labs[2:uj::nx(labs)]                                                       # remove the function call leaving the variables as named in the calling function
  nok <- base::...length() > 0
  unq <- uj::f0(!nok, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  errs <- base::c(uj::f0(nok, NULL, "[...] is empty."),
                  uj::f0(unq, NULL, "[...] arguments must be uniquely named."),
                  uj::f0(lab, NULL, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  code <- base::paste0("list(", base::paste0(base::paste(labs, "=", labs), collapse = ", "), ")")  # create the call [list(<var1> = <var1>, <var2> = <var2>, ...)]
  base::eval.parent(base::parse(text = code, n = 1))                                         # and evaluate it in the environment of the calling function
}
