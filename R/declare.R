#' @name declare
#' @encoding UTF-8
#' @family extensions
#' @title Declare basic R objects with extended functionality
#' @description This family of functions enforces the restrictions that names must (a) contain only ASCII letters, numerals, `'.'`, and `'_'`; (b) begin with a letter or `'.'` followed by a letter; and (c) end with a letter or numeral.
#' \cr\cr **Functions declaring \link[=atm_dtf]{atomic data.frames}:**
#' \tabular{ll}{  `dtf`     \tab Declares a `data.frame.`                                                                                          \cr   \tab   \cr
#'                `dtf.`    \tab Declares a `data.frame` concisely: `dtf.(a, b)` is identical to `data.frame(a = a, b = b, stringsAsFactors = F)`. \cr   \tab   \cr
#'                `dtf0`    \tab Declares a `0`-row `data.frame.`                                                                                  \cr   \tab   \cr
#'                `dtfNA`   \tab Declares a `data.frame` of `NA` values.                                                                                          }
#' \cr\cr **Functions declaring matrices:**
#' \tabular{ll}{  `mat`     \tab Declares a matrix.                                                                                                                  \cr   \tab   \cr
#'                `matd`    \tab Declares a square diagonal matrix: off-diags are `0`, `FALSE`, and `""` for `x` of mode `'numeric'`, `'logical'`, and `'character'`, respectively. }
#' \cr\cr **Functions declaring atomic vectors:**
#' \tabular{ll}{  `vec`     \tab Declares a vector.                                                                       \cr   \tab   \cr
#'                `vec.`    \tab Declares a named atomic vector concisely: `vec.(a, b)` is identical to `c(a = a, b = b)` \cr   \tab   \cr
#'                `vecNA`   \tab Declares a vector of `NA` values.                                                                       }
#' \cr\cr **Functions declaring \link[=iVLS]{vlists}:**
#' \tabular{ll}{  `vls`     \tab Declares a \link[=VLS]{vlist}.                                                     \cr   \tab   \cr
#'                `vls.`    \tab Declares a named \link[=VLS]{vlist} concisely: `vls.(a, b)` is identical to `list(a = a, b = b)`. }
#' @param ... Objects to placed in an atomic vec, atomic matrix, atomic data.frame, vlist, or square atomic diagonal matrix. \link[=a]{Atomized} for vec and matrix creation.
#' @param X A non-empty vector of mode `'numeric'`, `'character'`, or `'logical'`.
#' @param R A non-`NA` numeric scalar number of replications.
#' @param RN Possibly pipe-delimited complete character vec of matrix row names.
#' @param CN Possibly pipe-delimited complete character vec of matrix column names.
#' @param NR Complete positive whole-number scalars giving number of matrix rows.
#' @param NC Complete positive whole-number scalars giving number of matrix columns.
#' @param BR `TRUE` or `FALSE` indicating whether to fill matrices by row.
#' @param VN A possibly pipe-delimited, \link[=cmp_chr_vec]{complete character vec} of names to apply to vector or list elements
#' @param CN A possibly pipe-delimited complete character vec of column names (respectively) for a data.frame.
#' @return **An atomic data.frame**  \cr\cr `dtf, dtf., dtf0, dtfNA`
#' \cr\cr  **An atomic vector**      \cr\cr `vec, vec., vecNA`
#' \cr\cr  **A \link[=VLS]{vlist}**  \cr\cr `vls, vls.`
#' \cr\cr  **A square matrix**       \cr\cr `dmat`
#' \cr\cr  **A matrix**              \cr\cr `mat`
#' @examples
#' egA <- "a"
#' egB <- "b"
#' egC <- "c"
#' egNA <- NA
#' egABC <- c(a, b, c)
#' egNums <- 0:9
#' egNUMS <- 1:9
#' egLabs1 <- c("abc", "nums", "na")
#' egLabs2 <- c("abc", "NUMS", "na")
#'
#' vec(egA, egB, egC)
#' vec(egA, egB, egC, VN = abc)
#' vec(egA, egB, egC, VN = "a|b|c")
#' vec(egA = egA, egB = egB, egC = egC)
#' vec.(egA, egB, egC)
#' vecNA(3)
#'
#' mat(1:9)
#' mat(1:9, NR = 3)
#' mat(1:9, NC = 3)
#' mat(1:9, NR = 3, RN = "a|b|c", CN = egABC, BR = TRUE)
#' dmat(egABC)
#' dmat(1, 3)
#' dmat(TRUE, 3)
#'
#' vls(egABC, egNums, egNA)
#' vls(egABC, egNums, egABC, VN = labs1)
#' vls(egABC = egABC, egNums = egNums, egNA = egNA)
#' vls(egABC, egNums, egNA, VN = "egABC|egNums|egNA")
#' vls.(egABC, egNums, egNA)
#'
#' dtf(egABC, egNUMS, egNA, CN = labs2)
#' dtf(egABC = egABC, egNUMS = egNUMS, egNA = egNA)
#' dtf(egABC, egNUMS, egNA, CN = "egABC|egNUMS|egNA")
#' dtf.(egABC, egNUMS, egNA)
#' dtf0(egABC)
#' dtf0("egA|egB|egC")
#' dtfNA(egABC, 3)
#' @export
dtf <- function(..., CN = NULL) {
  labs1 <- uj::f0(base::is.null(CN), NULL, uj::f0(uj:::.cmp_chr_scl(CN), uj::av(base::strsplit(CN, "|", fixed = T)), CN))
  labs2 <- uj::dn()
  labs <- uj::f0(!base::is.null(labs1), labs1, labs2)
  x <- base::list(...)
  n <- base::length(x)
  ok.0 <- n > 0
  ok.lb1 <- uj::f0(base::is.null(labs1), T, base::length(labs) == n & !base::any(labs == ""))
  ok.lb2 <- uj::f0(base::is.null(labs2), T, !base::any(labs2 == ""))
  ok.lbs <- uj::f0(!ok.lb1 | !ok.lb2, T, uj:::.labs_ok(labs))
  errs <- NULL
  if (!ok.0) {errs <- base::c(errs, "[...] is empty.")}
  if (!ok.lb1) {errs <- base::c(errs, "the number of column names in [CN] must equal [...length()] when [CN] is not NULL.")}
  if (!ok.lb2) {errs <- base::c(errs, "[...] arguments must be named when [CN = NULL].")}
  if (!ok.lbs) {errs <- base::c(errs, "column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  ns <- uj::ns(x)
  mx <- base::max(ns)
  ok1 <- base::all(base::sapply(x, uj:::.atm_vec))
  ok2 <- base::all(ns > 0)
  ok3 <- uj::f0(!ok1, T, base::all((mx / ns) == base::round(mx / ns)))
  ok4 <- base::length(labs) == base::length(base::unique(labs))
  if (!ok1) {errs <- base::c(errs, "[...] arguments must be atomic vecs (?atm_vec).")}
  if (!ok2) {errs <- base::c(errs, "[...] arguments may not be of length 0.")}
  if (!ok3) {errs <- base::c(errs, "[...] arguments are not recyclable.")}
  if (!ok4) {errs <- base::c(errs, "column names must be unique.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  x <- tibble::tibble(...)
  base::colnames(x) <- labs
  x
}

#' @rdname declare
#' @export
dtf. <- function(...) {
  labs <- base::as.character(base::match.call())
  labs <- labs[2:base::length(labs)]
  nok <- base::...length() > 0
  avc <- uj::f0(!nok, T, base::all(base::sapply(base::list(...), uj:::.atm_vec)))
  unq <- uj::f0(!nok, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  if (nok & avc) {
    ns <- uj::ns(base::list(...))
    nr <- base::max(ns) / ns
    rec <- base::all(nr == base::round(nr))
  } else {rec <- TRUE}
  errs <- NULL
  if (!nok) {errs <- base::c(errs, "[...] is empty.")}
  if (!unq) {errs <- base::c(errs, "[...] arguments must be uniquely named.")}
  if (!avc) {errs <- base::c(errs, "[...] arguments must be atomic vecs (?atm_vec).")}
  if (!rec) {errs <- base::c(errs, "[...] arguments are not recyclable.")}
  if (!lab) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  code <- base::paste0("tibble::tibble(", base::paste0(base::paste0(labs, "=", labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}

#' @rdname declare
#' @export
dtf0 <- function(CN) {
  if (!uj:::.cmp_chr_vec(CN)) {uj::stopperr("[CN] must be a complete character vector (?cmp_chr_vec).", PKG = "uj")}
  CN <- uj::av(base::strsplit(CN, "|", fixed = T))
  if (!uj:::.labs_ok(CN)) {uj::stopperr("column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")}
  code <- base::paste0("tibble::tibble(", base::paste0(base::paste0(CN, " = NA"), collapse = ", "), ", .rows = 0)")
  uj::run(code)
}

#' @rdname declare
#' @export
dtfNA <- function(CN, NR) {
  errs <- NULL
  if (!uj:::.cmp_chr_vec(CN)) {errs <- base::c(errs, "[CN] must be a complete character vector (?cmp_chr_vec).")}
  if (!uj:::.cmp_psw_scl(NR)) {errs <- base::c(errs, "[NR] must be NULL or a positive whole number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  CN <- uj::av(base::strsplit(CN, "|", fixed = T))
  if (!uj:::.labs_ok(CN)) {uj::stopperr("Column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")}
  code <- base::paste0("tibble::tibble(", base::paste0(base::paste0(CN, " = base::rep.int(NA, NR)"), collapse = ", "), ")")
  uj::run(code)
}


#' @rdname declare
#' @export
mat <- function(..., R = 1, NR = NULL, NC = NULL, BR = F, RN = NULL, CN = NULL) {
  X <- uj::av(...)
  if (base::length(X) == 0) {X <- NA}
  errs <- NULL
  if (!uj:::.cmp_psw_scl(R)) {errs <- base::c(errs, "[R] must be a positive whole number scalar (?cmp_psw_scl).")}
  if (!base::is.null(NR) & !uj:::.cmp_nnw_scl(NR)) {errs <- base::c(errs, "[NR] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!base::is.null(NC) & !uj:::.cmp_nnw_scl(NC)) {errs <- base::c(errs, "[NC] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!uj:::.cmp_lgl_scl(BR)) {errs <- base::c(errs, "[BR] must be TRUE or FALSE.")}
  if (!base::is.null(RN) & !uj:::.cmp_nnw_scl(RN)) {errs <- base::c(errs, "[RN] must be NULL or a complete character vector (?cmp_chr_vec).")}
  if (!base::is.null(CN) & !uj:::.cmp_nnw_scl(CN)) {errs <- base::c(errs, "[CN] must be NULL or a complete character vector (?cmp_chr_vec).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  X <- base::rep.int(X, R)
  if (base::length(X) == 1) {X <- base::rep.int(X, NR * NC)}
  if (base::is.null(NR) & base::is.null(NC)) {NR <- 1; NC <- base::length(X)}
  else if (base::is.null(NR)) {NR <- base::length(X) / NC}
  else if (base::is.null(NC)) {NC <- base::length(X) / NR}
  match.both <- NR * NC == base::length(X)
  match.one <- NR == round(NR) & NC == round(NC)
  if (!match.both | !match.one) {uj::stopperr("[R * length(av(...))] is not divisible by [NR] and/or [NC].", PKG = "uj")}
  RN <- uj::f0(base::is.null(RN), NULL, uj::av(base::strsplit(RN, "|", fixed = T)))
  CN <- uj::f0(base::is.null(CN), NULL, uj::av(base::strsplit(CN, "|", fixed = T)))
  ok.RN <- uj::f0(base::is.null(RN), T, base::length(RN) == NR)
  ok.CN <- uj::f0(base::is.null(CN), T, base::length(CN) == NC)
  if (!ok.RN) {errs <- base::c(errs, "The number of rownames in [RN] does not match the number of rows.")}
  if (!ok.CN) {errs <- base::c(errs, "The number of colnames in [CN] does not match the number of cols.")}
  if (!uj::f0(base::is.null(RN), T, uj:::.labs_ok(RN))) {errs <- base::c(errs, "row names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!uj::f0(base::is.null(CN), T, uj:::.labs_ok(CN))) {errs <- base::c(errs, "col names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  X <- base::matrix(X, nrow = NR, ncol = NC, byrow = BR)
  base::colnames(X) <- CN
  base::rownames(X) <- RN
  X
}

#' @rdname declare
#' @export
matd <- function(X = 1, R = 1) {
  ok.X <- (uj:::.NUM(X) | uj:::.LGL(X) | uj:::.CHR(X))
  ok.R <- uj:::.cmp_psw_scl(R)
  ok.GE1 <- uj::f0(!ok.X | !ok.R, T, base::length(X) > 1 | R > 1)
  errs <- NULL
  if (!ok.X) {errs <- base::c(errs, "[X] must be an numeric, logical, or character.")}
  if (!ok.R) {errs <- base::c(errs, "[R] must a positive whole-number scalar (?cmp_psw_scl).")}
  if (!ok.GE1) {errs <- base::c(errs, "Neither [length(X)] nor [R] is greater than 1.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  NR <- base::nrow(X)
  NC <- base::ncol(X)
  if (base::is.matrix(X)) {if (NR > 0 & NR == NC) {X <- base::diag(X, NR, NC)}}
  X <- uj::av(X)
  if (R > 1) {X <- base::rep(X, R)}
  if (base::all(base::is.na(X))) {X[base::is.na(X)] <- NA_real_}
  N <- base::length(X)
  bl <- uj::f0(uj:::.NUM(X), 0, uj::f0(uj:::.LGL(X), F, uj::f0(uj:::.CHR(X), "", NA)))
  y <- base::matrix(bl, nrow = N, ncol = N)
  base::diag(y) <- X
  y
}

#' @rdname declare
#' @export
vec <- function(..., R = 1, VN = NULL) {
  if (base::...length() > 0 | uj:::.cmp_psw_scl(R)) {
    X <- uj::av(...)
    errs <- NULL
    if (!uj:::.cmp_nnw_scl(R)) {errs <- base::c(errs, "[R] must be a non-negative whole-number scalar (?cmp_nnw_scl).")}
    if (!base::is.null(VN) & !uj:::.cmp_chr_vec(VN)) {errs <- base::c(errs, "[VN] must be NULL or a complete character vec (?cmp_chr_vec).")}
    if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
    if (R > 1) {X <- base::rep.int(X, R)}
    if (!base::is.null(VN)) {
      VN <- uj::av(base::strsplit(VN, "|", fixed = T))
      if (base::length(X) != base::length(VN)) {errs <- base::c(errs, "[VN] must be the same length as the vector resulting from atomizing (?av) [...].")}
      if (base::any(VN == "")) {errs <- base::c(errs, "[VN] contains a blank string (after splitting along pipes).")}
      if (!uj:::.labs_ok(VN)) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
      base::names(X) <- VN
    }
    X
  } else if (!uj:::.cmp_nnw_scl(R)) {uj::stopperr("[R] must be a non-negative whole-number scalar (?cmp_nnw_scl).", PKG = "uj")}
  else {base::vector()}
}

#' @rdname declare
#' @export
vec. <- function(...) {
  labs <- base::as.character(base::match.call())
  labs <- labs[2:base::length(labs)]
  nok <- base::...length() > 0
  asc <- uj::f0(!nok, T, base::all(base::sapply(base::list(...), uj:::.atm_scl)))
  unq <- uj::f0(!nok, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  errs <- NULL
  if (!nok) {errs <- base::c(errs, "[...] is empty.")}
  if (!unq) {errs <- base::c(errs, "[...] argument names must be uniquely.")}
  if (!asc) {errs <- base::c(errs, "[...] arguments must be atomic and scalar (?atm_scl).")}
  if (!lab) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  code <- base::paste0("base::c(", base::paste0(uj::p0(labs, "=", labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}

#' @rdname declare
#' @export
vecNA <- function(R) {base::rep.int(NA, R)}

#' @rdname declare
#' @export
vls <- function(..., VN = NULL) {
  export <- uj::f0(uj::cmp_chr_vec(VN), uj::av(base::strsplit(VN, "|", fixed = T)), VN)
  y <- base::list(...)
  n.dots <- base::length(y)
  ok.VN <- uj::f0(base::is.null(export), T, uj::f0(!uj::cmp_chr_vec(VN), F, base::length(VN) == n.dots))
  errs <- NULL
  if (n.dots == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (!ok.VN) {errs <- base::c(errs, "[VN] must be NULL or match the number of arguments in [...].")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (!uj::f0(base::is.null(VN), T, uj:::.labs_ok(VN))) {uj::stopperr("element names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  uj::name_vals(y, VN)
}

#' @rdname declare
#' @export
vls. <- function(...) {
  labs <- base::as.character(base::match.call())
  labs <- labs[2:base::length(labs)]
  nok <- base::...length() > 0
  unq <- uj::f0(!nok, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  errs <- NULL
  if (!nok) {errs <- base::c(errs, "[...] is empty.")}
  if (!unq) {errs <- base::c(errs, "[...] arguments must be uniquely named.")}
  if (!lab) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  code <- base::paste0("list(", base::paste0(base::paste0(labs, "=", labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}
