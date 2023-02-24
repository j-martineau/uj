# internal ####

.labs_ok <- function(x) {
  alpha <- base::c(letters, LETTERS)
  nums <- uj::asCHR(0:9)
  ok <- uj::isIN(base::substr(x, 1, 1), alpha)
  ok[!ok] <- base::substr(x[!ok], 1, 1) == "." & uj::isIN1(base::substr(x[!ok], 2, 2), alpha)
  if (base::all(ok)) {
    nch <- uj::LEN(x)
    ok <- ok & uj::isIN(base::substr(x, nch, nch), base::c(alpha, nums))
    uj::f0(!base::all(ok), F, uj::allIN(uj::av(base::strsplit(x, "", fixed = T)), base::c(alpha, nums, ".", "_")))
  } else {F}
}

# exported ####

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
#' @param EN A possibly pipe-delimited, \link[=cmp_chr_vec]{complete character vec} of names to apply to vector or list elements
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
#' vec(egA, egB, egC, EN = abc)
#' vec(egA, egB, egC, EN = "a|b|c")
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
#' vls(egABC, egNums, egABC, EN = labs1)
#' vls(egABC = egABC, egNums = egNums, egNA = egNA)
#' vls(egABC, egNums, egNA, EN = "egABC|egNums|egNA")
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
  labs1 <- uj::f0(uj::NLL(CN), NULL, uj::f0(uj::cmp_chr_scl(CN), uj::av(base::strsplit(CN, "|", fixed = T)), CN))
  labs2 <- uj::f0(uj::NLL(uj::DN()), NULL, uj::DN())
  labs <- uj::f0(uj::DEF(labs1), labs1, labs2)
  x <- base::list(...)
  n <- uj::N(x)
  ok.0 <- n > 0
  ok.lb1 <- uj::f0(uj::NLL(labs1), T, uj::N(labs) == n & uj::noneBL(labs == ""))
  ok.lb2 <- uj::f0(uj::NLL(labs2), T, uj::noneBL(labs2))
  ok.lbs <- uj::f0(!ok.lb1 | !ok.lb2, T, uj:::.labs_ok(labs))
  uj::errs_if_nots(ok.0  , "[...] is empty."                                                                   ,
                   ok.lb1, "the number of column names in [CN] must equal [...length()] when [CN] is not NULL.",
                   ok.lb2, "[...] arguments must be named when [CN = NULL]."                                   ,
                   ok.lbs, "column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj" )
  ns <- uj::NS(x)
  mx <- base::max(ns)
  ok1 <- base::all(base::sapply(x, uj::atm_vec))
  ok2 <- base::all(ns > 0)
  ok3 <- uj::f0(!ok1, T, base::all(uj::rounded(mx / ns)))
  ok4 <- uj::NEQ(labs, uj::UV(labs))
  uj::errs_if_nots(ok1, "[...] arguments must be atomic vecs (?atm_vec).",
                   ok2, "[...] arguments may not be of length 0."        ,
                   ok3, "[...] arguments are not recyclable."            ,
                   ok4, "column names must be unique."                   , PKG = "uj")
  x <- tibble::tibble(...)
  uj::CN(x) <- labs
  x
}

#' @rdname declare
#' @export
dtf. <- function(...) {
  labs <- uj::asCHR(base::match.call())
  labs <- labs[2:uj::N(labs)]
  nok <- uj::ND1P()
  avc <- uj::f0(!nok, T, base::all(base::sapply(base::list(...), uj::atm_vec)))
  unq <- uj::f0(!nok, T, uj::NEQ(labs, uj::UV(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  if (nok & avc) {
    ns <- uj::NS(base::list(...))
    nr <- base::max(ns) / ns
    rec <- base::all(uj::rounded(nr))
  } else {rec <- TRUE}
  uj::errs_if_nots(nok, "[...] is empty."                                ,
                   unq, "[...] arguments must be uniquely named."        ,
                   avc, "[...] arguments must be atomic vecs (?atm_vec).",
                   rec, "[...] arguments are not recyclable."            ,
                   lab, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj" )
  code <- uj::p0("tibble::tibble(", uj::g(", ", uj::p0(labs, "=", labs)), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}

#' @rdname declare
#' @export
dtf0 <- function(CN) {
  uj::err_if_not(uj::cmp_chr_vec(CN), "[CN] must be a complete character vector (?cmp_chr_vec).", PKG = "uj")
  CN <- uj::av(base::strsplit(CN, "|", fixed = T))
  uj::err_if_not(uj:::.labs_ok(CN), "column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")
  code <- uj::p0("tibble::tibble(", uj::g(", ", uj::p0(CN, " = NA")), ", .rows = 0)")
  uj::run(code)
}

#' @rdname declare
#' @export
dtfNA <- function(CN, NR) {
  uj::errs_if_nots(uj::cmp_chr_vec(CN), "[CN] must be a complete character vector (?cmp_chr_vec)."           ,
                   uj::cmp_psw_scl(NR), "[NR] must be NULL or a positive whole number scalar (?cmp_psw_scl).", PKG = "uj")
  CN <- uj::av(base::strsplit(CN, "|", fixed = T))
  uj::err_if_not(uj:::.labs_ok(CN), "Column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")
  code <- uj::p0("tibble::tibble(", uj::g(", ", uj::p0(CN, " = base::rep.int(NA, NR)")), ")")
  uj::run(code)
}


#' @rdname declare
#' @export
mat <- function(..., R = 1, NR = NULL, NC = NULL, BR = F, RN = NULL, CN = NULL) {
  X <- uj::av(...)
  if (uj::N0(X)) {X <- NA}
  uj::errs_if_nots(cmp_psw_scl(R)                   , "[R] must be a positive whole number scalar (?cmp_psw_scl)."             ,
                   uj::NLL(NR) | uj::cmp_nnw_scl(NR), "[NR] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).",
                   uj::NLL(NC) | uj::cmp_nnw_scl(NC), "[NC] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).",
                   uj::isTF1(BR)                    , "[BR] must be TRUE or FALSE."                                            ,
                   uj::NLL(RN) | uj::cmp_nnw_scl(RN), "[RN] must be NULL or a complete character vector (?cmp_chr_vec)."       ,
                   uj::NLL(CN) | uj::cmp_nnw_scl(CN), "[CN] must be NULL or a complete character vector (?cmp_chr_vec)."       , PKG = "uj")
  X <- base::rep.int(X, R)
  if (uj::N1(X)) {X <- base::rep.int(X, NR * NC)}
  if (uj::NLL(NR) & uj::NLL(NC)) {NR <- 1; NC <- uj::N(X)}
  else if (uj::NLL(NR)) {NR <- uj::N(X) / NC}
  else if (uj::NLL(NC)) {NC <- uj::N(X) / NR}
  match.both <- NR * NC == uj::N(X)
  match.one <- uj::rounded(NR) & uj::rounded(NC)
  uj::err_if_not(match.both & match.one, "[R * length(av(...))] is not divisible by [NR] and/or [NC].", PKG = "uj")
  RN <- uj::f0(uj::NLL(RN), NULL, uj::av(base::strsplit(RN, "|", fixed = T)))
  CN <- uj::f0(uj::NLL(CN), NULL, uj::av(base::strsplit(CN, "|", fixed = T)))
  ok.RN <- uj::f0(uj::NLL(RN), T, uj::N(RN) == NR)
  ok.CN <- uj::f0(uj::NLL(CN), T, uj::N(CN) == NC)
  uj::errs_if_nots(ok.RN                                   , "The number of rownames in [RN] does not match the number of rows.",
                   ok.CN                                   , "The number of colnames in [CN] does not match the number of cols.",
                   uj::f0(uj::NLL(RN), T, uj:::.labs_ok(RN)), "row names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.",
                   uj::f0(uj::NLL(CN), T, uj:::.labs_ok(CN)), "col names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj" )
  X <- base::matrix(X, nrow = NR, ncol = NC, byrow = BR)
  X <- uj::namerc(X, RN, CN)
  X
}

#' @rdname declare
#' @export
matd <- function(X = 1, R = 1) {
  ok.X <- (uj::NUM(X) | uj::LGL(X) | uj::CHR(X))
  ok.R <- uj::cmp_psw_scl(R)
  ok.GE1 <- uj::f0(!ok.X | !ok.R, T, uj::N2P(X) | R > 1)
  uj::errs_if_nots(ok.X  , "[X] must be an numeric, logical, or character."         ,
                   ok.R  , "[R] must a positive whole-number scalar (?cmp_psw_scl).",
                   ok.GE1, "Neither [length(X)] nor [R] is greater than 1."         , PKG = "uj")
  NR <- uj::NR(X)
  NC <- uj::NC(X)
  if (uj::isMAT(X)) {if (NR > 0 & NR == NC) {X <- base::diag(X, NR, NC)}}
  X <- uj::av(X)
  if (R > 1) {X <- base::rep(X, R)}
  if (uj::allNA(X)) {X[uj::na(X)] <- NA_real_}
  N <- uj::N(X)
  bl <- uj::f0(uj::NUM(X), 0, uj::f0(uj::LGL(X), F, uj::f0(uj::CHR(X), "", NA)))
  y <- base::matrix(bl, nrow = N, ncol = N)
  base::diag(y) <- X
  y
}

#' @rdname declare
#' @export
vec <- function(..., R = 1, EN = NULL) {
  if (uj::ND1P() | uj::notEQ(R, 0)) {
    X <- uj::av(...)
    uj::errs_if_nots(uj::cmp_nnw_scl(R)               , "[R] must be a non-negative whole-number scalar (?cmp_nnw_scl).",
                     uj::NLL(EN) | uj::cmp_chr_vec(EN), "[EN] must be NULL or a complete character vec (?cmp_chr_vec)." , PKG = "uj")
    if (R > 1) {X <- base::rep.int(X, R)}
    if (uj::DEF(EN)) {
      EN <- uj::av(base::strsplit(EN, "|", fixed = T))
      uj::errs_if_nots(uj::NEQ(X, EN)    , "[EN] must be the same length as the vector resulting from atomizing (?av) [...].",
                       uj::anyBL(EN)     , "[EN] contains a blank string (after splitting along pipes)."                     ,
                       uj:::.labs_ok(EN) , "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")
      X <- uj::namee(X, EN)
    }
    X
  } else {base::vector()}
}

#' @rdname declare
#' @export
vec. <- function(...) {
  labs <- uj::asCHR(base::match.call())
  labs <- uj::Nth_plus(labs, 2)
  nok <- uj::ND1P()
  asc <- uj::f0(!nok, T, base::all(base::sapply(base::list(...), uj::atm_scl)))
  unq <- uj::f0(!nok, T, uj::NEQ(labs, uj::UV(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  uj::errs_if_nots(nok, "[...] is empty.",
                   unq, "[...] argument names must be uniquely.",
                   asc, "[...] arguments must be atomic and scalar (?atm_scl).",
                   lab, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj" )
  code <- uj::p0("base::c(", uj::g(", ", uj::p0(labs, "=", labs)), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}

#' @rdname declare
#' @export
vecNA <- function(R) {base::rep.int(NA, R)}

#' @rdname declare
#' @export
vls <- function(..., EN = NULL) {
  EN <- uj::f0(uj::cmp_chr_vec(EN), uj::av(base::strsplit(EN, "|", fixed = T)), EN)
  y <- base::list(...)
  n.dots <- uj::N(y)
  ok.EN <- uj::f0(uj::NLL(EN), T, uj::f0(!uj::cmp_chr_vec(EN), F, uj::N(EN) == n.dots))
  uj::errs_if_nots(n.dots > 0, "[...] is empty.",
                   ok.EN, "[EN] must be NULL or match the number of arguments in [...].", PKG = "uj")
  uj::err_if_not(uj::f0(uj::NLL(EN), T, uj:::.labs_ok(EN)), "element names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")
  uj::namee(y, EN)
}

#' @rdname declare
#' @export
vls. <- function(...) {
  labs <- uj::asCHR(base::match.call())
  labs <- uj::Nth_plus(labs, 2)
  nok <- uj::ND1P()
  unq <- uj::f0(!nok, T, uj::NEQ(labs, uj::UV(labs)))
  lab <- uj::f0(!nok | !unq, T, uj:::.labs_ok(labs))
  uj::errs_if_nots(nok, "[...] is empty.",
                   unq, "[...] arguments must be uniquely named.",
                   lab, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")
  code <- uj::p0("list(", uj::g(", ", uj::p0(labs, "=", labs)), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}
