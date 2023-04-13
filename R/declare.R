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
#'                `matd`    \tab Declares a square diagonal matrix: off-diags are `0`, `FALSE`, and `""` for `X` of mode `'numeric'`, `'logical'`, and `'character'`, respectively. }
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
#' vls(egABC, egNums, egABC, VN = Labs1)
#' vls(egABC = egABC, egNums = egNums, egNA = egNA)
#' vls(egABC, egNums, egNA, VN = "egABC|egNums|egNA")
#' vls.(egABC, egNums, egNA)
#'
#' dtf(egABC, egNUMS, egNA, CN = Labs2)
#' dtf(egABC = egABC, egNUMS = egNUMS, egNA = egNA)
#' dtf(egABC, egNUMS, egNA, CN = "egABC|egNUMS|egNA")
#' dtf.(egABC, egNUMS, egNA)
#' dtf0(egABC)
#' dtf0("egA|egB|egC")
#' dtfNA(egABC, 3)
#' @export
dtf <- function(..., CN = NULL) {
  Labs1 <- uj::f0(base::is.null(CN), NULL, uj::f0(uj:::.cmp_chr_scl(CN), uj::av(base::strsplit(CN, "|", fixed = T)), CN))
  Labs2 <- uj::dn()
  Labs <- uj::f0(!base::is.null(Labs1), Labs1, Labs2)
  X <- base::list(...)
  N <- base::length(X)
  Ok0 <- N > 0
  OkLb1 <- uj::f0(base::is.null(Labs1), T, base::length(Labs) == N & !base::any(Labs == ""))
  OkLb2 <- uj::f0(base::is.null(Labs2), T, !base::any(Labs2 == ""))
  OkLbs <- uj::f0(!OkLb1 | !OkLb2, T, uj:::.labs_ok(Labs))
  Errors <- NULL
  if (!Ok0) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!OkLb1) {Errors <- base::c(Errors, "the number of column names in [CN] must equal [...length()] when [CN] is not NULL.")}
  if (!OkLb2) {Errors <- base::c(Errors, "[...] arguments must be named when [CN = NULL].")}
  if (!OkLbs) {Errors <- base::c(Errors, "column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Ns <- uj::ns(X)
  MaxN <- base::max(Ns)
  Ok1 <- base::all(base::sapply(X, uj:::.atm_vec))
  Ok2 <- base::all(Ns > 0)
  Ok3 <- uj::f0(!Ok1, T, base::all((MaxN / Ns) == base::round(MaxN / Ns)))
  Ok4 <- base::length(Labs) == base::length(base::unique(Labs))
  if (!Ok1) {Errors <- base::c(Errors, "[...] arguments must be atomic vecs (?atm_vec).")}
  if (!Ok2) {Errors <- base::c(Errors, "[...] arguments may not be of length 0.")}
  if (!Ok3) {Errors <- base::c(Errors, "[...] arguments are not recyclable.")}
  if (!Ok4) {Errors <- base::c(Errors, "column names must be unique.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  X <- tibble::tibble(...)
  base::colnames(X) <- Labs
  X
}

#' @rdname declare
#' @export
dtf. <- function(...) {
  Labs <- base::as.character(base::match.call())
  Labs <- Labs[2:base::length(Labs)]
  OkN <- base::...length() > 0
  AVC <- uj::f0(!OkN, T, base::all(base::sapply(base::list(...), uj:::.atm_vec)))
  Unq <- uj::f0(!OkN, T, base::length(Labs) == base::length(base::unique(Labs)))
  Lab <- uj::f0(!OkN | !Unq, T, uj:::.labs_ok(Labs))
  if (OkN & AVC) {
    ns <- uj::ns(base::list(...))
    nr <- base::max(ns) / ns
    Rec <- base::all(nr == base::round(nr))
  } else {Rec <- TRUE}
  Errors <- NULL
  if (!OkN) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!Unq) {Errors <- base::c(Errors, "[...] arguments must be uniquely named.")}
  if (!AVC) {Errors <- base::c(Errors, "[...] arguments must be atomic vecs (?atm_vec).")}
  if (!Rec) {Errors <- base::c(Errors, "[...] arguments are not recyclable.")}
  if (!Lab) {Errors <- base::c(Errors, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Code <- base::paste0("tibble::tibble(", base::paste0(base::paste0(Labs, "=", Labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = Code, n = 1))
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
  Errors <- NULL
  if (!uj:::.cmp_chr_vec(CN)) {Errors <- base::c(Errors, "[CN] must be a complete character vector (?cmp_chr_vec).")}
  if (!uj:::.cmp_psw_scl(NR)) {Errors <- base::c(Errors, "[NR] must be NULL or a positive whole number scalar (?cmp_psw_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
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
  Errors <- NULL
  if (!uj:::.cmp_psw_scl(R)) {Errors <- base::c(Errors, "[R] must be a positive whole number scalar (?cmp_psw_scl).")}
  if (!base::is.null(NR) & !uj:::.cmp_nnw_scl(NR)) {Errors <- base::c(Errors, "[NR] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!base::is.null(NC) & !uj:::.cmp_nnw_scl(NC)) {Errors <- base::c(Errors, "[NC] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!uj:::.cmp_lgl_scl(BR)) {Errors <- base::c(Errors, "[BR] must be TRUE or FALSE.")}
  if (!base::is.null(RN) & !uj:::.cmp_nnw_scl(RN)) {Errors <- base::c(Errors, "[RN] must be NULL or a complete character vector (?cmp_chr_vec).")}
  if (!base::is.null(CN) & !uj:::.cmp_nnw_scl(CN)) {Errors <- base::c(Errors, "[CN] must be NULL or a complete character vector (?cmp_chr_vec).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  X <- base::rep.int(X, R)
  if (base::length(X) == 1) {X <- base::rep.int(X, NR * NC)}
  if (base::is.null(NR) & base::is.null(NC)) {NR <- 1; NC <- base::length(X)}
  else if (base::is.null(NR)) {NR <- base::length(X) / NC}
  else if (base::is.null(NC)) {NC <- base::length(X) / NR}
  MatchBoth <- NR * NC == base::length(X)
  MatchOne <- NR == round(NR) & NC == round(NC)
  if (!MatchBoth | !MatchOne) {uj::stopperr("[R * length(av(...))] is not divisible by [NR] and/or [NC].", PKG = "uj")}
  RN <- uj::f0(base::is.null(RN), NULL, uj::av(base::strsplit(RN, "|", fixed = T)))
  CN <- uj::f0(base::is.null(CN), NULL, uj::av(base::strsplit(CN, "|", fixed = T)))
  OkRN <- uj::f0(base::is.null(RN), T, base::length(RN) == NR)
  OkCN <- uj::f0(base::is.null(CN), T, base::length(CN) == NC)
  if (!OkRN) {Errors <- base::c(Errors, "The number of rownames in [RN] does not match the number of rows.")}
  if (!OkCN) {Errors <- base::c(Errors, "The number of colnames in [CN] does not match the number of cols.")}
  if (!uj::f0(base::is.null(RN), T, uj:::.labs_ok(RN))) {Errors <- base::c(Errors, "row names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!uj::f0(base::is.null(CN), T, uj:::.labs_ok(CN))) {Errors <- base::c(Errors, "col names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  X <- base::matrix(X, nrow = NR, ncol = NC, byrow = BR)
  base::colnames(X) <- CN
  base::rownames(X) <- RN
  X
}

#' @rdname declare
#' @export
matd <- function(X = 1, R = 1) {
  OkX <- (uj:::.NUM(X) | uj:::.LGL(X) | uj:::.CHR(X))
  OkR <- uj:::.cmp_psw_scl(R)
  OkGE1 <- uj::f0(!OkX | !OkR, T, base::length(X) > 1 | R > 1)
  Errors <- NULL
  if (!OkX) {Errors <- base::c(Errors, "[X] must be an numeric, logical, or character.")}
  if (!OkR) {Errors <- base::c(Errors, "[R] must a positive whole-number scalar (?cmp_psw_scl).")}
  if (!OkGE1) {Errors <- base::c(Errors, "Neither [length(X)] nor [R] is greater than 1.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  NR <- base::nrow(X)
  NC <- base::ncol(X)
  if (base::is.matrix(X)) {if (NR > 0 & NR == NC) {X <- base::diag(X, NR, NC)}}
  X <- uj::av(X)
  if (R > 1) {X <- base::rep(X, R)}
  if (base::all(base::is.na(X))) {X[base::is.na(X)] <- NA_real_}
  N <- base::length(X)
  Bl <- uj::f0(uj:::.NUM(X), 0, uj::f0(uj:::.LGL(X), F, uj::f0(uj:::.CHR(X), "", NA)))
  Y <- base::matrix(Bl, nrow = N, ncol = N)
  base::diag(Y) <- X
  Y
}

#' @rdname declare
#' @export
vec <- function(..., R = 1, VN = NULL) {
  if (base::...length() > 0 | uj:::.cmp_psw_scl(R)) {
    X <- uj::av(...)
    Errors <- NULL
    if (!uj:::.cmp_nnw_scl(R)) {Errors <- base::c(Errors, "[R] must be a non-negative whole-number scalar (?cmp_nnw_scl).")}
    if (!base::is.null(VN) & !uj:::.cmp_chr_vec(VN)) {Errors <- base::c(Errors, "[VN] must be NULL or a complete character vec (?cmp_chr_vec).")}
    if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
    if (R > 1) {X <- base::rep.int(X, R)}
    if (!base::is.null(VN)) {
      VN <- uj::av(base::strsplit(VN, "|", fixed = T))
      if (base::length(X) != base::length(VN)) {Errors <- base::c(Errors, "[VN] must be the same length as the vector resulting from atomizing (?av) [...].")}
      if (base::any(VN == "")) {Errors <- base::c(Errors, "[VN] contains a blank string (after splitting along pipes).")}
      if (!uj:::.labs_ok(VN)) {Errors <- base::c(Errors, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
      base::names(X) <- VN
    }
    X
  } else if (!uj:::.cmp_nnw_scl(R)) {uj::stopperr("[R] must be a non-negative whole-number scalar (?cmp_nnw_scl).", PKG = "uj")}
  else {base::vector()}
}

#' @rdname declare
#' @export
vec. <- function(...) {
  Labs <- base::as.character(base::match.call())
  Labs <- Labs[2:base::length(Labs)]
  OkN <- base::...length() > 0
  ASC <- uj::f0(!OkN, T, base::all(base::sapply(base::list(...), uj:::.atm_scl)))
  Unq <- uj::f0(!OkN, T, base::length(Labs) == base::length(base::unique(Labs)))
  Lab <- uj::f0(!OkN | !Unq, T, uj:::.labs_ok(Labs))
  Errors <- NULL
  if (!OkN) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!Unq) {Errors <- base::c(Errors, "[...] argument names must be uniquely.")}
  if (!ASC) {Errors <- base::c(Errors, "[...] arguments must be atomic and scalar (?atm_scl).")}
  if (!Lab) {Errors <- base::c(Errors, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Code <- base::paste0("base::c(", base::paste0(uj::p0(Labs, "=", Labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = Code, n = 1))
}

#' @rdname declare
#' @export
vecNA <- function(R) {base::rep.int(NA, R)}

#' @rdname declare
#' @export
vls <- function(..., VN = NULL) {
  Export <- uj::f0(uj::cmp_chr_vec(VN), uj::av(base::strsplit(VN, "|", fixed = T)), VN)
  Y <- base::list(...)
  nDots <- base::length(Y)
  OkVN <- uj::f0(base::is.null(Export), T, uj::f0(!uj::cmp_chr_vec(VN), F, base::length(VN) == nDots))
  Errors <- NULL
  if (nDots == 0) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!OkVN) {Errors <- base::c(Errors, "[VN] must be NULL or match the number of arguments in [...].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!uj::f0(base::is.null(VN), T, uj:::.labs_ok(VN))) {uj::stopperr("element names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", PKG = "uj")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  uj::name_vals(Y, VN)
}

#' @rdname declare
#' @export
vls. <- function(...) {
  Labs <- base::as.character(base::match.call())
  Labs <- Labs[2:base::length(Labs)]
  OkN <- base::...length() > 0
  Unq <- uj::f0(!OkN, T, base::length(Labs) == base::length(base::unique(Labs)))
  Lab <- uj::f0(!OkN | !Unq, T, uj:::.labs_ok(Labs))
  Errors <- NULL
  if (!OkN) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!Unq) {Errors <- base::c(Errors, "[...] arguments must be uniquely named.")}
  if (!Lab) {Errors <- base::c(Errors, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Code <- base::paste0("list(", base::paste0(base::paste0(Labs, "=", Labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = Code, n = 1))
}
