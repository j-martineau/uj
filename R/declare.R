# internals ####

.labs_ok <- function(x) {
  alpha  <- base::c(letters, LETTERS)
  digits <- base::as.character(0:9)
  ok <- base::substr(x, 1, 1) %in% alpha
  ok[!ok] <- base::substr(x[!ok], 1, 1) == "." & base::substr(x[!ok], 2, 2) %in% alpha
  if (base::all(ok)) {
    nch <- base::nchar(x)
    ok <- ok & base::substr(x, nch, nch) %in% base::c(alpha, digits)
    uj::f0(!base::all(ok), F, base::all(uj::av(base::strsplit(x, "", fixed = T)) %in% base::c(alpha, digits, ".", "_")))
  } else {F}
}

# exported ####

#' @name declare
#' @encoding UTF-8
#' @family extensions
#' @title Declare basic .r objects with extended functionality
#' @description This family of functions enforces the restrictions that names must (a) contain only ASCII letters, numerals, `'.'`, and `'_'`; (b) begin with a letter or `'.'` followed by a letter; and (c) end with a letter or numeral.
#' \cr\cr **Functions declaring \link[=atm_dtf]{atomic data.frames}:**
#' \tabular{ll}{  `dtf`     \tab Declares a data frame.                                                                                          \cr   \tab   \cr
#'                `dtf.`    \tab Declares a data frame concisely: `dtf.(a, b)` is identical to `data.frame(a = a, b = b, stringsAsFactors = F)`. \cr   \tab   \cr
#'                `dtf0`    \tab Declares a `0`-row data frame.                                                                                  \cr   \tab   \cr
#'                `dtfNA`   \tab Declares a data frame of `NA` values.                                                                                          }
#' \cr\cr **Functions declaring matrices:**
#' \tabular{ll}{  `mat`     \tab Declares a matrix.                                                                                              \cr   \tab   \cr
#'                `matd`    \tab Declares a square diagonal matrix: off-diagonals are `0`, `FALSE`, and `""` consistent with `mode(av(...))`.    \cr   \tab   \cr
#'                `matNA`   \tab Declares a matrix of `NA` values.                                                                                              }
#' \cr\cr **Functions declaring atomic vectors:**
#' \tabular{ll}{  `vec`     \tab Declares a vector.                                                                                              \cr   \tab   \cr
#'                `vec.`    \tab Declares a named atomic vector concisely: `vec.(a, b)` is identical to `c(a = a, b = b)`.                       \cr   \tab   \cr
#'                `vecNA`   \tab Declares a vector of `NA` values.                                                                                              }
#' \cr\cr **Functions declaring \link[=iVLS]{vlists}:**
#' \tabular{ll}{  `vls`     \tab Declares a \link[=VLS]{vlist}.                                                                                  \cr   \tab   \cr
#'                `vls.`    \tab Declares a named \link[=VLS]{vlist} concisely: `vls.(a, b)` is identical to `list(a = a, b = b)`.                              }
#' @param ... Objects to placed in an atomic vec, atomic matrix, atomic data.frame, vlist, or square atomic diagonal matrix. \link[=a]{Atomized} for vec and matrix creation.
#' @param x A non-empty vector of mode `'numeric'`, `'character'`, or `'logical'`.
#' @param .r A non-`NA` numeric scalar number of replications.
#' @param .br `TRUE` or `FALSE` indicating whether to fill matrices by row.
#' @param .cn Possibly pipe-delimited complete character vec of matrix/data.frame column names.
#' @param .rn Possibly pipe-delimited complete character vec of matrix row names.
#' @param .vn A possibly pipe-delimited, \link[=cmp_chr_vec]{complete character vec} of names to apply to vector or list elements
#' @param .nr Complete positive whole-number scalars giving number of matrix rows.
#' @param .nc Complete positive whole-number scalars giving number of matrix columns.
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
#' vec(egA, egB, egC, .vn = abc)
#' vec(egA, egB, egC, .vn = "a|b|c")
#' vec(egA = egA, egB = egB, egC = egC)
#' vec.(egA, egB, egC)
#' vecNA(3)
#'
#' mat(1:9)
#' mat(1:9, .nr = 3)
#' mat(1:9, .nc = 3)
#' mat(1:9, .nr = 3, .rn = "a|b|c", .cn = egABC, .br = TRUE)
#' dmat(egABC)
#' dmat(1, 3)
#' dmat(TRUE, 3)
#'
#' vls(egABC, egNums, egNA)
#' vls(egABC, egNums, egABC, .vn = Labs1)
#' vls(egABC = egABC, egNums = egNums, egNA = egNA)
#' vls(egABC, egNums, egNA, .vn = "egABC|egNums|egNA")
#' vls.(egABC, egNums, egNA)
#'
#' dtf(egABC, egNUMS, egNA, ..cn = Labs2)
#' dtf(egABC = egABC, egNUMS = egNUMS, egNA = egNA)
#' dtf(egABC, egNUMS, egNA, ..cn = "egABC|egNUMS|egNA")
#' dtf.(egABC, egNUMS, egNA)
#' dtf0(egABC)
#' dtf0("egA|egB|egC")
#' dtfNA(egABC, 3)
#' @export
dtf <- function(..., ..cn = NULL) {
  labs1 <- uj::f0(base::is.null(..cn), NULL, uj::f0(uj::.cmp_chr_scl(..cn), uj::av(base::strsplit(..cn, "|", fixed = T)), ..cn))
  labs2 <- uj::dn()
  labs <- uj::f0(!base::is.null(labs1), labs1, labs2)
  x <- base::list(...)
  n <- base::length(x)
  ok0 <- n > 0
  okLb1 <- uj::f0(base::is.null(labs1), T, base::length(labs) == n & !base::any(labs == ""))
  okLb2 <- uj::f0(base::is.null(labs2), T, !base::any(labs2 == ""))
  okLbs <- uj::f0(!okLb1 | !okLb2, T, uj:::.labs_ok(labs))
  errs <- NULL
  if (!ok0) {errs <- base::c(errs, "[...] is empty.")}
  if (!okLb1) {errs <- base::c(errs, "the number of column names in [..cn] must equal [...length()] when [..cn] is not NULL.")}
  if (!okLb2) {errs <- base::c(errs, "[...] arguments must be named when [..cn = NULL].")}
  if (!okLbs) {errs <- base::c(errs, "column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  ns <- uj::ns(x)
  maxN <- base::max(ns)
  ok1 <- base::all(base::sapply(x, uj::.atm_vec))
  ok2 <- base::all(ns > 0)
  ok3 <- uj::f0(!ok1, T, base::all((maxN / ns) == base::round(maxN / ns)))
  ok4 <- base::length(labs) == base::length(base::unique(labs))
  if (!ok1) {errs <- base::c(errs, "[...] arguments must be atomic vecs (?atm_vec).")}
  if (!ok2) {errs <- base::c(errs, "[...] arguments may not be of length 0.")}
  if (!ok3) {errs <- base::c(errs, "[...] arguments are not recyclable.")}
  if (!ok4) {errs <- base::c(errs, "column names must be unique.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  x <- tibble::tibble(...)
  base::colnames(x) <- labs
  x
}

#' @rdname declare
#' @export
dtf. <- function(...) {
  labs <- base::as.character(base::match.call())
  labs <- labs[2:base::length(labs)]
  okN <- base::...length() > 0
  avc <- uj::f0(!okN, T, base::all(base::sapply(base::list(...), uj::.atm_vec)))
  unq <- uj::f0(!okN, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!okN | !unq, T, uj:::.labs_ok(labs))
  if (okN & avc) {
    ns <- uj::ns(base::list(...))
    .nr <- base::max(ns) / ns
    rec <- base::all(.nr == base::round(.nr))
  } else {rec <- TRUE}
  errs <- NULL
  if (!okN) {errs <- base::c(errs, "[...] is empty.")}
  if (!unq) {errs <- base::c(errs, "[...] arguments must be uniquely named.")}
  if (!avc) {errs <- base::c(errs, "[...] arguments must be atomic vecs (?atm_vec).")}
  if (!rec) {errs <- base::c(errs, "[...] arguments are not recyclable.")}
  if (!lab) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  code <- base::paste0("tibble::tibble(", base::paste0(base::paste0(labs, "=", labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}

#' @rdname declare
#' @export
dtf0 <- function(.cn) {
  if (!uj::.cmp_chr_vec(.cn)) {uj::stopperr("[.cn] must be a complete character vector (?cmp_chr_vec).", pkg = "uj")}
  .cn <- uj::av(base::strsplit(.cn, "|", fixed = T))
  if (!uj:::.labs_ok(.cn)) {uj::stopperr("column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", pkg = "uj")}
  code <- base::paste0("tibble::tibble(", base::paste0(base::paste0(.cn, " = NA"), collapse = ", "), ", rows = 0)")
  uj::run(code)
}

#' @rdname declare
#' @export
dtfNA <- function(.cn, .nr) {
  errs <- NULL
  if (!uj::.cmp_chr_vec(.cn)) {errs <- base::c(errs, "[.cn] must be a complete character vector (?cmp_chr_vec).")}
  if (!uj::.cmp_psw_scl(.nr)) {errs <- base::c(errs, "[.nr] must be NULL or a positive whole number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  .cn <- uj::av(base::strsplit(.cn, "|", fixed = T))
  if (!uj:::.labs_ok(.cn)) {uj::stopperr("Column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", pkg = "uj")}
  code <- base::paste0("tibble::tibble(", base::paste0(base::paste0(.cn, " = base::rep.int(NA, .nr)"), collapse = ", "), ")")
  uj::run(code)
}


#' @rdname declare
#' @export
mat <- function(..., .r = 1, .nr = NULL, .nc = NULL, .br = F, .rn = NULL, .cn = NULL) {
  x <- uj::av(...)
  if (base::length(x) == 0) {x <- NA}
  errs <- NULL
  if (!uj::.cmp_psw_scl(.r)) {errs <- base::c(errs, "[.r] must be a positive whole number scalar (?cmp_psw_scl).")}
  if (!base::is.null(.nr) & !uj::.cmp_nnw_scl(.nr)) {errs <- base::c(errs, "[.nr] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!base::is.null(.nc) & !uj::.cmp_nnw_scl(.nc)) {errs <- base::c(errs, "[.nc] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!uj::.cmp_lgl_scl(.br)) {errs <- base::c(errs, "[.br] must be TRUE or FALSE.")}
  if (!base::is.null(.rn) & !uj::.cmp_nnw_scl(.rn)) {errs <- base::c(errs, "[.rn] must be NULL or a complete character vector (?cmp_chr_vec).")}
  if (!base::is.null(.cn) & !uj::.cmp_nnw_scl(.cn)) {errs <- base::c(errs, "[.cn] must be NULL or a complete character vector (?cmp_chr_vec).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  x <- base::rep.int(x, .r)
  if (base::length(x) == 1) {x <- base::rep.int(x, .nr * .nc)}
  if (base::is.null(.nr) & base::is.null(.nc)) {.nr <- 1; .nc <- base::length(x)}
  else if (base::is.null(.nr)) {.nr <- base::length(x) / .nc}
  else if (base::is.null(.nc)) {.nc <- base::length(x) / .nr}
  matchBoth <- .nr * .nc == base::length(x)
  matchOne <- .nr == round(.nr) & .nc == round(.nc)
  if (!matchBoth | !matchOne) {uj::stopperr("[.r * length(av(...))] is not divisible by [.nr] and/or [.nc].", pkg = "uj")}
  .rn <- uj::f0(base::is.null(.rn), NULL, uj::av(base::strsplit(.rn, "|", fixed = T)))
  .cn <- uj::f0(base::is.null(.cn), NULL, uj::av(base::strsplit(.cn, "|", fixed = T)))
  okRN <- uj::f0(base::is.null(.rn), T, base::length(.rn) == .nr)
  okCN <- uj::f0(base::is.null(.cn), T, base::length(.cn) == .nc)
  if (!okRN) {errs <- base::c(errs, "The number of rownames in [.rn] does not match the number of rows.")}
  if (!okCN) {errs <- base::c(errs, "The number of colnames in [.cn] does not match the number of cols.")}
  if (!uj::f0(base::is.null(.rn), T, uj:::.labs_ok(.rn))) {errs <- base::c(errs, "row names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!uj::f0(base::is.null(.cn), T, uj:::.labs_ok(.cn))) {errs <- base::c(errs, "col names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  x <- base::matrix(x, nrow = .nr, ncol = .nc, byrow = .br)
  base::colnames(x) <- .cn
  base::rownames(x) <- .rn
  x
}

#' @rdname declare
#' @export
matNA <- function(.nr = 1, .nc = 1, .rn = NULL, .cn = NULL) {
  okNR <- uj::.cmp_nnw_scl(.nr)
  okNC <- uj::.cmp_nnw_scl(.nc)
  okRN <- uj::f0(uj::N(.rn) == 0, T, uj::is_EQ(uj::N(.rn), .nr))
  okCN <- uj::f0(uj::N(.cn) == 0, T, uj::is_EQ(uj::N(.cn), .nc))
  errs <- NULL
  if (!okNR) {errs <- base::c(errs, "[.nr] must be a non-negative whole-number scalar.")}
  if (!okNC) {errs <- base::c(errs, "[.nc] must be a non-negative whole-number scalar.")}
  if (!okRN) {errs <- base::c(errs, "[.rn] must be NULL or of length [.nr].")}
  if (!okRN) {errs <- base::c(errs, "[.cn] must be NULL or of length [.nc].")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  x <- base::matrix(NA, nrow = .nr, ncol = .cn)
  if (!base::is.null(.rn)) {base::rownames(x) <- .rn}
  if (!base::is.null(.cn)) {base::rownames(x) <- .cn}
  x
}


#' @rdname declare
#' @export
matd <- function(x = 1, .r = 1) {
  okX <- (uj::.NUM(x) | uj::.LGL(x) | uj::.CHR(x))
  okR <- uj::.cmp_psw_scl(.r)
  okGE1 <- uj::f0(!okX | !okR, T, base::length(x) > 1 | .r > 1)
  errs <- NULL
  if (!okX) {errs <- base::c(errs, "[x] must be an numeric, logical, or character.")}
  if (!okR) {errs <- base::c(errs, "[.r] must a positive whole-number scalar (?cmp_psw_scl).")}
  if (!okGE1) {errs <- base::c(errs, "Neither [length(x)] nor [.r] is greater than 1.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  .nr <- base::nrow(x)
  .nc <- base::ncol(x)
  if (base::is.matrix(x)) {if (.nr > 0 & .nr == .nc) {x <- base::diag(x, .nr, .nc)}}
  x <- uj::av(x)
  if (.r > 1) {x <- base::rep(x, .r)}
  if (base::all(base::is.na(x))) {x[base::is.na(x)] <- NA_real_}
  n <- base::length(x)
  bl <- uj::f0(uj::.NUM(x), 0, uj::f0(uj::.LGL(x), F, uj::f0(uj::.CHR(x), "", NA)))
  y <- base::matrix(bl, nrow = n, ncol = n)
  base::diag(y) <- x
  y
}

#' @rdname declare
#' @export
vec <- function(..., .r = 1, .vn = NULL) {
  if (base::...length() > 0 | uj::.cmp_psw_scl(.r)) {
    x <- uj::av(...)
    errs <- NULL
    if (!uj::.cmp_nnw_scl(.r)) {errs <- base::c(errs, "[.r] must be a non-negative whole-number scalar (?cmp_nnw_scl).")}
    if (!base::is.null(.vn) & !uj::.cmp_chr_vec(.vn)) {errs <- base::c(errs, "[.vn] must be NULL or a complete character vec (?cmp_chr_vec).")}
    if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
    if (.r > 1) {x <- base::rep.int(x, .r)}
    if (!base::is.null(.vn)) {
      .vn <- uj::av(base::strsplit(.vn, "|", fixed = T))
      if (base::length(x) != base::length(.vn)) {errs <- base::c(errs, "[.vn] must be the same length as the vector resulting from atomizing (?av) [...].")}
      if (base::any(.vn == "")) {errs <- base::c(errs, "[.vn] contains a blank string (after splitting along pipes).")}
      if (!uj:::.labs_ok(.vn)) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
      base::names(x) <- .vn
    }
    x
  } else if (!uj::.cmp_nnw_scl(.r)) {uj::stopperr("[.r] must be a non-negative whole-number scalar (?cmp_nnw_scl).", pkg = "uj")}
  else {base::vector()}
}

#' @rdname declare
#' @export
vec. <- function(...) {
  labs <- base::as.character(base::match.call())
  labs <- labs[2:base::length(labs)]
  okN <- base::...length() > 0
  asc <- uj::f0(!okN, T, base::all(base::sapply(base::list(...), uj::.atm_scl)))
  unq <- uj::f0(!okN, T, base::length(labs) == base::length(base::unique(labs)))
  lab <- uj::f0(!okN | !unq, T, uj:::.labs_ok(labs))
  errs <- NULL
  if (!okN) {errs <- base::c(errs, "[...] is empty.")}
  if (!unq) {errs <- base::c(errs, "[...] argument names must be uniquely.")}
  if (!asc) {errs <- base::c(errs, "[...] arguments must be atomic and scalar (?atm_scl).")}
  if (!lab) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  code <- base::paste0("base::c(", base::paste0(uj::p0(labs, "=", labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}

#' @rdname declare
#' @export
vecNA <- function(.r) {base::rep.int(NA, .r)}

#' @rdname declare
#' @export
vls <- function(..., .vn = NULL) {
  export <- uj::f0(uj::cmp_chr_vec(.vn), uj::av(base::strsplit(.vn, "|", fixed = T)), .vn)
  y <- base::list(...)
  nDots <- base::length(y)
  okVN <- uj::f0(base::is.null(export), T, uj::f0(!uj::cmp_chr_vec(.vn), F, base::length(.vn) == nDots))
  errs <- NULL
  if (nDots == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (!okVN) {errs <- base::c(errs, "[.vn] must be NULL or match the number of arguments in [...].")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  if (!uj::f0(base::is.null(.vn), T, uj:::.labs_ok(.vn))) {uj::stopperr("element names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.", pkg = "uj")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  uj::name_vals(y, .vn)
}

#' @rdname declare
#' @export
vls. <- function(...) {
  labs <- base::as.character(base::match.call())
  labs <- labs[2:base::length(labs)]
  okN  <- base::...length() > 0
  unq  <- uj::f0(!okN, T, base::length(labs) == base::length(base::unique(labs)))
  lab  <- uj::f0(!okN | !unq, T, uj:::.labs_ok(labs))
  errs <- NULL
  if (!okN) {errs <- base::c(errs, "[...] is empty.")}
  if (!unq) {errs <- base::c(errs, "[...] arguments must be uniquely named.")}
  if (!lab) {errs <- base::c(errs, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "uj")}
  code <- base::paste0("list(", base::paste0(base::paste0(labs, "=", labs), collapse = ", "), ")")
  base::eval.parent(base::parse(text = code, n = 1))
}
