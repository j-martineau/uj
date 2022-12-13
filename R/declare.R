# internal ####

.labs_ok <- function(x) {
  alpha <- c(letters, LETTERS)
  nums <- as.character(0:9)
  ok <- substr(x, 1, 1) %in% alpha
  ok[!ok] <- substr(x[!ok], 1, 1) == "." & substr(x[!ok], 2, 2) %in% alpha
  if (all(ok)) {
    ok <- ok & substr(x, nchar(x), nchar(x)) %in% c(alpha, nums)
    f0(all(ok), all(av(strsplit(x, "", fixed = T)) %in% c(alpha, nums, ".", "_")), F)
  } else {F}
}

# exported ####

#' @name declare
#' @title Declare basic R objects with extended functionality
#' @description This family of functions enforces the restrictions that names must (a) contain only ASCII letters, numerals, `'.'`, and `'_'`; (b) begin with a letter or `'.'` followed by a letter; and (c) end with a letter or numeral.
#'  \cr\cr Functions declaring \link[=atm_dtf]{atomic data.frames}:\tabular{rl}{
#'             `dtf`   \tab Declares a data.frame.
#'    \cr     `dtf.`   \tab Declares a data.frame concisely\eqn{^1}.
#'    \cr     `dtf0`   \tab Declares a `0`-row data.frame.
#'    \cr   `dtf_na`   \tab Declares a data.frame of `NA` values.
#'  }
#'  Functions declaring matrices:\tabular{rl}{
#'           `mat`   \tab Declares a matrix.
#'    \cr   `matd`   \tab Declares a square diagonal matrix\eqn{^2}.
#'  }
#'  Functions declaring atomic vectors:\tabular{rl}{
#'             `vec`   \tab Declares a vector.
#'    \cr     `vec.`   \tab Declares a named atomic vector concisely\eqn{^3}.
#'    \cr `  vec_na`   \tab Declares a vector of  `NA` values.
#'  }
#'  Functions declaring \link[=ivls]{vlists}:\tabular{rl}{
#'           `vls`   \tab Declares a \link[=ivls]{vlist}.
#'    \cr   `vls.`   \tab Declares a named \link[=ivls]{vlist} concisely\eqn{^4}.
#'  }
#'            \eqn{^{1.}} `dtf.(a, b)` is identical to `data.frame(a = a, b = b, stringsAsFactors = F)`.
#'  \cr\cr    \eqn{^{2.}} Off-diags are `0`, `FALSE`, `""` for `x` of mode `'numeric'`, `'logical'`, `'character'`, respectively.
#'  \cr\cr    \eqn{^{3.}} `vec.(a, b)` is identical to `c(a = a, b = b)`.
#'  \cr\cr    \eqn{^{4.}} `vls.(a, b)` is identical to `list(a = a, b = b)`.
#' @param ... Objects to placed in an atomic vec, atomic matrix, atomic data.frame, vlist, or square atomic diagonal matrix. \link[=a]{Atomized} for vec and matrix creation.
#' @param x A non-empty vector of mode `'numeric'`, `'character'`, or `'logical'`.
#' @param r A non-`NA` numeric scalar number of replications.
#' @param en. A possibly pipe-delimited, \link[=cmp_chr_vec]{complete character vec} of names to apply to vector or list elements
#' @param cn. A possibly pipe-delimited complete character vec of column names (respectively) for a data.frame.
#' @param rn,cn Possibly pipe-delimited complete character vecs of row and column names (respectively) for a matrix.
#' @param nr,nc Complete positive whole-number scalars giving number of rows and columns, respectively.
#' @param br A non-`NA` logical scalar indicating whether to fill matrices by row.
#' @return \tabular{rl}{
#'       `dtf_na`   \tab An atomic
#'   \cr   `dtf0`   \tab data.frame.
#'   \cr   `dtf.`   \tab   
#'   \cr    `dtf`   \tab   
#'   \cr            \tab   
#'   \cr `vec_na`   \tab An atomic
#'   \cr   `vec.`   \tab vector.
#'   \cr    `vec`   \tab   
#'   \cr            \tab   
#'   \cr   `vls.`   \tab A \link[=ivls]{vlist}.
#'   \cr    `vls`   \tab   
#'   \cr            \tab   
#'   \cr   `dmat`   \tab A square matrix.
#'   \cr            \tab   
#'   \cr    `mat`   \tab A matrix.
#' }
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
  labs1 <- f0(is.null(cn.), NULL, f0(cmp_chr_scl(cn.), av(strsplit(cn., "|", fixed = T)), cn.))
  labs2 <- f0(is.null(...names()), NULL, ...names())
  labs <- f0(!is.null(labs1), labs1, labs2)
  x <- list(...)
  n <- length(x)
  errs <- NULL
  ok.0 <- n > 0
  ok.lb1 <- f0(is.null(labs1), T, length(labs) == n & !any(labs == ""))
  ok.lb2 <- f0(is.null(labs2), T, !any(labs2 == ""))
  ok.lbs <- f0(!ok.lb1 | !ok.lb2, T, .labs_ok(labs))
  if (!(ok.0 & ok.lb1 & ok.lb2 & ok.lbs)) {
    stop(.errs(c(f0(ok.0  , NULL, "[...] is empty."),
                 f0(ok.lb1, NULL, "the number of column names in [cn.] must equal ...length() when [cn.] is not NULL."),
                 f0(ok.lb2, NULL, "[...] arguments must be named when [cn. = NULL]."),
                 f0(ok.lbs, NULL, "column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))))
  }
  ns <- lengths(x)
  mx <- max(ns)
  ok1 <- all(sapply(x, atm_vec))
  ok2 <- all(ns > 0)
  ok3 <- f0(!ok1, T, all(mx / ns == round(mx / ns)))
  ok4 <- length(labs) == length(unique(labs))
  errs <- c(f0(ok1, NULL, "[...] arguments must be atomic vecs (?atm_vec)."),
            f0(ok2, NULL, "[...] arguments may not be of length 0."        ),
            f0(ok3, NULL, "[...] arguments are not recyclable."            ),
            f0(ok4, NULL, "column names must be unique."                   ))
  if (!is.null(errs)) {stop(.errs(errs))}
  x <- data.frame(..., stringsAsFactors = FALSE)
  colnames(x) <- labs
  x
}

#' @rdname declare
#' @export
dtf. <- function(...) {
  labs <- as_chr(match.call())                                                   # get a function call object and convert to character
  labs <- labs[2:length(labs)]                                                   # remove the function call leaving the variables as named in the calling function
  nok <- ...length() > 0
  avc <- f0(!nok, T, all(sapply(list(...), atm_vec)))
  unq <- f0(!nok, T, length(labs) == length(unique(labs)))
  lab <- f0(!nok | !unq, T, .labs_ok(labs))
  if (nok & avc) {
    ns <- lengths(list(...))
    nr <- max(ns) / ns
    rec <- all(nr == round(nr))
  } else {rec <- TRUE}
  errs <- c(f0(nok, NULL, "[...] is empty."),
            f0(unq, NULL, "[...] arguments must be uniquely named."),
            f0(avc, NULL, "[...] arguments must be atomic vecs (?atm_vec)."),
            f0(rec, NULL, "[...] arguments are not recyclable."),
            f0(lab, NULL, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))
  if (!is.null(errs)) {stop(.errs(errs))}
  code <- paste0("data.frame(", paste0(paste(labs, "=", labs), collapse = ", "), ", stringsAsFactors = FALSE)")
  eval.parent(parse(text = code, n = 1))                                         # evaluate the call in the environment of the calling function
}

#' @rdname declare
#' @export
dtf0 <- function(cn) {
  if (!cmp_chr_vec(cn)) {stop(.errs("[cn] must be a complete character vector (?cmp_chr_vec)."))}
  cn <- av(strsplit(cn, "|", fixed = T))
  if (!.labs_ok(cn)) {stop(.errs("column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}
  cmd <- paste0("tibble::tibble(", paste0(paste0(cn, " = NA"), collapse = ", "), ", .rows = 0)")
  run(cmd)
}

#' @rdname declare
#' @export
dtf_na <- function(cn, nr) {
  errs <- c(f0(cmp_chr_vec(cn), NULL, "[cn] must be a complete character vector (?cmp_chr_vec)."),
            f0(cmp_psw_scl(nr), NULL, "[nr] must be NULL or a positive whole number scalar (?cmp_psw_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  cn <- av(strsplit(cn, "|", fixed = T))
  if (!.labs_ok(cn)) {stop(.errs("column names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}
  cmd <- paste0("data.frame(", paste0(paste0(cn, " = rep.int(NA, nr)"), collapse = ", "), ", stringsAsFactor = FALSE)")
  run(cmd)
}


#' @rdname declare
#' @export
mat <- function(..., r = 1, nr = NULL, nc = NULL, br = F, rn = NULL, cn = NULL) {
  x <- av(...)
  if (length(x) == 0) {x <- NA}
  errs <- c(f0(cmp_psw_scl(r)               , NULL, "[r] must be a positive whole number scalar (?cmp_psw_scl)."),
            f0(is.null(nr) | cmp_nnw_scl(nr), NULL, "[nr] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl)."),
            f0(is.null(nc) | cmp_nnw_scl(nc), NULL, "[nc] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl)."),
            f0(isTRUE(br)  | isFALSE(br)    , NULL, "[br] must be TRUE or FALSE."),
            f0(is.null(rn) | cmp_chr_vec(rn), NULL, "[rn] must be NULL or a complete character vector (?cmp_chr_vec)."),
            f0(is.null(cn) | cmp_chr_vec(cn), NULL, "[cn] must be NULL or a complete character vector (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  x <- rep.int(x, r)
  if (is.null(nr) & is.null(nc)) {nr <- 1; nc <- length(x)}
  else if (is.null(nr)) {nr <- length(x) / nc}
  else if (is.null(nc)) {nc <- length(x) / nr}
  if (nr * nc != length(x) | round(nr) != nr | round(nc) != nc) {stop(.errs("[r * length(av(...))] is not divisible by [nr] and/or [nc]."))}
  rn <- f0(is.null(rn), NULL, av(strsplit(rn, "|", fixed = T)))
  cn <- f0(is.null(cn), NULL, av(strsplit(cn, "|", fixed = T)))
  ok.rn <- f0(is.null(rn), T, length(rn) == nr)
  ok.cn <- f0(is.null(cn), T, length(cn) == nc)
  if (!ok.rn | !ok.cn) {
    stop(c(f0(ok.rn, NULL, "The number of rownames in [rn] does not match the number of rows."),
           f0(ok.cn, NULL, "The number of colnames in [cn] does not match the number of cols.")))
  }
  errs <- NULL
  if (!is.null(rn)) {if (!.labs_ok(rn)) {errs <- c(errs, "row names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}}
  if (!is.null(cn)) {if (!.labs_ok(cn)) {errs <- c(errs, "col names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral.")}}
  if (!is.null(errs)) {stop(.errs(errs))}
  x <- matrix(x, nrow = nr, ncol = nc, byrow = br)
  rownames(x) <- rn
  colnames(x) <- cn
  x
}

#' @rdname declare
#' @export
matd <- function(x = 1, r = 1) {
  ok.x <- inum(x) | ilgl(x) | ichr(x)
  ok.r <- cmp_psw_scl(r)
  ok.ge1 <- f0(length(x) > 1, T, f0(ok.r, T, r > 1))
  errs <- c(f0(ok.x  , NULL, "[x] must be an numeric, logical, or character."),
            f0(ok.r  , NULL, "[r] must a positive whole-number scalar (?cmp_psw_scl)."),
            f0(ok.ge1, NULL, "Neither [length(x)] nor [r] is greater than 1."))
  if (!is.null(errs)) {stop(.errs(errs))}
  nr <- nrow(x)
  nc <- ncol(x)
  if (is.matrix(x)) {if (nr > 0 & nr == nc) {x <- diag(x, nr, nc)}}
  x <- av(x)
  if (r > 1) {x <- rep(x, r)}
  if (all(is.na(x))) {x[is.na(x)] <- NA_real_}
  n <- length(x)
  blank <- f0(inum(x), 0, f0(ilgl(x), F, f0(ichr(x), "", NA)))
  out <- matrix(blank, nrow = n, ncol = n)
  diag(out) <- x
  out
}

#' @rdname declare
#' @export
vec <- function(..., r. = 1, en. = NULL) {
  x <- av(...)
  if (length(x) == 0 | isEQ(r, 0)) {return(vector())}
  errs <- c(f0(               cmp_nnw_scl(r. ), NULL, "[r.] must be a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(is.null(en.) | cmp_chr_vec(en.), NULL, "[en.] must be NULL or a complete character vec (?cmp_chr_vec)." ))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (r. > 1) {x <- rep.int(x, r.)}
  if (idef(en.)) {
    en. <- av(strsplit(en., "|", fixed = T))
    if (length(x) != length(en.)) {stop(.errs("[en.] must be the same length as the vector resulting from atomizing (?av) [...]."))}
    if (any(en. == "")          ) {stop(.errs("[en.] contains a blank string (after splitting along pipes)."))}
    if (!.labs_ok(en.)          ) {stop(.errs("[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}
    names(x) <- en.
  }
  x
}

#' @rdname declare
#' @export
vec. <- function(...) {
  labs <- as_chr(match.call())                                                   # get a function call object and convert to character
  labs <- labs[2:nx(labs)]                                                       # remove the function call leaving the variables as named in the calling function
  nok <- ...length() > 0
  asc <- f0(!nok, T, all(sapply(list(...), atm_scl)))
  unq <- f0(!nok, T, length(labs) == length(unique(labs)))
  lab <- f0(!nok | !unq, T, .labs_ok(labs))
  errs <- c(f0(nok, NULL, "[...] is empty."),
            f0(unq, NULL, "[...] argument names must be uniquely."),
            f0(asc, NULL, "[...] arguments must be atomic and scalar (?atm_scl)."),
            f0(lab, NULL, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))
  if (!is.null(errs)) {stop(.errs(errs))}
  code <- paste0("c(",  paste0(paste(labs, "=", labs), collapse = ", "), ")")    # create the call [c(<var1> = <var1>, <var2> = <var2>, ...)]
  eval.parent(parse(text = code, n = 1))                                         # and evaluate it in the environment of the calling function
}

#' @rdname declare
#' @export
vec_na <- function(r) {rep.int(NA, r)}

#' @rdname declare
#' @export
vls <- function(..., en. = NULL) {
  en. <- f0(cmp_chr_vec(en.), av(strsplit(en., "|", fixed = T)), en.)
  dots <- list(...)
  n.dots <- length(dots)
  ok.en <- f0(inll(en.), T, f0(!cmp_chr_vec(en.), F, length(en.) == n.dots))
  errs <- c(f0(n.dots > 0, NULL, "[...] is empty."),
            f0(ok.en     , NULL, "[en.] must be NULL or match the number of arguments in [...]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!is.null(en.)) {if (!.labs_ok(en.)) {stop(.errs("element names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))}}
  if (idef(en.)) {names(dots) <- en.}
  dots
}

#' @rdname declare
#' @export
vls. <- function(...) {
  labs <- as_chr(match.call())                                                   # get a function call object and convert to character
  labs <- labs[2:nx(labs)]                                                       # remove the function call leaving the variables as named in the calling function
  nok <- ...length() > 0
  unq <- f0(!nok, T, length(labs) == length(unique(labs)))
  lab <- f0(!nok | !unq, T, .labs_ok(labs))
  errs <- c(f0(nok, NULL, "[...] is empty."),
            f0(unq, NULL, "[...] arguments must be uniquely named."),
            f0(lab, NULL, "[...] argument names must contain only unaccented English letters, numerals, periods, and underscores. They must begin with a letter or a period followed by a letter and must end with a letter or numeral."))
  if (!is.null(errs)) {stop(.errs(errs))}
  code <- paste0("list(", paste0(paste(labs, "=", labs), collapse = ", "), ")")  # create the call [list(<var1> = <var1>, <var2> = <var2>, ...)]
  eval.parent(parse(text = code, n = 1))                                         # and evaluate it in the environment of the calling function
}
