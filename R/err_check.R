.errs <- function(x) {g0(p0("\n \u2022 ", x))}
.has_dots <- function(...) {...length() > 0}
.pop_dots <- function(...) {all(lengths(list(...)) > 0)}
.unq_named <- function(...) {x <- ...names(); length(x) == length(unique(x))}
.all_named <- function(...) {f0(length(...names()) == 0, F, f0(length(...names()) != ...length(), F, f0(!any(...names() == ""), F, .unq_named(...))))}
.ox_vals <- function(x, join) {
  n <- length(x)
  if (n == 1) {x}
  else if (n == 2) {paste0(x[1], " ", join, " ", x[2])}
  else {paste0(paste0(x[1:(n - 1)], collapse = ", "), ", ", join, " ", x[n])}
}

#' @family errs
#' @title Error checking, banking, and processing
#' @description Bank error messages in the immediate environment of a function to allow for exhaustive error checking before throwing an exception. Results in a possibly multiple-error, accumulated message to be processed upon completion of error checking.
#' \cr\cr
#' **General purpose functions**
#' \tabular{ll}{
#'   \cr `err_check`   \tab Checks for banked error messages in the environment of the function `gens.` generations back in the call stack, and if there are any, processes them, stopping execution. If there are none, takes no action.
#'   \cr `bank_err`    \tab Banks an arbitrary error message (built by collapsing `...` args) in the environment of the function `gens.` generations back in the call stack.
#' }
#' **Condition-based functions**
#' \tabular{ll}{
#'   \cr **Function**   \tab **Condition for banking an error message**
#'   \cr `bank_dots`    \tab A `...` arg\eqn{^a} fails to satisfy the \link[=is_prop_spec]{property spec} in the `spec.` argument.
#'   \cr `bank_chars`   \tab A named `...` arg contains characters not supplied in the `chars.` argument.
#'   \cr `bank_spec`    \tab A named `...` arg fails to satisfy the \link[=is_prop_spec]{property spec} in the `spec.` argument\eqn{^b}.
#'   \cr `bank_funs`    \tab A named `...` arg fails to satisfy *any* of the \link[=prop_funs]{property function(s)} named in the `funs.` argument.
#'   \cr `bank_vals`    \tab A named `...` arg contains values not supplied in the `vals.` argument.
#'   \cr `bank_fail`    \tab A named `...` arg produces an error when submitted to \code{\link[base]{identity}}.
#'   \cr `bank_lgl`     \tab A named `...` arg is neither `TRUE`, `FALSE`, `NA`\eqn{^c}, nor contained in the `extras.` argument\eqn{^d}.
#'   \cr `bank_not`     \tab A named `...` arg is `FALSE`\eqn{^e}.
#'   \cr `bank_pop`     \tab A named `...` arg is either `NULL` or otherwise of length `0`.
#'   \cr `bank_when`    \tab The first named `...` arg is the `n`-th value in the `whens.` argument, but the second *is not* the `n`-th value in the `values.` argument.
#' }
#'     \eqn{^{a.}} Named when `named. = TRUE`.
#' \cr \eqn{^{b.}} May be scalar `NA` when `nas. = TRUE`.
#' \cr \eqn{^{c.}} When `nas. = TRUE`.
#' \cr \eqn{^{d.}} When `extras.` is non-`NULL`.
#' \cr \eqn{^{e.}} Collapses *unnamed* `...` args to an error message template, replacing the escape sequence `'{@@}'` with the named `...` arg's name.
#' @section The `...` arguments: Arguments supplied in `...` differ across functions in terms of whether they are named, how many named and/or unnamed `...` args there are, and their property requirement as follows:
#' \tabular{llll}{
#'       **Function** \tab    **Number of**        \tab `...` **args**       \tab   **Property**
#'   \cr **Name**     \tab    **(named)**          \tab  **(unnamed)**       \tab   **requirement**
#'   \cr `bank_err`   \tab    `0+`\eqn{^a}         \tab  `0+`\eqn{^a}        \tab   \link[=pop_atm]{populated and atomic}
#'   \cr `bank_when`  \tab    `2`                  \tab  `2`                 \tab   \link[=atm_scl]{atomic scalar}
#'   \cr `bank_lgl`   \tab    `1+`                 \tab  `1+`                \tab   \link[=atm_scl]{logical scalar}
#'   \cr `bank_not`   \tab    `1+`                 \tab  `0`                 \tab   logical scalar
#'   \cr `bank_chars` \tab    `1+`                 \tab  `1+`                \tab   `atomic`\eqn{^b}
#'   \cr `bank_vals`  \tab    `1+`                 \tab  `1+`                \tab   `atomic`\eqn{^b}
#'   \cr `bank_dots`  \tab    `1+`\eqn{^c} or `0+` \tab  `0`\eqn{^c} or `0+` \tab   *none*
#'   \cr `bank_fail`  \tab    `1+`                 \tab  `0`                 \tab   *none*
#'   \cr `bank_funs`  \tab    `1+`                 \tab  `0`                 \tab   *none*
#'   \cr `bank_spec`  \tab    `1+`                 \tab  `0`                 \tab   *none*
#'   \cr `bank_pop`   \tab    `1+`                 \tab  `0`                 \tab   *none*
#' }
#'     \eqn{^{a.}} At least `1 ...` arg in total.
#' \cr \eqn{^{b.}} When `atm. = TRUE`.
#' \cr \eqn{^{c.}} When `named. = TRUE`.
#' @param gens. A \link[=cmp_nnw_scl]{complete non-negative whole-number scalar} indicating the number of generations back in the call stack in which to bank and/or check for error messages.
#' @param ... Differs by function in terms of whether they are named, how many there are, and their \link[=ppp]{property requirements} as described in the section entitled *the* `...` *arguments*.
#' @param nas. `TRUE` or `FALSE` indicating whether `NA` values qualify as `'logical'`.
#' @param funs. A \link[=cmp_chr_vec]{complete character vec} containing `1` or more \link[=prop_funs]{property function} names.
#' @param spec. A \link[=cmp_chr_scl]{complete character scalar} containing a \link[=is_prop_spec]{property spec}.
#' @param named. `TRUE` or `FALSE` indicating whether `...` args must uniquely named without using `""`.
#' @param whens. A \link[=pop_atm]{populated atomic object} of length `length(values.)`.
#' @param values. A \link[=cmp_atm]{complete atomic object} of length `length(whens.)`.
#' @param extras. `NULL` or a \link[=cmp_atm]{complete atomic object} containing additional valid values.
#' @return `NULL`
#' @export
err_check <- function(gens. = 0) {
  ok.g <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  if (!ok.g) {stop(.errs("[gens.] doesn't point to a function in the call stack."))}
  gens. <- gens. + 1
  name <- "._ERR_._BANK_."
  errs <- exists(name, envir = parent.frame(gens.), inherits = F)
  if (errs) {
    func <- callers(gens.)
    bank <- get(name, envir = parent.frame(gens.), inherits = F)
    bank <- .errs(bank)
    bank <- paste0("\nIN [", func, "]", bank)
    rm(list = name, envir = parent.frame(gens.), inherits = F)
    stop(bank)
  }
  NULL
}

#' @rdname err_check
#' @export
bank_err <- function(..., gens. = 0) {
  mssg <- trimws(paste0(as.character(av(...)), collapse = ""), which = "both")
  mssg <- gsub("\"", "'", mssg, fixed = TRUE)
  ok.g <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  ok.m <- f0(length(mssg) == 0, F, nchar(mssg) > 0)
  errs <- c(f0(ok.g, NULL, "[gens.] doesn't point to a function in the call stack."),
            f0(ok.m, NULL, "[...] args contain no atomic values."                  ))
  if (!is.null(errs)) {stop(.errs(errs))}
  name <- "._ERR_._BANK_."
  gens. <- gens. + 1
  if (!exists(name, envir = parent.frame(gens.), inherits = F)) {bank <- NULL}
  else {bank <- get(name, envir = parent.frame(gens.))}
  bank <- unique(c(bank, mssg))
  assign(name, bank, envir = parent.frame(gens.))
  NULL
}

#' @rdname err_check
#' @export
bank_lgl <- function(..., nas. = F, extras. = NULL) {
  ok.hd <- .has_dots(...)
  ok.an <- .all_named(...)
  ok.na <- isTF(nas.)
  ok.ex <- f0(is.null(extras.), T, cmp_atm(extras.))
  errs <- c(f0(ok.hd, F, "There are no [...] args"),
            f0(ok.an, F, "All [...] args must be uniquely named without using \"\"."),
            f0(ok.na, F, "[na.] must be TRUE or FALSE."),
            f0(ok.ex, F, "[extras.] must be NULL or complete and atomic (?cmp_atm)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  x <- list(...)
  if (!all(sapply(x, atm_scl))) {stop(.errs("All [...] args must be atomic and scalar (?atm_scl)."))}
  ok.x <- sapply(x, isTRUE)
  ok.x <- ok.x | sapply(x, isFALSE)
  ok.x <- ok.x & f0(!nas.           , TRUE, sapply(x, isNAS        ))
  ok.x <- ok.x & f0(is.null(extras.), TRUE, sapply(x, isIN, extras.))
  if (!all(ok.x)) {
    labs <- paste0("[", ...names()[!ok.x], "]")
    mult <- f0(length(labs) > 1, "s", "")
    MULT <- f0(length(labs) > 1, "", "s")
    vals <- c("TRUE", "FALSE")
    vals <- c(vals, f0(nas., "NA", NULL))
    vals <- c(vals, f0(is.null(extras.), NULL, f0(is.character(extras.), paste0("'", extras., "'"), as.character(extras.))))
    err <- paste0("Argument", mult, .ox_vals(labs, "and"), "contain", MULT, " one or more values not in {", .ox_vals(vals, "and"), "}.")
    bank_err(err, gens. = 1)
  }
  NULL
}

#' @rdname err_check
#' @export
bank_not <- function(...) {
  named <- named_dots(...)
  blank <- unnamed_dots(...)
  mssg <- paste0(as.character(unlist(blank, T, F)), collapse = "")
  labs <- names(named)
  ok.nnd <- length(named) > 0
  ok.nbd <- length(blank) > 0
  ok.bln <- f0(!ok.nnd, T, !any(names == ""))
  ok.unq <- length(labs) == length(unique(labs))
  ok.msg <- any(grepl("{@}", mssg, fixed = TRUE))
  errs <- c(f0(ok.nnd, NULL, "There are no named [...] args."),
            f0(ok.nbd, NULL, "There are no unnamed [...] args."),
            f0(ok.bln, NULL, "Names for [...] args may not be blank (\"\")."),
            f0(ok.unq, NULL, "Named [...] args must be uniquely named."),
            f0(ok.msg, NULL, "At least 1 unnamed [...] arg must contain the escape sequence '{@} for inserting the names of named [...] args."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 1:length(named)) {if (!named[[1]]) {
    err <- gsub("{@}", paste0("[", names[i], "]"), mssg)
    bank_err(err, gens = 1)
  }}
  NULL
}

#' @rdname err_check
#' @export
bank_pop <- function(...) {
  named <- named_dots(...)
  blank <- unnamed_dots(...)
  labs <- names(named)
  labs <- names(named)
  ok.has <- (length(named) + length(blank)) > 0
  ok.nmd <- length(blank) == 0
  ok.nms <- f0(!ok.nmd, T, !any(labs == ""))
  ok.unq <- f0(!ok.nmd, T, length(labs) == length(unique(labs)))
  errs <- c(f0(ok.has, NULL, "There are no [...] args."),
            f0(ok.nmd, NULL, "All [...] args must be named."),
            f0(ok.nms, NULL, "[...] arg names may not be blank strings (\"\")."),
            f0(ok.unq, NULL, "Each [...] arg name must be unique."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 1:length(named)) {if (inil(named[[1]])) {bank_err("[", labs[i], "] is NULL or empty.", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_funs <- function(funs., ..., vals. = NULL) {
  if (cmp_chr_vec(funs.)) {
    funs. <- av(strsplit(funs., "|", TRUE))
    len3 <- nchar(funs.) == 3
    funs.[len3] <- paste0("i", funs.[len3])
  } else {stop(.errs("[funs.] must be a complete character vec (?cmp_chr_vec)."))}
  labs <- ...names()
  n.dots <- ...length()
  ok.dots <- n.dots > 0
  ok.labs <- .all_named(...)
  ok.funs <- all(sapply(funs., is_prop_fun))
  ok.vals <- f0(is.null(vals.), T, pop_atm(vals.))
  errs <- c(f0(ok.dots, NULL, "There are no [...] arguments."),
            f0(ok.labs, NULL, "All [...] args must be uniquely named without using blank strings."),
            f0(ok.funs, NULL, "[funs.] contains a function name not found in prop_funs()."),
            f0(ok.vals, NULL, "[vals.] must be NULL or complete and atomic (?cmp_atm)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  errs <- paste0("[", labs, "] must be ", spec_concise(funs.))
  for (i in 1:n.dots) {
    ok <- FALSE
    for (fun in funs.) {
      ok <- ok | eval(parse(text = paste0(fun, "(...elt(i))")))
      ok <- ok & f0(is.null(vals.), T, all(...elt(i) %in% vals.))
    }
    if (!ok) {bank_err(errs[i], gens. = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_spec <- function(spec., ..., nas. = F) {
  labs <- ...names()
  n.dots <- ...length()
  ok.spec <- f0(!cmp_chr_scl(spec.), F, is_prop_spec(spec.))
  ok.has <- .has_dots(...)
  ok.nmd <- .all_named(...)
  ok.nas <- isTRUE(nas.) | isFALSE(nas.)
  errs <- c(f0(ok.spec, NULL, "[spec.] is not a valid property specification (?is_prop_spec)."),
            f0(ok.has , NULL, "[...] is empty."),
            f0(ok.nmd , NULL, "All [...] args must be uniquely named without using blank strings."),
            f0(ok.nas , NULL, "[nas.] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  errs <- paste0("[", labs, "] must be ", spec_concise(spec.), ".")
  for (i in 1:n.dots) {
    val <- F
    if (nas.) {val <- isNAS(...elt(i))}
    if (!val) {val <- ippp(...elt(i), spec.)}
    if (!val) {bank_err(errs[i], gens. = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_vals <- function(vals., ..., atm. = TRUE, nas. = FALSE) {
  labs <- ...names()
  n.dots <- ...length()
  ok.vals <- cmp_atm(vals.)
  ok.has <- .has_dots(...)
  ok.nmd <- .all_named(...)
  ok.atm <- isTRUE(atm.) | isFALSE(atm.)
  ok.nas <- isTRUE(nas.) | isFALSE(nas.)
  errs <- c(f0(ok.vals, NULL, "[vals.] must be complete and atomic (?cmp_atm)."),
            f0(ok.has , NULL, "[...] is empty."),
            f0(ok.nmd , NULL, "All [...] args must be uniquely named without using blank strings."),
            f0(ok.atm , NULL, "[atm.] must be TRUE or FALSE."),
            f0(ok.nas , NULL, "[nas.] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  dots <- list(...)
  xatm <- sapply(dots, pop_atm)
  if (atm. & !all(xatm)) {stop(.errs("When [atm. = TRUE], all [...] args must be populated and atomic (?pop_atm)."))}
  if (!atm.) {
    xvls <- sapply(dots, atm_vls)
    xdtf <- sapply(dots, atm_dtf)
    if (!all(xatm | xvls | xdtf)) {stop(.errs("All [...] args must be populated and atomic (?pop_atm), atomic vlists (?atm_vls), or atomic data.frames (?atm_dtf)."))}
  }
  .valid <- function(y) {
    if (is.atomic(y)) {all(sapply(av(y), isIN, vals.))}
    else if (is.data.frame(y)) {all(apply(y, 2, .valid))}
    else {all(sapply(y, .valid))}
  }
  for (i in 1:n.dots) {if (!.valid(dots[i])) {bank_err("[", labs[i], "] contains 1 or more values not in [vals.].", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_chars <- function(chars., ..., atm. = TRUE) {
  labs <- ...names()
  n.dots <- ...length()
  ok.chr <- cmp_chr(chars.)
  ok.has <- .has_dots(...)
  ok.nmd <- .all_named(...)
  ok.atm <- isTRUE(atm.) | isFALSE(atm.)
  errs <- c(f0(ok.chr, NULL, "[chars.] must be complete and atomic (?cmp_atm)."),
            f0(ok.has , NULL, "[...] is empty."),
            f0(ok.nmd , NULL, "All [...] args must be uniquely named without using blank strings."),
            f0(ok.atm , NULL, "[atm.] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  dots <- list(...)
  xatm <- sapply(dots, pop_atm)
  if (atm. & !all(xatm)) {stop(.errs("When [atm. = TRUE], all [...] args must be populated and atomic (?pop_atm)."))}
  if (!atm.) {
    xvls <- sapply(dots, atm_vls)
    xdtf <- sapply(dots, atm_dtf)
    if (!all(xatm | xvls | xdtf)) {stop(.errs("All [...] args must be populated and atomic (?pop_atm), atomic vlists (?atm_vls), or atomic data.frames (?atm_dtf)."))}
  }
  chars.scl <- paste0(unique(av(chars.)), collapse = "")
  chars. <- strsplit(chars.scl, "", fixed = T)
  .valid <- function(y) {
    if (is.atomic(y)) {
      y <- strsplit(paste0(av(y), collapse = ""), "", fixed = T)
      all(y %in% chars.)
    }
    else if (is.data.frame(y)) {all(apply(y, 2, .valid))}
    else {all(sapply(y, .valid))}
  }
  for (i in 1:n.dots) {if (!.valid(dots[i])) {bank_err("[", labs[i], "] contains 1 or more characters not in {", chars.scl, "}.", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_dots <- function(spec., ..., named. = F) {
  ok.spc <- f0(!cmp_chr_scl(spec.), F, is_prop_spec(spec.))
  ok.has <- .has_dots(...)
  ok.nmd <- isTRUE(named.) | isFALSE(named.)
  ok.nms <- f0(!ok.nmd | isFALSE(named.), T, .all_named(...))
  errs <- c(f0(ok.spc, NULL, "[spec.] must be complete and atomic (?cmp_atm)."),
            f0(ok.has, NULL, "[...] is empty."),
            f0(ok.nmd, NULL, "[named.] must be TRUE or FALSE."),
            f0(ok.nms, NULL, "All [...] args must be uniquely named without using blank strings."))
  if (!is.null(errs)) {stop(.errs(errs))}
  ok.dots <- sapply(list(...), ippp, spec = spec.)
  if (!all(ok.dots)) {bank_err("All [...] args must be ", spec_concise(spec.), ".", gens. = 1)}
  NULL
}

#' @rdname err_check
#' @export
bank_when <- function(whens., values., ...) {
  labs <- ...names()
  ok.whens <- ipop(whens.) & iatm(whens.)
  ok.values <- ipop(values.) & iatm(values.)
  ok.n <- ...length() == 2
  ok.labs <- f0(!ok.n, T, f0(length(labs) == 2, F, f0(any(labs == ""), F, labs[1] != labs[2])))
  ok.dots <- iscl(..1) & iscl(..2)
  ok.whens2 <- f0(ok.whens & ok.values, compatible(whens. , ..1), T)
  ok.values2 <- f0(ok.whens & ok.values, compatible(values., ..2), T)
  ok.lens <- length(whens.) == length(values.)
  errs <- c(f0(ok.whens  , NULL, "[whens.] must be non-empty and atomic."),
            f0(ok.values , NULL, "[values.] must be non-empty and atomic."),
            f0(ok.n      , NULL, "There must be two args in [...]"),
            f0(ok.labs   , NULL, "[...] args must be uniquely named without using blank strings."),
            f0(ok.whens2 , NULL, "[whens.] and [..1] are of incompatible (?compatible) modes."),
            f0(ok.values2, NULL, "[values.] and [..2] are of incompatible (?compatible) modes."),
            f0(ok.dots   , NULL, "Both args in [...] must be atomic and scalar (?iscl)."),
            f0(ok.lens   , NULL, "[whens.] and [values.] must be of the same length."))
  if (!is.null(errs)) {stop(.errs(errs))}
  labs1 <- paste0("[", labs[1], "]")
  labs2 <- paste0("[", labs[2], "]")
  when <- ..1
  value <- ..2
  i <- which(sapply(whens., isEQ, y = when))
  if (length(i) > 0) {
    match <- values.[i[1]]
    if (!isEQ(value, match)) {
      if (is.character(match)) {match <- paste0("'", match, "'")}
      bank_err("When ", labs1, " is ", when, ", ", labs2, " must be ", match, ".", gens. = 1)
    }
  }
  NULL
}

#' @rdname err_check
#' @export
bank_fail <- function(...) {
  labs <- ...names()
  nl <- length(unique(labs))
  n <- ...length()
  ok.x <- n > 0
  ok.lb <- f0(!ok.x, T, f0(nl != n, F, !any(labs == "")))
  errs <- c(f0(ok.x , NULL, "[...] is empty."),
            f0(ok.lb, NULL, "All args in [...] must be uniquely named without using blank strings."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 1:n) {
    x <- failsafe(...elt(i))
    if (isERR(x)) {bank_err("evaluating arg [", labs[i], "] produced an error: ", paste0(av(x), collapse = ""), gens = 1)}
  }
  NULL
}
