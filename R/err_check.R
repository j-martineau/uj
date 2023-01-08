.errs <- function(x) {uj::g0(uj::p0("\n \u2022 ", x))}
.has_dots <- function(...) {base::...length() > 0}
.pop_dots <- function(...) {base::all(base::lengths(base::list(...)) > 0)}
.unq_named <- function(...) {x <- base::...names(); base::length(x) == base::length(base::unique(x))}
.all_named <- function(...) {uj::f0(base::length(base::...names()) == 0, F, f0(base::length(base::...names()) != base::...length(), F, uj::f0(base::any(base::...names() == ""), F, uj:::.unq_named(...))))}
.ox_vals <- function(x, join) {
  n <- base::length(x)
  if (n == 1) {x}
  else if (n == 2) {base::paste0(x[1], " ", join, " ", x[2])}
  else {base::paste0(base::paste0(x[1:(n - 1)], collapse = ", "), ", ", join, " ", x[n])}
}

#' @encoding UTF-8
#' @family properties
#' @family errs
#' @title Error checking, banking, and processing
#' @description Bank error messages in the immediate environment of a function to allow for exhaustive error checking before throwing an exception. Results in a possibly multiple-error, accumulated message to be processed upon completion of error checking.
#' \cr
#' \cr
#' **General purpose functions**
#' \tabular{rl}{
#'     `banked_errs`   \tab Retrieves the bank of error message stored in the environment of the function `gens.` generations back in the call stack.
#'   \cr               \tab  
#'   \cr `err_check`   \tab Checks for banked error messages in the environment of the function `gens.` generations back in the call stack, and if there are any, processes them, communicates them, purges them, and stops execution. If there are none, takes no action.
#'   \cr               \tab  
#'   \cr  `bank_err`   \tab Banks an arbitrary error message (built by collapsing `...` args) in the environment of the function `gens.` generations back in the call stack.
#' }
#' \cr
#' **Condition-based functions**
#' \tabular{rl}{
#'          **Function**   \tab **Condition for banking an error message**
#'   \cr                   \tab  
#'   \cr   `bank_nas_or`   \tab A named `...` arg is neither `NULL` nor satisfies any property function in the `funs.` arg.
#'   \cr                   \tab  
#'   \cr   `bank_nll_or`   \tab A named `...` arg is neither scalar `NA` nor satisfies any property function in the `funs.` arg.
#'   \cr                   \tab  
#'   \cr    `bank_class`   \tab A named `...` arg is not of any class named in the `class.` arg.
#'   \cr                   \tab  
#'   \cr    `bank_class`   \tab A named `...` arg is not of any class named in the `class.` arg.
#'   \cr                   \tab  
#'   \cr    `bank_chars`   \tab A named `...` arg contains characters not supplied in the `chars.` arg.
#'   \cr                   \tab  
#'   \cr     `bank_when`   \tab The first named `...` arg is the `n`-th value in the `whens.` arg, but the second *is not* the `n`-th value in the `values.` arg.
#'   \cr                   \tab  
#'   \cr     `bank_dots`   \tab A `...` arg\eqn{^a} fails to satisfy the \link[=is_prop_spec]{property spec} in the `spec.` arg.
#'   \cr                   \tab  
#'   \cr     `bank_spec`   \tab A named `...` arg fails to satisfy the \link[=is_prop_spec]{property spec} in the `spec.` argument\eqn{^b}.
#'   \cr                   \tab  
#'   \cr     `bank_funs`   \tab A named `...` arg fails to satisfy *any* of the \link[=prop_funs]{property function(s)} named in the `funs.` arg.
#'   \cr                   \tab  
#'   \cr     `bank_vals`   \tab A named `...` arg contains values not supplied in the `vals.` arg.
#'   \cr                   \tab  
#'   \cr     `bank_fail`   \tab A named `...` arg produces an error when submitted to \code{\link[base]{identity}}.
#'   \cr                   \tab  
#'   \cr      `bank_lgl`   \tab A named `...` arg is neither `TRUE`, `FALSE`, `NA`\eqn{^c}, nor contained in the `extras.` arg\eqn{^d}.
#'   \cr                   \tab  
#'   \cr      `bank_not`   \tab A named `...` arg is `FALSE`\eqn{^e}.
#'   \cr                   \tab  
#'   \cr      `bank_pop`   \tab A named `...` arg is either `NULL` or otherwise of length `0`.
#' }
#'    \eqn{^{a.}} Named when `named. = TRUE`.
#' \cr
#' \cr    \eqn{^{b.}} May also be scalar `NA` when `nas. = TRUE`.
#' \cr
#' \cr    \eqn{^{c.}} When `nas. = TRUE`.
#' \cr
#' \cr    \eqn{^{d.}} When `extras.` is non-`NULL`.
#' \cr
#' \cr    \eqn{^{e.}} Collapses *unnamed* `...` args to an error message template, replacing the escape sequence `'{@@}'` with the named `...` arg's name.
#' @section The `...` arguments: Arguments supplied in `...` differ across functions in terms of whether they are named, how many named and/or unnamed `...` args there are, and their \link[=ppp]{property requirements} as follows:
#' \tabular{rlll}{
#'         **Function** \tab    **Number of**        \tab `...` **args**       \tab   **Property**
#'   \cr       **Name** \tab    **(named)**          \tab  **(unnamed)**       \tab   **requirement**
#'   \cr   `bank_class` \tab    `1+`                 \tab  `0`                 \tab   *none*
#'   \cr   `bank_chars` \tab    `1+`                 \tab  `1+`                \tab   `atomic`\eqn{^b}
#'   \cr    `bank_when` \tab    `2`                  \tab  `2`                 \tab   \link[=atm_scl]{atomic scalar}
#'   \cr    `bank_vals` \tab    `1+`                 \tab  `1+`                \tab   `atomic`\eqn{^b}
#'   \cr    `bank_dots` \tab    `1+`\eqn{^c} or `0+` \tab  `0`\eqn{^c} or `0+` \tab   *none*
#'   \cr    `bank_fail` \tab    `1+`                 \tab  `0`                 \tab   *none*
#'   \cr    `bank_funs` \tab    `1+`                 \tab  `0`                 \tab   *none*
#'   \cr    `bank_spec` \tab    `1+`                 \tab  `0`                 \tab   *none*
#'   \cr     `bank_err` \tab    `0+`\eqn{^a}         \tab  `0+`\eqn{^a}        \tab   \link[=pop_atm]{populated and atomic}
#'   \cr     `bank_lgl` \tab    `1+`                 \tab  `1+`                \tab   \link[=atm_scl]{logical scalar}
#'   \cr     `bank_not` \tab    `1+`                 \tab  `0`                 \tab   logical scalar
#'   \cr     `bank_pop` \tab    `1+`                 \tab  `0`                 \tab   *none*
#' }
#'    \eqn{^{a.}} At least `1 ...` arg in total.
#' \cr    \eqn{^{b.}} When `atm. = TRUE`.
#' \cr    \eqn{^{c.}} When `named. = TRUE`.
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
  ok.g <- uj::f0(!uj::cmp_nnw_scl(gens.), F, gens. <= uj::ncallers() - 1)
  if (!ok.g) {stop(uj:::.errs("[gens.] doesn't point to a function in the call stack."))}
  gens. <- gens. + 1
  name <- "._ERR_._BANK_."
  errs <- base::exists(name, envir = base::parent.frame(gens.), inherits = F)
  if (errs) {
    func <- uj::callers(gens.)
    bank <- base::get(name, envir = base::parent.frame(gens.), inherits = F)
    bank <- uj:::.errs(bank)
    bank <- base::paste0("\nIN [", func, "]", bank)
    base::rm(list = name, envir = base::parent.frame(gens.), inherits = F)
    stop(bank)
  }
  NULL
}

#' @rdname err_check
#' @export
banked_errs <- function(gens. = 0) {
  ok.g <- uj::f0(!uj::cmp_nnw_scl(gens.), F, gens. <= uj::ncallers() - 1)
  if (!ok.g) {stop(.errs("[gens.] doesn't point to a function in the call stack."))}
  gens. <- gens. + 1
  name <- "._ERR_._BANK_."
  errs <- base::exists(name, envir = base::parent.frame(gens.), inherits = F)
  if (errs) {
    func <- uj::callers(gens.)
    bank <- base::get(name, envir = base::parent.frame(gens.), inherits = F)
    bank <- uj:::.errs(bank)
    bank <- base::paste0("\nIN [", func, "]", bank)
  } else {bank <- NULL}
  bank
}

#' @rdname err_check
#' @export
bank_err <- function(..., gens. = 0) {
  mssg <- base::trimws(base::paste0(base::as.character(uj::av(...)), collapse = ""), which = "both")
  mssg <- base::gsub("\"", "'", mssg, fixed = TRUE)
  ok.g <- uj::f0(!uj::cmp_nnw_scl(gens.), F, gens. <= uj::ncallers() - 1)
  ok.m <- uj::f0(base::length(mssg) == 0, F, base::nchar(mssg) > 0)
  errs <- base::c(uj::f0(ok.g, NULL, "[gens.] doesn't point to a function in the call stack."),
                  uj::f0(ok.m, NULL, "[...] args contain no atomic values."                  ))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  name <- "._ERR_._BANK_."
  gens. <- gens. + 1
  if (!base::exists(name, envir = base::parent.frame(gens.), inherits = F)) {bank <- NULL}
  else {bank <- base::get(name, envir = base::parent.frame(gens.))}
  bank <- base::unique(c(bank, mssg))
  base::assign(name, bank, envir = base::parent.frame(gens.))
  NULL
}

#' @rdname err_check
#' @export
bank_lgl <- function(..., nas. = F, extras. = NULL) {
  ok.hd <- uj:::.has_dots(...)
  ok.an <- uj:::.all_named(...)
  ok.na <- uj::isTF(nas.)
  ok.ex <- uj::f0(base::is.null(extras.), T, uj::cmp_atm(extras.))
  errs <- base::c(uj::f0(ok.hd, NULL, "There are no [...] args"),
                  uj::f0(ok.an, NULL, "All [...] args must be uniquely named without using \"\"."),
                  uj::f0(ok.na, NULL, "[na.] must be TRUE or FALSE."),
                  uj::f0(ok.ex, NULL, "[extras.] must be NULL or complete and atomic (?cmp_atm)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x <- base::list(...)
  if (!base::all(base::sapply(x, atm_scl))) {stop(uj:::.errs("All [...] args must be atomic and scalar (?atm_scl)."))}
  ok.x <- base::sapply(x, base::isTRUE)
  ok.x <- ok.x | base::sapply(x, base::isFALSE)
  ok.x <- ok.x & uj::f0(!nas., TRUE, base::sapply(x, uj::isNAS))
  ok.x <- ok.x & uj::f0(base::is.null(extras.), TRUE, base::sapply(x, uj::isIN, extras.))
  if (!base::all(ok.x)) {
    labs <- base::paste0("[", base::...names()[!ok.x], "]")
    mult <- uj::f0(base::length(labs) > 1, "s", "")
    MULT <- uj::f0(base::length(labs) > 1, "", "s")
    vals <- base::c("TRUE", "FALSE")
    vals <- base::c(vals, uj::f0(nas., "NA", NULL))
    vals <- base::c(vals, uj::f0(base::is.null(extras.), NULL, uj::f0(base::is.character(extras.), base::paste0("'", extras., "'"), base::as.character(extras.))))
    err <- base::paste0("Argument", mult, uj:::.ox_vals(labs, "and"), "contain", MULT, " one or more values not in {", uj:::.ox_vals(vals, "and"), "}.")
    uj::bank_err(err, gens. = 1)
  }
  NULL
}

#' @rdname err_check
#' @export
bank_nll_or <- function(funs., ..., vals. = NULL) {
  if (uj::cmp_chr_vec(funs.)) {
    funs. <- uj::av(base::strsplit(funs., "|", TRUE))
    len3 <- base::nchar(funs.) == 3
    funs.[len3] <- base::paste0("i", funs.[len3])
  } else {stop(uj:::.errs("[funs.] must be a complete character vec (?cmp_chr_vec)."))}
  labs <- base::...names()
  n.dots <- base::...length()
  ok.dots <- n.dots > 0
  ok.labs <- uj::f0(!ok.dots, T, uj:::.all_named(...))
  ok.funs <- base::all(base::sapply(funs., is_prop_fun))
  ok.vals <- uj::f0(base::is.null(vals.), T, uj::pop_atm(vals.))
  errs <- base::c(uj::f0(ok.dots, NULL, "[...] arguments must be supplied."),
                  uj::f0(ok.labs, NULL, "[...] args must be uniquely named without using blank strings."),
                  uj::f0(ok.funs, NULL, "[funs.] contains a function name not found in prop_funs()."),
                  uj::f0(ok.vals, NULL, "[vals.] must be NULL or complete and atomic (?cmp_atm)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  errs <- base::paste0("[", labs, "] must be NULL or ", uj::spec_concise(funs.))
  for (i in 1:n.dots) {if (!base::is.null(base::...elt(i))) {
    ok <- FALSE
    for (fun in funs.) {
      ok <- ok | base::eval(base::parse(text = base::paste0(fun, "(base::...elt(i))")))
      ok <- ok & uj::f0(base::is.null(vals.), T, base::all(base::...elt(i) %in% vals.))
    }
    if (!ok) {uj::bank_err(errs[i], gens. = 1)}
  }}
  NULL
}

#' @rdname err_check
#' @export
bank_nas_or <- function(funs., ..., vals. = NULL) {
  if (uj::cmp_chr_vec(funs.)) {
    funs. <- uj::av(base::strsplit(funs., "|", TRUE))
    len3 <- base::nchar(funs.) == 3
    funs.[len3] <- base::paste0("i", funs.[len3])
  } else {stop(uj:::.errs("[funs.] must be a complete character vec (?cmp_chr_vec)."))}
  labs <- base::...names()
  n.dots <- base::...length()
  ok.dots <- n.dots > 0
  ok.labs <- uj::f0(!ok.dots, T, uj:::.all_named(...))
  ok.funs <- base::all(base::sapply(funs., is_prop_fun))
  ok.vals <- uj::f0(base::is.null(vals.), T, uj::pop_atm(vals.))
  errs <- base::c(uj::f0(ok.dots, NULL, "[...] arguments must be supplied."),
                  uj::f0(ok.labs, NULL, "[...] args must be uniquely named without using blank strings."),
                  uj::f0(ok.funs, NULL, "[funs.] contains a function name not found in prop_funs()."),
                  uj::f0(ok.vals, NULL, "[vals.] must be NULL or complete and atomic (?cmp_atm)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  errs <- base::paste0("[", labs, "] must be NULL or ", uj::spec_concise(funs.))
  for (i in 1:n.dots) {if (!uj::isNAS(base::...elt(i))) {
    ok <- FALSE
    for (fun in funs.) {
      ok <- ok | base::eval(base::parse(text = base::paste0(fun, "(...elt(i))")))
      ok <- ok & f0(base::is.null(vals.), T, base::all(base::...elt(i) %in% vals.))
    }
    if (!ok) {uj::bank_err(errs[i], gens. = 1)}
  }}
  NULL
}

#' @rdname err_check
#' @export
bank_not <- function(...) {
  named <- uj::named_dots(...)
  blank <- uj::unnamed_dots(...)
  mssg <- base::paste0(base::as.character(base::unlist(blank, T, F)), collapse = "")
  labs <- base::names(named)
  ok.nnd <- base::length(named) > 0
  ok.nbd <- base::length(blank) > 0
  ok.bln <- uj::f0(!ok.nnd, T, !base::any(labs == ""))
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  ok.msg <- base::any(base::grepl("{@}", mssg, fixed = TRUE))
  errs <- base::c(uj::f0(ok.nnd, NULL, "There are no named [...] args."),
                  uj::f0(ok.nbd, NULL, "There are no unnamed [...] args."),
                  uj::f0(ok.bln, NULL, "Names for [...] args may not be blank (\"\")."),
                  uj::f0(ok.unq, NULL, "Named [...] args must be uniquely named."),
                  uj::f0(ok.msg, NULL, "At least 1 unnamed [...] arg must contain the escape sequence '{@} for inserting the names of named [...] args."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  for (i in 1:base::length(named)) {if (!named[[1]]) {
    err <- base::gsub("{@}", base::paste0("[", labs[i], "]"), mssg)
    uj::bank_err(err, gens = 1)
  }}
  NULL
}

#' @rdname err_check
#' @export
bank_class <- function(class., ...) {
  named <- uj::named_dots(...)
  blank <- uj::unnamed_dots(...)
  labs <- base::names(named)
  labs <- base::names(named)
  ok.cls <- uj::cmp_chr_vec(class.)
  ok.has <- (base::length(named) + base::length(blank)) > 0
  ok.nmd <- base::length(blank) == 0
  ok.nms <- uj::f0(!ok.nmd, T, !base::any(labs == ""))
  ok.unq <- uj::f0(!ok.nmd, T, base::length(labs) == base::length(base::unique(labs)))
  errs <- base::c(uj::f0(ok.cls, NULL, "[class.] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(ok.has, NULL, "There are no [...] args."),
                  uj::f0(ok.nmd, NULL, "All [...] args must be named."),
                  uj::f0(ok.nms, NULL, "[...] arg names may not be blank strings (\"\")."),
                  uj::f0(ok.unq, NULL, "[...] arg names must be unique."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  class. <- uj::av(base::strsplit(class., "|", fixed = T))
  msg <- base::paste0("'", class., "'")
  n <- base::length(msg)
  if      (n == 1) {msg <- base::paste0("class ", msg)}
  else if (n == 2) {msg <- base::paste0("class ", msg[1], " or ", msg[2])}
  else {msg <- base::paste0("any class in {", base::paste0(msg, collapse = ", "), "}")}
  for (i in 1:base::length(named)) {if (!base::any(class. %in% base::class(named[[i]]))) {uj::bank_err("[", labs[i], "] must be of ", msg, ".", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_pop <- function(...) {
  named <- uj::named_dots(...)
  blank <- uj::unnamed_dots(...)
  labs <- base::names(named)
  labs <- base::names(named)
  ok.has <- (base::length(named) + base::length(blank)) > 0
  ok.nmd <- base::length(blank) == 0
  ok.nms <- uj::f0(!ok.nmd, T, !base::any(labs == ""))
  ok.unq <- uj::f0(!ok.nmd, T, base::length(labs) == base::length(base::unique(labs)))
  errs <- base::c(uj::f0(ok.has, NULL, "There are no [...] args."),
                  uj::f0(ok.nmd, NULL, "All [...] args must be named."),
                  uj::f0(ok.nms, NULL, "[...] arg names may not be blank strings (\"\")."),
                  uj::f0(ok.unq, NULL, "[...] arg names must be unique."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  for (i in 1:base::length(named)) {if (base::length(named[i]) == 0) {uj::bank_err("[", labs[i], "] is NULL or empty.", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_funs <- function(funs., ..., vals. = NULL) {
  if (uj::cmp_chr_vec(funs.)) {
    funs. <- uj::av(base::strsplit(funs., "|", TRUE))
    len3 <- base::nchar(funs.) == 3
    funs.[len3] <- base::paste0("i", funs.[len3])
  } else {stop(uj:::.errs("[funs.] must be a complete character vec (?cmp_chr_vec)."))}
  labs <- base::...names()
  n.dots <- base::...length()
  ok.dots <- n.dots > 0
  ok.labs <- uj::f0(!ok.dots, T, uj:::.all_named(...))
  ok.funs <- base::all(base::sapply(funs., is_prop_fun))
  ok.vals <- uj::f0(base::is.null(vals.), T, uj::pop_atm(vals.))
  errs <- base::c(uj::f0(ok.dots, NULL, "[...] arguments must be supplied."),
                  uj::f0(ok.labs, NULL, "[...] args must be uniquely named without using blank strings."),
                  uj::f0(ok.funs, NULL, "[funs.] contains a function name not found in prop_funs()."),
                  uj::f0(ok.vals, NULL, "[vals.] must be NULL or complete and atomic (?cmp_atm)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  errs <- base::paste0("[", labs, "] must have the following properties: ", uj::spec_concise(funs.))
  for (i in 1:n.dots) {
    ok <- FALSE
    for (fun in funs.) {
      ok <- ok | base::eval(base::parse(text = base::paste0(fun, "(base::...elt(i))")))
      ok <- ok & uj::f0(base::is.null(vals.), T, base::all(base::...elt(i) %in% vals.))
    }
    if (!ok) {uj::bank_err(errs[i], gens. = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_spec <- function(spec., ..., nas. = F) {
  labs <- base::...names()
  n.dots <- base::...length()
  ok.spec <- uj::f0(!uj::cmp_chr_scl(spec.), F, uj::is_prop_spec(spec.))
  ok.has <- uj:::.has_dots(...)
  ok.nmd <- uj:::.all_named(...)
  ok.nas <- base::isTRUE(nas.) | base::isFALSE(nas.)
  errs <- base::c(uj::f0(ok.spec, NULL, "[spec.] is not a valid property specification (?is_prop_spec)."),
                  uj::f0(ok.has , NULL, "[...] is empty."),
                  uj::f0(ok.nmd , NULL, "All [...] args must be uniquely named without using blank strings."),
                  uj::f0(ok.nas , NULL, "[nas.] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  errs <- base::paste0("[", labs, "] must be ", uj::spec_concise(spec.), ".")
  for (i in 1:n.dots) {
    val <- F
    if (nas.) {val <- uj::isNAS(base::...elt(i))}
    if (!val) {val <- uj::ippp(base::...elt(i), spec.)}
    if (!val) {uj::bank_err(errs[i], gens. = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_vals <- function(vals., ..., atm. = TRUE, nas. = FALSE) {
  labs <- base::...names()
  n.dots <- base::...length()
  ok.vals <- uj::cmp_atm(vals.)
  ok.has <- uj:::.has_dots(...)
  ok.nmd <- uj:::.all_named(...)
  ok.atm <- base::isTRUE(atm.) | base::isFALSE(atm.)
  ok.nas <- base::isTRUE(nas.) | base::isFALSE(nas.)
  errs <- base::c(uj::f0(ok.vals, NULL, "[vals.] must be complete and atomic (?cmp_atm)."),
                  uj::f0(ok.has , NULL, "[...] is empty."),
                  uj::f0(ok.nmd , NULL, "All [...] args must be uniquely named without using blank strings."),
                  uj::f0(ok.atm , NULL, "[atm.] must be TRUE or FALSE."),
                  uj::f0(ok.nas , NULL, "[nas.] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  dots <- base::list(...)
  xatm <- base::sapply(dots, pop_atm)
  if (atm. & !base::all(xatm)) {stop(uj:::.errs("When [atm. = TRUE], all [...] args must be populated and atomic (?pop_atm)."))}
  if (!atm.) {
    xvls <- base::sapply(dots, atm_vls)
    xdtf <- base::sapply(dots, atm_dtf)
    if (!base::all(xatm | xvls | xdtf)) {stop(uj:::.errs("All [...] args must be populated and atomic (?pop_atm), atomic vlists (?atm_vls), or atomic data.frames (?atm_dtf)."))}
  }
  .valid <- function(y) {
    if (base::is.atomic(y)) {base::all(base::sapply(uj::av(y), uj::isIN, vals.))}
    else if (base::is.data.frame(y)) {base::all(base::apply(y, 2, .valid))}
    else {base::all(base::sapply(y, .valid))}
  }
  for (i in 1:n.dots) {if (!.valid(dots[[i]])) {uj::bank_err("[", labs[i], "] contains 1 or more values not in [vals.].", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_chars <- function(chars., ..., atm. = TRUE) {
  labs <- base::...names()
  n.dots <- base::...length()
  ok.chr <- uj::cmp_chr(chars.)
  ok.has <- uj:::.has_dots(...)
  ok.nmd <- uj:::L.all_named(...)
  ok.atm <- base::isTRUE(atm.) | base::isFALSE(atm.)
  errs <- base::c(uj::f0(ok.chr, NULL, "[chars.] must be complete and atomic (?cmp_atm)."),
                  uj::f0(ok.has , NULL, "[...] is empty."),
                  uj::f0(ok.nmd , NULL, "All [...] args must be uniquely named without using blank strings."),
                  uj::f0(ok.atm , NULL, "[atm.] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  dots <- base::list(...)
  xatm <- base::sapply(dots, pop_atm)
  if (atm. & !base::all(xatm)) {stop(uj:::.errs("When [atm. = TRUE], all [...] args must be populated and atomic (?pop_atm)."))}
  if (!atm.) {
    xvls <- base::sapply(dots, atm_vls)
    xdtf <- base::sapply(dots, atm_dtf)
    if (!base::all(xatm | xvls | xdtf)) {stop(uj:::.errs("All [...] args must be populated and atomic (?pop_atm), atomic vlists (?atm_vls), or atomic data.frames (?atm_dtf)."))}
  }
  chars.scl <- base::paste0(base::unique(uj::av(chars.)), collapse = "")
  chars. <- av(base::strsplit(chars.scl, "", fixed = T))
  .valid <- function(y) {
    if (base::is.atomic(y)) {
      y <- uj::av(base::strsplit(base::paste0(uj::av(y), collapse = ""), "", fixed = T))
      base::all(y %in% chars.)
    }
    else if (base::is.data.frame(y)) {base::all(base::apply(y, 2, .valid))}
    else {base::all(base::sapply(y, .valid))}
  }
  for (i in 1:n.dots) {if (!.valid(dots[[i]])) {uj::bank_err("[", labs[i], "] contains 1 or more characters not in { ", chars.scl, " }.", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_dots <- function(spec., ..., named. = F) {
  ok.spc <- uj::f0(!uj::cmp_chr_scl(spec.), F, uj::is_prop_spec(spec.))
  ok.has <- uj:::.has_dots(...)
  ok.nmd <- base::isTRUE(named.) | base::isFALSE(named.)
  ok.nms <- uj::f0(!ok.nmd | base::isFALSE(named.), T, uj:::.all_named(...))
  errs <- base::c(uj::f0(ok.spc, NULL, "[spec.] must be complete and atomic (?cmp_atm)."),
                  uj::f0(ok.has, NULL, "[...] is empty."),
                  uj::f0(ok.nmd, NULL, "[named.] must be TRUE or FALSE."),
                  uj::f0(ok.nms, NULL, "All [...] args must be uniquely named without using blank strings."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  ok.dots <- base::sapply(base::list(...), uj::ippp, spec = spec.)
  if (!base::all(ok.dots)) {uj::bank_err("All [...] args must be ", uj::spec_concise(spec.), ".", gens. = 1)}
  NULL
}

#' @rdname err_check
#' @export
bank_when <- function(whens., values., ...) {
  labs <- base::...names()
  ok.whens <- uj::ipop(whens.) & uj::iatm(whens.)
  ok.values <- uj::ipop(values.) & uj::iatm(values.)
  ok.n <- base::...length() == 2
  ok.labs <- uj::f0(!ok.n, T, uj::f0(base::length(labs) == 2, F, uj::f0(base::any(labs == ""), F, labs[1] != labs[2])))
  ok.dots <- uj::iscl(..1) & uj::iscl(..2)
  ok.whens2 <- uj::f0(ok.whens & ok.values, uj::compatible(whens. , ..1), T)
  ok.values2 <- uj::f0(ok.whens & ok.values, uj::compatible(values., ..2), T)
  ok.lens <- base::length(whens.) == base::length(values.)
  errs <- base::c(uj::f0(ok.whens  , NULL, "[whens.] must be non-empty and atomic."),
                  uj::f0(ok.values , NULL, "[values.] must be non-empty and atomic."),
                  uj::f0(ok.n      , NULL, "There must be two args in [...]"),
                  uj::f0(ok.labs   , NULL, "[...] args must be uniquely named without using blank strings."),
                  uj::f0(ok.whens2 , NULL, "[whens.] and [..1] are of incompatible (?compatible) modes."),
                  uj::f0(ok.values2, NULL, "[values.] and [..2] are of incompatible (?compatible) modes."),
                  uj::f0(ok.dots   , NULL, "Both args in [...] must be atomic and scalar (?iscl)."),
                  uj::f0(ok.lens   , NULL, "[whens.] and [values.] must be of the same length."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  labs1 <- base::paste0("[", labs[1], "]")
  labs2 <- base::paste0("[", labs[2], "]")
  when <- ..1
  value <- ..2
  i <- base::which(base::sapply(whens., uj::isEQ, y = when))
  if (base::length(i) > 0) {
    match <- values.[i[1]]
    if (!uj::isEQ(value, match)) {
      if (base::is.character(match)) {match <- base::paste0("'", match, "'")}
      uj::bank_err("When ", labs1, " is ", when, ", ", labs2, " must be ", match, ".", gens. = 1)
    }
  }
  NULL
}

#' @rdname err_check
#' @export
bank_fail <- function(...) {
  labs <- base::...names()
  nl <- base::length(base::unique(labs))
  n <- base::...length()
  ok.x <- n > 0
  ok.lb <- uj::f0(!ok.x, T, uj::f0(nl != n, F, !base::any(labs == "")))
  errs <- base::c(uj::f0(ok.x , NULL, "[...] is empty."),
                  uj::f0(ok.lb, NULL, "All args in [...] must be uniquely named without using blank strings."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  for (i in 1:n) {
    x <- uj::failsafe(base::...elt(i))
    if (uj::isERR(x)) {uj::bank_err("evaluating arg [", labs[i], "] produced an error: ", base::paste0(uj::av(x), collapse = ""), gens = 1)}
  }
  NULL
}
