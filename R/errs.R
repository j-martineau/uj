#' @name err_check
#' @family errs
#' @title Error Management
#' @description Bank error messages in the immediate environment of a function
#'   to allow for exhaustive error checking before throwing an exception.
#'   Results in a combined, multiple-error message to be reported at the
#'   completion of all error checks.\tabular{ll}{
#'     FUNCTION      \tab WHAT IT DOES                                       \cr
#'     `err_check`   \tab \emph{Process any banked error messages}. Check for
#'                        banked error messages in the environment of the
#'                        function `gens` generations back in the call stack,
#'                        and if there are any, process them, stopping
#'                        execution. If there are none, take no action. To be
#'                        called after error checking is complete.           \cr
#'     `bank_err`    \tab \emph{Bank an arbitrary error message}. Bank the
#'                        error message in `...` in the environment of the
#'                        function `gens` generations back in the call stack.
#'                        The error message is constructed by
#'                        \link[=av]{atomizing} and collapsing `...` into a
#'                        character scalar.                                  \cr
#'     `bank_lgl`    \tab \emph{Check for logical scalar args}. Checks named
#'                        `...` arguments for whether they are scalar `TRUE` or
#'                        scalar `FALSE`. If `na = TRUE`, also allows atomic
#'                        scalar `NA`. If `extras` contains atomic values
#'                        (logical or not), also allows those values. For each
#'                        named argument that does not meet the requirements,
#'                        banks an error.                                   \cr
#'     `bank_not`    \tab \emph{Bank errors for `FALSE` args}. For each scalar
#'                        `FALSE` named `...` argument, create an error message
#'                        by collapsing unnamed `...` arguments into a character
#'                        scalar message template, banking the error message for
#'                        that named argument. The location where the name of a
#'                        `FALSE` named argument should occur in the message is
#'                        indicated by the escape sequence `'{@@}'`.         \cr
#'     `bank_pop`    \tab \emph{Bank errors for non-populated args}. For each
#'                         named `...` argument that is either `NULL` or empty
#'                         (of length `0`), banks an error message.          \cr
#'     `bank_funs`   \tab \emph{Bank errors for args not passing property
#'                        checking functions}. For each named `...` argument,
#'                        check whether it satisfies any of the property
#'                        functions named in `funs.`, and if not, bank an error
#'                        message. `funs. must be a character scalar containing
#'                        the names of one or more \link[ppp_funs]{property
#'                        functions}, separated by pipes. An argument passes the
#'                        test if calling any one of the functions results in a
#'                        value of `TRUE`.                                   \cr
#'     `bank_ppp`    \tab \emph{Bank errors for args not passing arbitrary
#'                        property checks}. More flexible but less efficient
#'                        than `bank_funs`. For each named `...` argument, bank
#'                        an error message if it does satisfy the
#'                        \link[ippp]{property specification} in `ppp`. If
#'                        `nas. = TRUE`, arguments may also be atomic scalar
#'                        `NA` values. `ppp.` must be a character scalar
#'                        containing one or more property combos. Property
#'                        combos are created by separating multiple properties
#'                        from \link[=all_props]{all_props()}. A property combo
#'                        may be a single property. Multiple property combos are
#'                        separated from each other using pipes. An argument
#'                        satisfies the property specification if it satisfies
#'                        any of the property combos.                        \cr
#'     `bank_vals`   \tab \emph{Bank errors for args with out-of-bounds values}.
#'                        Bank errors if atomic scalar arguments do not have one
#'                        of a list of values. For each named atomic scalar
#'                        `...` argument, check it against the remaining
#'                        (unnamed) atomic `...` arguments (which do not have to
#'                        be of the same mode), and if the named argument's
#'                        value does not match a value from any unnamed `...`
#'                        argument, bank an error message.                   \cr
#'     `bank_dots`   \tab \emph{Bank errors for `...` args}. Bank errors if
#'                        `...` arguments do not satisfy a property
#'                        specification, and optionally, if they are not named.
#'                        Check if each `...` argument satisfies the property
#'                        specification in `prop.` and if not, bank an error
#'                        message. If `named. = TRUE`, check whether all `...`
#'                        arguments are named, and if not banks an error
#'                        message.                                           \cr
#'     `bank_when`   \tab \emph{Bank errors for one arg conditional on the value
#'                        of another}. Bank errors conditionally on the value of
#'                        two arguments. Banks an error if the first named
#'                        atomic scalar `...` argument has a value contained in
#'                        `whens.` and the second named atomic scalar `...`
#'                        argument does not contain a value from `values.`.  \cr
#'     `bank_pats`   \tab \emph{Bank errors for args not matching any patterns
#'                        of properties}. Bank errors conditionally on the
#'                        pattern of properties of arguments in `...` matching
#'                        none of the specified patterns of associated
#'                        properties in `pats.`.                             \cr
#'     `bank_fail`   \tab \emph{Bank errors for args producing an error when
#'                        evaluated}.                                          }
#' @param x An R object.
#' @param gens. A \link[cmp_nnw_scl]{Complete non-negative whole-number scalar}
#'   indicating the number of generations back in the call stack in which to
#'   bank and/or check for error messages.
#' @param ... For `bank_err`: An arbitrary number of atomic arguments that when
#'   collapsed into a character scalar form an error message.
#'   \cr\cr
#'   For `bank_lgl`, `bank_pop`, `bank_funs`, `bank_prop`, `bank_pats`: An
#'   arbitrary number of named arguments to be error checked.
#'   \cr\cr
#'   For `bank_not`: An arbitrary number of named logical scalar arguments to be
#'   checked for `TRUE`-ness plus an arbitrary number of unnamed atomic
#'   arguments that when collapsed form an error message.
#'   \cr\cr
#'   For `bank_vals`: An arbitrary number of named atomic scalar arguments and
#'   an arbitrary number of unnamed atomic arguments holding valid values of the
#'   named arguments.
#'   \cr\cr
#'   For `bank_dots`: An arbitrary number of named or unnamed arguments to be
#'   error checked.
#'   \cr\cr
#'   For `bank_when`: Two named atomic scalars to be error checked.
#' @param nas. A non-`NA` logical scalar indicating whether atomic scalar `NA`
#'   values qualify.
#' @param extras `NUlL` or an \link[=icmp]{complete atomic object} containing
#'   additional atomic values that qualify as valid.
#' @param funs \link[=cmp_chr_scl]{Complete character scalar} containing a
#'   \link[=ppp_funs]{property function} name or multiple property function
#'   names separated by pipes, which are used to check if named arguments in
#'   `...` satisfy the property specification in any of the function names.
#' @param ppp A \link[=cmp_chr_scl]{complete character scalar} containing one or
#'   more property combos (combos are created by separating one or more
#'   \link[=all_props]{property values} with underscores. Multiple combos are
#'   separated by pipes.
#' @param named. A non-`NA` logical scalar indicating whether `...` arguments
#'   must be uniquely named without using blank strings.
#' @param whens.,values. \link[=ipop]{Populated atomic objects}.
#' @param pats. An \link[=ivls]{atomic vlist} containing multiple elements, each
#'   of which must be a \link[=cmp_chr_mvc]{complete character multivec} of the
#'   same length as the number of `...` arguments. Each element gives a pattern
#'   of valid properties for the argument in `...` in the form of one property
#'   specification per argument in `...` in the same order of those arguments.
#' @return `NULL`. Called for the side effect of banking and/or processing error
#'   messages.
#' @export
err_check <- function(gens. = 0) {
  ok.gens <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  if (!ok.gens) {stop("\n \u2022 [gens.] doesn't point to a function in the call stack.")}
  gens. <- gens. + 1
  bank.name <- "._ERR_._BANK_."
  errs <- exists(bank.name, envir = parent.frame(gens.), inherits = F)
  if (errs) {
    fun.name <- callers(gens.)
    err.bank <- get(bank.name, envir = parent.frame(gens.), inherits = F)
    err.bank <- paste0(paste0("\n \u2022 ", err.bank), collapse = "")
    err.bank <- paste0("\nIN [", fun.name, "]", err.bank)
    stop(err.bank)
  }
  NULL
}

#' @rdname err_check
#' @export
bank_err <- function(..., gens. = 0) {
  ok.gens <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  if (!ok.gens) {stop("\n \u2022 [gens.] doesn't point to a function in the call stack.")}
  gens. <- gens. + 1
  bank.name <- "._ERR_._BANK_."
  gens. <- gens. + 1
  err <- trimws(paste0(as.character(av(...)), collapse = ""), which = "both")
  ok.err <- length(err) > 0 & notEQ(err, "")
  if (!ok.err) {stop("\n \u2022 [...] has no atomic values")}
  err <- gsub("\"", "'", err, fixed = TRUE)
  if (!exists(bank.name, envir = parent.frame(gens.), inherits = F)) {err.bank <- NULL}
  else {err.bank <- get(bank.name, envir = parent.frame(gens.))}
  err.bank <- unique(c(err.bank, err))
  assign(bank.name, err.bank, envir = parent.frame(gens.))
  NULL
}

#' @rdname err_check
#' @export
bank_lgl <- function(..., nas. = F, extras. = NULL) {
  dots  <- named_dots(...)
  labs <- names(dots)
  n.dots <- ...length()
  n.labs <- length(labs)
  ok.0 <- ...length() > 0
  ok.labs <- f0(!ok.0, T, f0(n.dots != n.labs, F, f0(any(labs == ""), F, isEQ(labs, unique(labs)))))
  ok.lgl <- f0(n.dots == 0, T, all(sapply(dots, cmp_lgl_scl)))
  ok.nas <- isTF(nas.)
  ok.extras <- f0(inll(extras.), T, icmp(extras.) & is.atomic(extras.))
  errs <- c(f0(ok.0     , NULL, "\n \u2022 [...] is empty."),
            f0(ok.labs  , NULL, "\n \u2022 All arguments in [...] must be named uniquely without using blank strings."),
            f0(ok.lgl   , NULL, "\n \u2022 All arguments in [...] must be complete logical scalars (?cmp_lgl_scl)."),
            f0(ok.nas   , NULL, "\n \u2022 [nas.] must be TRUE or FALSE."),
            f0(ok.extras, NULL, "\n \u2022 [extras.] must be NULL or complete and atomic (?icmp)."))
  if (!is.null(errs)) {stop(errs)}
  has.extras <- idef(extras.)
  if (has.extras) {
    av.extras <- extras. <- av(extras.)
    n.extras <- length(av.extras)
    if (is.character(av.extras)) {av.extras <- paste0("\"", av.extras, "\"")}
    if      (n.extras == 1) {av.extras <- c(", or ", av.extras)}
    else if (n.extras == 2) {av.extras <- c(", ", av.extras[1], ", or ", av.extras[2])}
    else {av.extras <- c(", ", paste(av.extras[1:(n.extras - 1)], collapse = ", "), ", or ", av.extras[n.extras])}
  }
  else {av.extras <- ""}
  false.insert <- f0(nas. | has.extras, ", FALSE", " or FALSE")
  na.insert <- f0(nas., f0(has.extras, ", NA", ", or NA"), "")
  msg.insert <- c("must be TRUE", false.insert, na.insert, av.extras, ".")
  for (i in 1:nd) {
    dot <- ...elt(i)
    ok.dot <- isTF(dot)
    if (!ok.dot & nas.) {ok.dot <- isNa(dot)}
    if (!ok.dot & has.extras) {ok.dot <- isIN(dot, has.extras)}
    if (!ok.dot) {bank_err("[", labs[i], "] ", msg.insert, gens = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_not <- function(...) {
  named <- named_dots(...)
  blank <- unnamed_dots(...)
  error <- paste0(as.character(unlist(blank, T, F)), collapse = "")
  names <- names(named)
  v0 <- ...length() > 0
  vnn <- f0(!v0, T, length(named) > 0)
  vnb <- f0(!v0, T, length(blank) > 0)
  vnm <- f0(!vnn, T, !any(names == "") & isEQ(names, unique(names)))
  vlg <- f0(length(names) == 0, T, all(sapply(named, cmp_lgl_scl)))
  ver <- any(grepl("{@}", error, fixed = TRUE))
  err <- NULL
  if (!v0) {err <- c(err, "\n \u2022 [...] is empty.")}
  if (!vnn) {err <- c(err, "\n \u2022 At least one argument in [...] must be named.")}
  if (!vnb) {err <- c(err, "\n \u2022 At least one argument in [...] must be unnamed.")}
  if (!vnm) {err <- c(err, "\n \u2022 Named arguments in [...] must be uniquely named (without using a blank string as a name).")}
  if (!vlg) {err <- c(err, "\n \u2022 Named arguments in [...] must be complete logical scalars (?cmp_lgl_scl).")}
  if (!ver) {err <- c(err, "\n \u2022 An unnamed argument in [...] must contain the escape sequence '{@}' for inserting the names of named arguments.")}
  if (idef(err)) {stop(err)}
  for (i in 1:length(named)) {
    if (!named[[1]]) {
      err <- gsub("{@}", paste0("[", names[i], "]"), error)
      bank_err(err, gens = 1)
  }}
  NULL
}

#' @rdname err_check
#' @export
bank_pop <- function(...) {
  named <- named_dots(...)
  unnamed <- unnamed_dots(...)
  labs <- names(named)
  n.dots <- ...length()
  n.labs <- length(labs)
  n.unnamed <- length(unnamed)
  ok.n <- nd > 0
  ok.unnamed <- f0(!ok.n, T, n.unnamed == 0)
  ok.named <- f0(n.dots != n.labs, F, !any(labs == "") & isEQ(labs, unique(labs)))
  errs <- c(f0(ok.n      , NULL, "\n \u2022 [...] is empty."),
            f0(ok.unnamed, NULL, "\n \u2022 All arguments in [...] must be named."),
            f0(ok.named  , NULL, "\n \u2022 Names of arguments in [...] must be unique without using blank strings."))
  if (!is.null(errs)) {stop(errs)}
  for (i in 1:length(named)) {if (inil(named[[1]])) {bank_err("[", labs[i], "] is NULL or empty.", gens. = 1)}}
  NULL
}

#' @rdname err_check
#' @export
bank_funs <- function(funs., ...) {
  if (cmp_chr_vec(funs.)) {
    funs. <- av(strsplit(funs., "|", TRUE))
    len3 <- nchar(funs.) == 3
    funs.[len3] <- paste0("i", funs.[len3])
  }
  else {stop(" \u2022 [funs.] must be a complete character vec (?cmp_chr_vec).")}
  labs <- ...names()
  n.dots <- ...length()
  n.labs <- length(labs)
  ok.n <- n.dots > 0
  ok.labs <- f0(n.dots != n.labs, F, !any(n.labs == "") & isEQ(n.labs, unique(n.labs)))
  ok.funs <- all(sapply(funs., is_ppp_fun))
  errs <- c(f0(ok.n   , NULL, "\n \u2022 [...] is empty."),
            f0(ok.labs, NULL, "\n \u2022 All arguments in [...] must be uniquely named without using blank strings."),
            f0(ok.funs, NULL, "\n \u2022 [funs.] contains a function name not found in ppp_funs()."))
  if (!is.null(errs)) {stop(errs)}
  errs <- paste0("[", labs, "] must be ", alt_ppp_concise(funs.))
  for (i in 1:n.dots) {
    ok <- FALSE
    for (fun in funs.) {ok <- ok | eval(parse(text = paste0(fun, "(...elt(i))")))}
    if (!ok) {bank_err(errs[i], gens. = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_ppp <- function(ppp., ..., nas. = F) {
  labs <- ...names()
  n.dots <- ...length()
  n.labs <- length(labs)
  ok.ppp <- f0(!cmp_chr_scl(ppp.), F, is_valid_ppp(ppp.))
  ok.n <- n.dots > 0
  ok.labs <- f0(n.dots != n.labs, F, !any(labs == "") & isEQ(labs, unique(labs)))
  ok.nas <- isTF(nas.)
  errs <- c(f0(ok.ppp , NULL, "\n \u2022 [ppp.] is not a valid property specification."),
            f0(ok.n   , NULL, "\n \u2022 [...] is empty."),
            f0(ok.labs, NULL, "\n \u2022 All arguments in [...] must be uniquely named without using blank strings."),
            f0(ok.nas , NULL, "\n \u2022 [nas.] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  errs <- paste0("[", labs, "] must be ", alt_ppp_concise(ppp.), ".")
  for (i in 1:n.dots) {
    val <- F
    if (nas.) {val <- isNa(...elt(i))}
    if (!val) {val <- ippp(...elt(i), ppp.)}
    if (!val) {bank_err(errs[i], gens. = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_vals <- function(...) {
  oxford_vals <- function(xx) {
    nn <- length(xx)
    if (nn == 1) {xx}
    else if (nn == 2) {paste0(xx[1], " or ", xx[2])}
    else {paste0(paste0(xx[1:(nn - 1)], collapse = ", "), ", or ", xx[nn])}
  }
  args <- named_dots(...)
  vals <- unnamed_dots(...)
  labs <- names(args)
  ok.0 <- ...length() > 0
  ok.args <- f0(!ok.0, T, length(args) > 0)
  ok.vals <- f0(!ok.0, T, length(args) > 0)
  ok.labs <- f0(!ok.0 | !ok.args, T, !any(labs == "") & isEQ(labs, unique(labs)))
  ok.atm <- f0(!ok.0, T, all(sapply(args, ipop), sapply(args, iatm)))
  ok.pop <- f0(!ok.vals, T, all(sapply(vals, ipop), sapply(vals, iatm)))
  errs <- c(f0(ok.0   , NULL, "\n \u2022 [...] is empty."),
            f0(ok.args, NULL, "\n \u2022 At least one argument in [...] must be named."),
            f0(ok.vals, NULL, "\n \u2022 At least one argument in [...] must be unnamed."),
            f0(ok.labs, NULL, "\n \u2022 Named arguments in [...] must be uniquely named without using blank strings."),
            f0(ok.atm , NULL, "\n \u2022 Named arguments in [...] must be non-empty and atomic."),
            f0(ok.pop , NULL, "\n \u2022 Unnamed arguments in [...] must be non-empty and atomic."))
  if (!is.null(errs)) {stop(errs)}
  values <- NULL
  for (val in vals) {
    if (any(is.na(val))) {values <- c(values, 'NA')}
    val <- val[!is.na(val)]
    if (length(val) > 0) {
      if (is.character(val)) {values <- c(values, paste0("\"", val, "\""))}
      else {values <- c(values, as.character(val))}
    }
  }
  values <- oxford_vals(unique(values))
  for (i in 1:length(args)) {
    arg <- args[[i]]
    lab <- labs[i]
    valid <- F
    for (j in 1:length(vals)) {if (!valid) {valid <- isIN(arg, vals[[j]])}}
    if (!valid) {bank_err("[", lab, "] must be ", values, ".", gens. = 1)}
  }
  NULL
}

#' @rdname err_check
#' @export
bank_dots <- function(ppp., ..., named. = F) {
  dots  <- list(...)
  named <- named_dots(...)
  blank <- unnamed_dots(...)
  labs <- names(named)
  ok.ppp <- f0(!cmp_chr_scl(ppp), F, is_valid_ppp(ppp))
  ok.0 <- ...length() > 0
  ok.named <- isTF(named.)
  ok.labs <- f0(!ok.0 | !ok.named, T, f0(!named., T, f0(length(blank) > 0, F, f0(any(names(labs) == ""), F, isEQ(labs, unique(labs))))))
  errs <- c(f0(ok.ppp  , NULL, "\n \u2022 [ppp.] must be a character scalar containing a valid property specification (?is_valid_ppp)."),
            f0(ok.0    , NULL, "\n \u2022 [...] is empty."),
            f0(ok.named, NULL, "\n \u2022 [named.] must be TRUE or FALSE."),
            f0(ok.labs , NULL, "\n \u2022 When [named. = TRUE], all arguments in [...] must be uniquely named without using blank strings."))
  if (!is.null(errs)) {stop(errs)}
  ok.dots <- sapply(dots, ippp, ppp = ppp.)
  if (!ok.dots) {bank_err("All arguments in [...] must be ", alt_ppp_concise(ppp.), ".", gens. = 1)}
  NULL
}

#' @rdname err_check
#' @export
bank_when <- function(whens., values., ...) {
  oxford_vals <- function(xx) {
    nn <- length(xx)
    if (nn == 1) {xx}
    else if (nn == 2) {paste0(xx[1], " or ", xx[2])}
    else {paste0(paste0(xx[1:(nn - 1)], collapse = ", "), ", or ", xx[nn])}
  }
  labs <- ...names()
  ok.whens <- ipop(whens.) & iatm(whens.)
  ok.values <- ipop(values.) & iatm(values.)
  ok.n <- ...length() == 2
  ok.labs <- f0(!ok.n, T, f0(length(labs) == 2, F, f0(any(labs == ""), F, labs[1] != labs[2])))
  ok.dots <- iscl(..1) & iscl(..2)
  ok.whens2 <- f0(ok.whens & ok.values, compatible(whens. , ..1), T)
  ok.values2 <- f0(ok.whens & ok.values, compatible(values., ..2), T)
  errs <- c(f0(ok.whens  , NULL, "\n \u2022 [whens.] must be non-empty and atomic."),
            f0(ok.values , NULL, "\n \u2022 [values.] must be non-empty and atomic."),
            f0(ok.n      , NULL, "\n \u2022 There must be two arguments in [...]"),
            f0(ok.labs   , NULL, "\n \u2022 Arguments in [...] must be uniquely named without using blank strings."),
            f0(ok.whens2 , NULL, "\n \u2022 [whens.] and [..1] are not of compatible (?compatible) modes."),
            f0(ok.values2, NULL, "\n \u2022 [values.] and [..2] are not of compatible (?compatible) modes."),
            f0(ok.dots   , NULL, "\n \u2022 Both arguments in [...] must be atomic and scalar (?iscl)."))
  if (!is.null(errs)) {stop(errs)}
  labs1 <- paste0("[", labs[1], "]")
  labs2 <- paste0("[", labs[2], "]")
  when <- ..1
  value <- ..2
  if (is.character(when)) {when <- paste0("\"", when, "\"")}
  if (is.character(value)) {value <- paste0("\"", value, "\"")}
  if (when %in% whens.) {if (!(value %in% values.)) {
    when <- oxford_vals(when)
    value <- oxford_vals(value)
    bank_err("When ", labs1, " is ", when, ", ", labs2, " must be ", value, ".", gens. = 1)
  }}
  NULL
}

#' @rdname err_check
#' @export
bank_pats <- function(pats., ...) {
  dots <- named_dots(...)
  blank <- unnamed_dots(...)
  labs <- names(dots)
  n.dots <- length(dots)
  n.blank <- length(blank)
  n.pats <- length(pats.)
  ok.vls <- cmp_chr_vls(pats.)
  pat.ns <- f0(ok.vls, lengths(pats.), 0)
  nu.labs <- length(unique(labs))
  ok.pats <- f0(ok.vls, all(sapply(av(pats.), is_valid_ppp)), F)
  ok.ns <- all(pat.ns == n.dots)
  ok.nu <- n.dots == nu.labs
  errs <- c(f0(ok.vls     , NULL, "\n \u2022 [pats.] must be a complate atomic vlist (?cmp_vls)."),
            f0(ok.nu      , NULL, "\n \u2022 Arguments in [...] must be uniquely named."),
            f0(ok.ns      , NULL, "\n \u2022 Elements of [pats.] must have length equal to the number of arguments in [...]."),
            f0(ok.pats    , NULL, "\n \u2022 All values in [pats.] must be valid property specification (see is_valid_xxx)"),
            f0(n.pats < 2 , NULL, "\n \u2022 The number of patterns in [pats.] is less than 2."),
            f0(n.dots < 2 , NULL, "\n \u2022 The number of arguments in [...] is less than 2."),
            f0(n.blank > 0, NULL, "\n \u2022 All arguments in [...] must be named."))
  if (!is.null(errs)) {stop(errs)}
  nc <- rep.int(0, n.dots)
  for (i in 1:n.pats) {
    pat <- pats.[[i]]
    valid <- T
    for (j in 1:n.dots) {
      var <- labs[j]
      ppp <- pat[j]
      nc[j] <- max(nc[j], nchar(ppp))
      if (valid) {valid <- valid & ippp(var, ppp)}
    }
    if (valid) {return(NULL)}
  }
  nc <- nc + 2
  width <- nchar(paste0("PATTERN ", n.pats))
  for (i in 1:n.dots) {
    n <- nc[i]
    labs[i] <- pad(labs[i], n = n)
    ipats <- pats.[[i]]
    for (j in 1:n.pats) {
      jpat <- ipats[j]
      jpat <- pad(paste0("\"", jpat, "\""), n = n)
      ipats[j] <- jpat
    }
    lab <- pad(paste0("PATTERN ", j), n = width)
    ipats <- paste0(c(lab, ipats), collapse = " | ")
    pats.[[i]] <- ipats
  }
  lab <- pad("ARGUMENT", n = width)
  vars <- paste0(c(lab, labs), collapse = " | ")
  err <- c("The pattern of values of ", ox_and(paste0("[", labs, "] ")), "does not match any of the valid patterns in [pats.], given as", "\n     ", paste0(c(vars, av(pats.)), collapse = "\n     "))
  bank_err(err, gens. = 1)
}

#' @rdname err_check
#' @export
bank_fail <- function(x) {
  x <- failsafe(x)
  if (isERR(x)) {bank_err(paste0(av(x), collapse = ""), gens = 1)}
  NULL
}
