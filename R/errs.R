#' @name errs
#' @title Error Management
#' @description Bank error messages in the immediate environment of a function
#'   to allow for exhaustive error checking before throwing an exception.
#'   Results in a combined, multiple-error message to be reported at the
#'   completion of all error checks.
#' @param gens. A non-negative whole-number scalar indicating the number of
#'   generations back in the call stack in which to bank and/or check for error
#'   messages.
#' @param ... For \code{bank_err}, an arbitrary number of atomic arguments that
#'   when collapsed into a character scalar form an error message.
#'   \cr\cr
#'   For \code{bank_lgl}, \code{bank_pop}, \code{bank_funs}, \code{bank_prop},
#'   and \code{bank_pats}, an arbitrary number of named arguments to be error
#'   checked.
#'   \cr\cr
#'   For \code{bank_not} both an arbitrary number of named logical scalar
#'   arguments to be checked for \code{TRUE}-ness and an arbitrary number of
#'   unnamed atomic arguments that when collapsed form an error message.
#'   \cr\cr
#'   For \code{bank_vals}, an arbitrary number of named atomic scalar arguments
#'   and an arbitrary number of unamed atomic arguments holding valid values of
#'   the named arguments.
#'   \cr\cr
#'   For \code{bank_dots}, an arbitrary number of named or unnamed arguments to
#'   be error checked.
#'   \cr\cr
#'   For \code{bank_when}, two named atomic scalar arguments to be error
#'   checked.
#'   \cr\cr
#'   For \code{bank_when}, two named atomic scalar arguments to be error
#'   checked.
#' @param nas. \code{TRUE} or \code{FALSE} indicating whether atomic scalar
#'   \code{NA} values qualify.
#' @param extras. \code{NUlL} or an complete atomic object containing additional
#'   atomic values that qualify as valid.
#' @param funs. A character scalar containing a
#'   \link[=xxx_funs]{property function} name or multiple property function
#'   names separated by pipes, which are used to check if named arguments in
#'   code{...} satisfy the property specification in any of the function names.
#' @param xxx. A character scalar containing one or more property combos
#'   (combos are created by separating one or more \link[=xxx_vals]{property
#'   values} with underscores. Multiple combos are separated by pipes.
#' @param named. \code{TRUE} or \code{FALSE} indicating whether arguments in
#'   \code{...} must be uniquely named without using blank strings.
#' @param when.,value. Non-empty atomic objects.
#' @param pats. An atomic vlist containing multiple elements, each of which must
#'   be a complete character mvect of the same length as the number of arguments
#'   in \code{...}. Each element gives a pattern of valid properties for the
#'   argument in \code{...} in the form of one property specification per
#'   argument in \code{...} in the same order of those arguments.
#' @return \code{NULL}. Called for the side effect of banking and/or processing
#'   error messages.
#' @export
errs <- NULL

oxford_vals. <- function(x.) {
  n. <- length(x.)
  if (n. == 1) {x.}
  else if (n. == 2) {paste0(x.[1], " or ", x.[2])}
  else {paste0(paste0(x.[1:(n. - 1)], collapse = ", "), ", or ", x.[n.])}
}

#' @describeIn errs Check for banked error messages in the environment of the
#'   function \code{gens.} generations back in the call stack, and if there are
#'   any, process them, stopping execution. If there are none, take no action.
#'   Should be called after all error checking has been completed.
#' @export
err_check <- function(gens. = 0) {
  vg. <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  if (!vg.) {stop("\n • [gens.] doesn't point to a function in the call stack.")}
  gens. <- gens. + 1
  lab. <- "._ERR_._BANK_."
  errs. <- exists(lab., envir = parent.frame(gens.), inherits = F)
  if (errs.) {
    func. <- callers(gens.)
    bank. <- get(lab., envir = parent.frame(gens.), inherits = F)
    bank. <- paste0(paste0("\n • ", bank.), collapse = "")
    bank. <- paste0("\nIN [", func., "]", bank.)
    stop(bank.)
  }
  NULL
}

#' @describeIn errs Banks the error message in \code{...} in the environment of
#'   the function \code{gens.} generations back in the call stack. The error
#'   message is constructed by atomizing and gluing all arguments in \code{...}
#'   into a character scalar.
#' @export
bank_err <- function(..., gens. = 0) {
  VG <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  if (!VG) {stop("\n * [gens.] doesn't point to a function in the call stack.")}
  gens. <- gens. + 1
  Name  <- "._ERR_._BANK_."
  gens. <- gens. + 1
  Err   <- trimws(paste0(as.character(av(...)), collapse = ""), which = "both")
  VE    <- length(Err) > 0 & notEQ(Err, "")
  if (!VE) {stop("\n • [...] has no atomic values")}
  Err <- gsub("\"", "'", Err, fixed = TRUE)
  if (!exists(Name, envir = parent.frame(gens.), inherits = F)) {Bank <- NULL}
  else {Bank <- get(Name, envir = parent.frame(gens.))}
  Bank <- unique(c(Bank, Err))
  assign(Name, Bank, envir = parent.frame(gens.))
  NULL
}

#' @describeIn errs Checks the named arguments in \code{...} for whether they
#'   are atomic scalar \code{TRUE} or atomic scalar \code{FALSE}. If \code{na. =
#'   TRUE}, also allows atomic scalar \code{NA}. If \code{extras.} contains
#'   atomic values (logical or not), also allows those values. For each named
#'   argument that does not meet the requirements, banks an error.
#' @export
bank_lgl <- function(..., na. = F, extras. = NULL) {
  dots.  <- named_dots(...)
  labs. <- names(dots.)
  nd. <- ...length()
  n. <- length(labs.)
  v0. <- ...length() > 0
  vl. <- f0(!v0., T, f0(nd. != n., F,
         f0(any(labs. == ""), F, isEQ(labs., unique(labs.)))))
  vlg. <- f0(n. == 0, T, all(sapply(dots., cmp_lgl_scl)))
  vna. <- isTF(na.)
  ve. <- f0(inll(extras.), T, cmp_atm(extras.))
  err. <- NULL
  if (!v0. ) {err. <- c(err., "\n • [...] is empty.")}
  if (!vl. ) {err. <- c(err., "\n • All arguments in [...] must be named uniquely without using blank strings.")}
  if (!vlg.) {err. <- c(err., "\n • All arguments in [...] must be non-NA logical scalars.")}
  if (!vna.) {err. <- c(err., "\n • [na.] must be TRUE or FALSE.")}
  if (!ve. ) {err. <- c(err., "\n • [extras.] must be NULL or complete and atomic.")}
  if (idef(err.)) {stop(err.)}
  ex. <- idef(extras.)
  if (ex.) {
    xav. <- extras. <- av(extras.)
    nex.  <- length(xav.)
    if (is.character(xav.)) {xav. <- paste0("\"", xav., "\"")}
    if      (nex. == 1) {xav. <- c(", or ", xav.)}
    else if (nex. == 2) {xav. <- c(", ", xav.[1], ", or ", xav.[2])}
    else {xav. <- c(", ", paste(xav.[1:(nex. - 1)], collapse = ", "), ", or ", xav.[nex.])}
  }
  else {xav. <- ""}
  xf. <- f0(na. | ex., ", FALSE", " or FALSE")
  xna. <- f0(na., f0(ex., ", NA", ", or NA"), "")
  xmsg. <- c("must be TRUE", xf., xna., xav., ".")
  for (i in 1:nd.) {
    x.  <- ...elt(i)
    vx. <- isTF(x.)
    if (!vx. & na.) {vx. <- isNa(x.)}
    if (!vx. & ex.) {vx. <- isIN(x., ex.)}
    if (!vx.) {bank_err("[", labs.[i], "] ", xmsg., gens. = 1)}
  }
  NULL
}

#' @describeIn errs For each named argument in \code{...} that is \code{FALSE},
#'   creates an error message by collapsing the remaining (unnamed) arguments in
#'   \code{...} into a character scalar, and banks the error message for that
#'   named argument. The location where the name of a \code{FALSE} named
#'   argument should occur in the message is indicated by the escape sequence
#'   \code{'{@@}'}.
#' @export
bank_not <- function(...) {
  named. <- named_dots(...)
  blank. <- unnamed_dots(...)
  error. <- paste0(as.character(unlist(blank., T, F)), collapse = "")
  names. <- names(named.)
  v0. <- ...length() > 0
  vnn. <- f0(!v0., T, length(named.) > 0)
  vnb. <- f0(!v0., T, length(blank.) > 0)
  vnm. <- f0(!vnn., T, !any(names. == "") & isEQ(names., unique(names.)))
  vlg. <- f0(length(names.) == 0, T, all(sapply(named., cmp_lgl_scl)))
  ver. <- any(grepl("{@}", error., fixed = TRUE))
  err. <- NULL
  if (!v0.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vnn.) {err. <- c(err., "\n • At least one argument in [...] must be named.")}
  if (!vnb.) {err. <- c(err., "\n • At least one argument in [...] must be unnamed.")}
  if (!vnm.) {err. <- c(err., "\n • Named arguments in [...] must be uniquely named (without using a blank string as a name).")}
  if (!vlg.) {err. <- c(err., "\n • Named arguments in [...] must be non-NA logical scalars.")}
  if (!ver.) {err. <- c(err., "\n • An unnamed argument in [...] must contain the escape sequence '{@}' for inserting the names of named arguments.")}
  if (idef(err.)) {stop(err.)}
  for (i. in 1:length(named.)) {
    if (!named.[[1]]) {
      err. <- gsub("{@}", paste0("[", names.[i.], "]"), error.)
      bank_err(err., gens. = 1)
  }}
  NULL
}

#' @describeIn errs For each named argument in \code{...} that is either
#'   \code{NULL} or empty (i.e., of length 0), banks an error message.
#' @export
bank_pop <- function(...) {
  nmd. <- named_dots(...)
  und. <- unnamed_dots(...)
  labs. <- names(nmd.)
  nd. <- ...length()
  nl. <- length(labs.)
  nu. <- length(und.)
  vd. <- nd. > 0
  vu. <- f0(!vd., T, nu. == 0)
  vl. <- f0(nd. != nl., F, !any(labs. == "") & isEQ(labs., unique(labs.)))
  err. <- NULL
  if (!vd.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vu.) {err. <- c(err., "\n • All arguments in [...] must be named.")}
  if (!vl.) {err. <- c(err., "\n • Names of arguments in [...] must be unique without using blank strings.")}
  if (idef(err.)) {stop(err.)}
  for (i. in 1:length(nmd.)) {if (xnil(nmd.[[1]])) {bank_err("[", labs.[i.], "] is NULL or empty.", gens. = 1)}}
  NULL
}

#' @describeIn errs \emph{Bank error messages if arguments satisfy a property
#'   function.} For each named argument in \code{...}, checks whether it
#'   satisfies a property function in \code{funs.}, and if not, banks an error
#'   message. \code{funs.} must be a character scalar containing the names of
#'   one or more \link[=xxx_funs]{property functions}, separated by pipes. An
#'   argument passes the test if calling any one of the functions results in a
#'   value of \code{TRUE}.
#' @export
bank_funs <- function(funs., ...) {
  if (is.character(funs.)) {FUNS. <- av(strsplit(funs., "|", TRUE))}
  labs. <- ...names()
  nd. <- ...length()
  nl. <- length(labs.)
  vd. <- nd. > 0
  vl. <- f0(nd. != nl., F, !any(labs. == "") & isEQ(labs., unique(labs.)))
  vf. <- all(sapply(FUNS., is_xxx_fun))
  err.  <- NULL
  if (!vd.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vl.) {err. <- c(err., "\n • All arguments in [...] must be uniquely named without using blank strings.")}
  if (!vf.) {err. <- c(err., "\n • After splitting on pipes, [funs] contains a value not found in xxx_funs().")}
  if (!xnll(err.)) {stop(err.)}
  err. <- paste0("[", labs., "] must be ", define_xxx_combos(funs.))
  for (i. in 1:nd.) {
    v. <- FALSE
    for (fun. in FUNS.) {v. <- v. | eval(parse(text = paste0(fun., "(...elt(i.))")))}
    if (!v.) {bank_err(err.[i.], gens. = 1)}
  }
  NULL
}

#' @describeIn errs More flexible but less efficient than \code{bank_funs}. For
#'   each named argument in \code{...}, banks an error message if it does
#'   satisfy the \link[=is_xxx]{property specification} in \code{xxx.}. If
#'   \code{na. = TRUE}, arguments may also be atomic scalar \code{NA} values.
#'   \code{xxx.} must be a character scalar containing one or more property
#'   combos. Property combos are created by separating multiple properties from
#'   \code{\link{xxx_all()}}. A property combo may be a single property.
#'   Multiple property combos are separated from each other using pipes. An
#'   argument satisfies the property specification if it satisfies any of the
#'   property combos.
#' @export
bank_xxx <- function(xxx., ..., nas. = F) {
  labs. <- ...names()
  nd. <- ...length()
  nl. <- length(labs.)
  vp. <- f0(!cmp_chr_scl(xxx.), F, is_valid_xxx(xxx.))
  vd. <- nd. > 0
  vl. <- f0(nd. != nl., F, !any(labs. == "") & isEQ(labs., unique(labs.)))
  vn. <- isTF(nas.)
  err. <- NULL
  if (!vp.) {err. <- c(err., "\n • [xxx.] is not a valid property specification.")}
  if (!vd.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vl.) {err. <- c(err., "\n • All arguments in [...] must be uniquely named without using blank strings.")}
  if (!vn.) {err. <- c(err., "\n • [nas.] must be TRUE or FALSE.")}
  if (!xnll(err.)) {stop(err.)}
  err. <- paste0("[", labs., "] must be ", define_xxx_combos(xxx.), ".")
  for (i. in 1:nd.) {
    val. <- F
    if (nas.) {val. <- isNa(...elt(i.))}
    if (!val.) {val. <- is_xxx(...elt(i.), xxx.)}
    if (!val.) {bank_err(err.[i.], gens. = 1)}
  }
  NULL
}

#' @describeIn errs Bank errors if atomic scalar arguments do not have one of a
#'   list of values. For each named atomic scalar argument in \code{...}, checks
#'   it against the remaining (unnamed) atomic arguments in \code{...} (which do
#'   not have to be of the same mode), and if the named argument's value does
#'   not match a value from any unnamed argument in \code{...}, banks an error
#'   message.
#' @export
bank_vals <- function(...) {
  args.  <- named_dots(...)
  vals.  <- unnamed_dots(...)
  labs. <- names(args.)
  v0. <- ...length() > 0
  va. <- f0(!v0., T, length(args.) > 0)
  vv. <- f0(!v0., T, length(vals.) > 0)
  vl. <- f0(!v0. | !va., T, !any(labs. == "") & isEQ(labs., unique(labs.)))
  vat. <- f0(!v0., T, all(sapply(args., pop_atm)))
  vpop. <- f0(!vv., T, all(sapply(vals., pop_atm)))
  err. <- NULL
  if (v0.) {err. <- c(err., "\n • [...] is empty.")}
  if (!va.) {err. <- c(err., "\n • At least one argument in [...] must be named.")}
  if (!vv.) {err. <- c(err., "\n • At least one argument in [...] must be unnamed.")}
  if (!vl.) {err. <- c(err., "\n • Named arguments in [...] must be uniquely named without using blank strings.")}
  if (!vat.) {err. <- c(err., "\n • Named arguments in [...] must be non-empty and atomic.")}
  if (!vpop.) {err. <- c(err., "\n • Unnamed arguments in [...] must be non-empty and atomic.")}
  if (idef(err.)) {stop(err.)}
  values. <- NULL
  for (val. in vals.) {
    if (any(is.na(val.))) {values. <- c(values., 'NA')}
    val. <- val.[!is.na(val.)]
    if (length(val.) > 0) {
      if (is.character(val.)) {values. <- c(values., paste0("\"", val., "\""))}
      else {values. <- c(values., as.character(val.))}
    }
  }
  values. <- oxford_vals.(unique(values.))
  for (i in 1:length(args.)) {
    arg. <- args.[[i]]
    lab. <- labs.[i]
    valid. <- F
    for (j in 1:length(vals.)) {if (!valid.) {valid. <- isIN(arg., vals.[[j]])}}
    if (!valid.) {bank_err("[", lab., "] must be ", values., ".", gens. = 1)}
  }
  NULL
}

#' @describeIn errs Bank errors if dot arguments do not satisfy a property
#'   specification, and optionally, if they are not named. Checks if each
#'   argument in \code{...} satisfies the property specification in \code{prop.}
#'   and if not, banks an error message. If \code{named. = TRUE}, checks whether
#'   all arguments in \code{...} are named, and if not banks an error message.
#' @export
bank_dots <- function(xxx., ..., named. = F) {
  dots.  <- list(...)
  named. <- named_dots(...)
  blank. <- unnamed_dots(...)
  labs.  <- names(named.)
  vp. <- f0(!cmp_chr_scl(xxx.), F, is_valid_xxx(xxx.))
  v0. <- ...length() > 0
  vn. <- isTF(named.)
  vnms. <- f0(!v0. | !vn., T, f0(!named., T, f0(length(blank.) > 0, F,
           f0(any(names(labs.) == ""), F, isEQ(labs., unique(labs.))))))
  err. <- NULL
  if (!vp.) {err. <- c(err., "\n • [xxx.] must be a character scalar containing a valid property specification (see is_valid_xxx).")}
  if (!v0.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vn.) {err. <- c(err., "\n • [named.] must be TRUE or FALSE.")}
  if (!vnms.) {err. <- c(err., "\n • When [named. = TRUE], all arguments in [...] must be uniquely named without using blank strings.")}
  if (idef(err.)) {stop(err.)}
  vd. <- sapply(dots., is_xxx, xxx = xxx.)
  if (!vd.) {bank_err("All arguments in [...] must be ", define_xxx_combos(xxx.), ".", gens. = 1)}
  NULL
}

#' @describeIn errs Bank errors conditionally on the value of two arguments.
#'   Banks an error if the first named atomic scalar argument in \code{...} has
#'   a value contained in \code{when.} and the second named atomic scalar
#'   argument in \code{...} does not contain a value from \code{value}.
#' @export
bank_when <- function(whens., values., ...) {
  labs. <- ...names()
  vw. <- ipop(whens.) & iatm(whens.)
  vv. <- ipop(values.) & iatm(values.)
  vd. <- ...length() == 2
  vl. <- f0(!vd., T, f0(length(labs.) == 2, F, f0(any(labs. == ""), F, labs.[1] != labs.[2])))
  vs. <- iscl(..1) & iscl(..2)
  v1. <- f0(vw. & vs., compatible(whens. , ..1), T)
  v2. <- f0(vw. & vs., compatible(values., ..2), T)
  err. <- NULL
  if (!vw.) {err. <- c(err., "\n • [whens.] must be non-empty and atomic.")}
  if (!vv.) {err. <- c(err., "\n • [values.] must be non-empty and atomic.")}
  if (!vd.) {err. <- c(err., "\n • There must be two arguments in [...]")}
  if (!vl.) {err. <- c(err., "\n • Arguments in [...] must be uniquely named without using blank strings.")}
  if (!v1.) {err. <- c(err., "\n • [whens.] and [..1] are not of compatible modes.")}
  if (!v2.) {err. <- c(err., "\n • [values.] and [..2] are not of compatible modes.")}
  if (!vs.) {err. <- c(err., "\n • Both arguments in [...] must be atomic and scalar.")}
  if (idef(err.)) {stop(err.)}
  labs1. <- paste0("[", labs.[1], "]")
  labs2. <- paste0("[", labs.[2], "]")
  w. <- ..1
  v. <- ..2
  if (is.character(w.)) {w. <- paste0("\"", w., "\"")}
  if (is.character(v.)) {v. <- paste0("\"", v., "\"")}
  if (w. %in% whens.) {if (!(v. %in% values.)) {
    w. <- oxford_vals.(w.)
    v. <- oxford_vals.(v.)
    bank_err("When ", labs1., " is ", w., ", ", labs2., " must be ", v., ".", gens. = 1)
  }}
  NULL
}

#' @describeIn errs Bank errors conditionally on the pattern of properties of
#'   arguments in \code{...} matching none of the specified patterns of
#'   associated properties in \code{pats.}.
#' @export
bank_pats <- function(pats., ...) {
  dots.  <- named_dots(...)
  blank. <- unnamed_dots(...)
  labs. <- names(dots.)
  ndt. <- length(dots.)
  nbl. <- length(blank.)
  npt. <- length(pats.)
  avl. <- cmp_chr_avl(pats.)
  npp. <- f0(avl., lengths(pats.), 0)
  nul. <- length(unique(labs.))
  vpt. <- f0(avl., all(sapply(av(pats.), is_valid_xxx)), F)
  veq. <- all(npp. == ndt.)
  vun. <- ndt. == nul.
  err.  <- NULL
  if (!avl.   ) {err. <- c(err., "\n • [pats] must be a complate atomic vlist (see is_cmp_vlist).")}
  if (npt. < 2) {err. <- c(err., "\n • The number of patterns in [pats] is less than 2.")}
  if (!vpt.   ) {err. <- c(err., "\n • All values in [pats] must be valid property specification (see is_valid_xxx)")}
  if (ndt. < 2) {err. <- c(err., "\n • The number of arguments in [...] is less than 2.")}
  if (nbl. > 0) {err. <- c(err., "\n • All arguments in [...] must be named.")}
  if (!vun.   ) {err. <- c(err., "\n • Arguments in [...] must be uniquely named.")}
  if (!veq.   ) {err. <- c(err., "\n • Elements of [pats] must have length equal to the number of arguments in [...].")}
  if (idef(err.)) {stop(err.)}
  nc. <- rep.int(0, ndt.)
  for (i. in 1:npt.) {
    pat. <- pats.[[i.]]
    valid. <- T
    for (j. in 1:ndt.) {
      var. <- labs.[j.]
      xxx. <- pat.[j.]
      nc.[j.] <- max(nc.[j.], nchar(xxx.))
      if (valid.) {valid. <- valid. & is_xxx(var., xxx.)}
    }
    if (valid.) {return(NULL)}
  }
  nc. <- nc. + 2
  wd. <- nchar(paste0("PATTERN ", npt.))
  for (i. in 1:ndt.) {
    n. <- nc.[i.]
    labs.[i.] <- pad(labs.[i.], n. = n.)
    ipats. <- pats.[[i.]]
    for (j. in 1:npt.) {
      jpat. <- ipats.[j.]
      jpat. <- pad(paste0("\"", jpat., "\""), n. = n.)
      ipats.[j.] <- jpat.
    }
    lab.  <- pad(paste0("PATTERN ", j.), n. = wd.)
    ipats <- paste0(c(lab., ipats.), collapse = " | ")
    pats.[[i.]] <- ipats.
  }
  lab. <- pad("ARGUMENT", n. = wd.)
  vars. <- paste0(c(lab., labs.), collapse = " | ")
  err. <- c("The pattern of values of ", ox_and(paste0("[", labs., "] ")) ,
           "does not match any of the valid patterns in [pats.], given as",
           "\n     ", paste0(c(vars., av(pats.)), collapse = "\n     ")    )
  bank_err(err., gens. = 1)
}

#' @describeIn errs If calling \code{identity(x)} generates an error, bank an
#'   error message.
#' @export
bank_fail <- function(x.) {
  x. <- failsafe(x.)
  if (isERR(x.)) {bank_err(paste0(av(x.), collapse = ""), gens. = 1)}
  NULL
}
