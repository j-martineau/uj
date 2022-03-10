#' @title Error Management
#' @name errs
#' @description Bank error messages in the immediate environment of a function
#'   to allow for exhaustive error checking before throwing an exception.
#'   Results in a combined, multiple-error message to be reported at the
#'   completion of all error checks.
#' @details \strong{\code{err_check(gens.)}}
#'   \cr Checks for banked error messages in the environment of the function
#'   \code{gens.} generations back in the call stack, and if there are any,
#'   processes them, stopping execution. If there are none, takes no action.
#'   Should be called at the end of error checking.
#'   \cr\cr
#'   \strong{\code{bank_err(..., gens.)}}
#'   \cr Banks the error message in \code{...} in the environment of the
#'   function \code{gens.} generations back in the call stack. The error message
#'   is constructed by collapses all arguments in \code{...} into a character
#'   scalar.
#'   \cr\cr
#'   \strong{\code{bank_lgc(..., na., extras.)}}
#'   \cr Checks the named arguments in \code{...} for whether they are atomic
#'   scalar \code{TRUE} or atomic scalar \code{FALSE}. If \code{na. = TRUE},
#'   also allows atomic scalar \code{NA}. If \code{extras.} contains atomic
#'   values (logical or not), also allows those values. For each named argument
#'   that does not meet the requirements, banks an error.
#'   \cr\cr
#'   \strong{\code{bank_not(...)}}
#'   \cr For each named argument in \code{...} that is \code{FALSE}, creates an
#'   error message by collapsing the remaining (unnamed) arguments in \code{...}
#'   into a character scalar, and banks the error message for that named
#'   argument. The location where the name of a \code{FALSE} named argument
#'   should occur in the message is indicated by the escape sequence
#'   \code{'{@@}'}.
#'   \cr\cr
#'   \strong{\code{bank_pop(...)}}
#'   \cr For each named argument in \code{...} that is either \code{NULL} or
#'   empty (i.e., of length 0), banks an error message.
#'   \cr\cr
#'   \strong{\code{bank_funs(funs., ...)}}
#'   \cr\emph{Bank error messages if arguments satisfy a property function.} For
#'   each named argument in \code{...}, checks whether it satisfies a property
#'   function in \code{funs.}, and if not, banks an error message. \code{funs.}
#'   must be a character scalar containing the names of one or more
#'   \link[=prop_funs]{property functions}, separated by pipes. An argument
#'   passes the test if calling any one of the functions results in a value of
#'   \code{TRUE}.
#'   \cr\cr
#'   \strong{\code{bank_prop(prop., ..., nas.)}}
#'   \cr More flexible but less efficient than \code{bank_funs}. For
#'   each named argument in \code{...}, banks an error message if it does
#'   satisfy the \link[=is_prop]{property specification} in \code{prop.}. If
#'   \code{na. = TRUE}, arguments may also be atomic scalar \code{NA} values.
#'   \code{prop.} must be a character scalar containing one or more property
#'   combos. Property combos are created by separating multiple properties from
#'   \code{\link{props_all()}}. A property combo may be a single property.
#'   Multiple property combos are separated from each other using pipes. An
#'   argument satisfies the property specification if it satisfies any of the
#'   property combos.
#'   \cr\cr
#'   \strong{\code{bank_vals(...)}}
#'   \cr Bank errors if atomic scalar arguments do not have one of a list of
#'   values. For each named atomic scalar argument in \code{...}, checks it
#'   against the remaining (unnamed) atomic arguments in \code{...} (which do
#'   not have to be of the same mode), and if the named argument's value does
#'   not match a value from any unnamed argument in \code{...}, banks an error
#'   message.
#'   \cr\cr
#'   \strong{\code{bank_dots(prop., ..., named.)}}
#'   \cr Bank errors if dot arguments do not satisfy a property specification,
#'   and optionally, if they are not named. Checks if each argument in
#'   \code{...} satisfies the property specification in \code{prop.}, and if not
#'   banks an error message. If \code{named. = TRUE}, checks whether all
#'   arguments in \code{...} are named, and if not banks an error message.
#'   \cr\cr
#'   \strong{\code{bank_when(when., value., ...)}}
#'   \cr Bank errors conditionally on the value of two arguments. Banks an error
#'   if the first named atomic scalar argument in \code{...} has a value
#'   contained in \code{when.} and the second named atomic scalar argument in
#'   \code{...} does not contain a value from \code{value}.
#'   \cr\cr
#'   \strong{\code{bank_pats(pats., ...)}}
#'   \cr Bank errors conditionally on the pattern of properties of arguments in
#'   \code{...} matching none of the specified patterns of associated properties
#'   in \code{pats.}.
#'   \cr\cr
#'   \strong{\code{bank_fail(x)}}
#'   \cr If calling \code{identity(x)} generates an error, bank the error
#'   message.
#' @param gens. A non-negative whole-number scalar indicating the number of
#'   generations back in the call stack in which to bank and/or check for error
#'   messages.
#' @param ... For \code{bank_err}, an arbitrary number of atomic arguments that
#'   when collapsed into a character scalar form an error message.
#'   \cr\cr
#'   For \code{bank_lgc}, \code{bank_pop}, \code{bank_funs}, \code{bank_prop},
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
#'   \link[=prop_funs]{property function} name or multiple property function
#'   names separated by pipes, which are used to check if named arguments in
#'   code{...} satisfy the property specification in any of the function names.
#' @param prop. A character scalar containing one or more property combos
#'   (combos are created by separating one or more \link[=prop_vals]{property
#'   values} with underscores. Multiple combos are separated by pipes.
#' @param named. \code{TRUE} or \code{FALSE} indicating whether arguments in
#'   \code{...} must be uniquely named without using blank strings.
#' @param when.,value. Non-empty atomic objects.
#' @param pats. An atomic vlist containing multiple elements, each of which must
#'   be a complete character mvect of the same length as the number of arguments
#'   in \code{...}. Each element gives a pattern of valid properties for the
#'   argumentx in \code{...} in the form of one property specification per
#'   argument in \code{...} in the same order of those arguments.
#' @return \code{NULL}. Called for the side effect of banking and/or processing
#'   error messages.
#' @export
errs <- NULL

oxford_vals. <- function(x) {
  N <- length(x)
  if (N == 1) {x}
  else if (N == 2) {paste0(x[1], " or ", x[2])}
  else {paste0(paste0(x[1:(N - 1)], collapse = ", "), ", or ", x[N])}
}

#' @rdname errs
#' @export
err_check <- function(gens. = 0) {
  VG <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  if (!VG) {stop("\n * [gens.] does not point to any function in the call stack.")}
  gens. <- gens. + 1
  Name <- "._ERR_._BANK_."
  Errs <- exists(Name, envir = parent.frame(gens.), inherits = F)
  if (Errs) {
    Func <- callers(gens.)
    Bank <- get(Name, envir = parent.frame(gens.), inherits = F)
    Bank <- paste0(paste0("\n  * ", Bank), collapse = "")
    Bank <- paste0("\nIN [", Func, "]", Bank)
    stop(Bank)
  }
  NULL
}

#' @rdname errs
#' @export
bank_err <- function(..., gens. = 0) {
  VG <- f0(!cmp_nnw_scl(gens.), F, gens. <= ncallers() - 1)
  if (!VG) {stop("\n * [gens.] does not point to any function in the call stack.")}
  gens. <- gens. + 1
  Name  <- "._ERR_._BANK_."
  gens. <- gens. + 1
  Err   <- trimws(paste0(as.character(av(...)), collapse = ""), which = "both")
  VE    <- length(Err) > 0 & notEQ(Err, "")
  if (!VE) {stop("\n  * [...] has no atomic values")}
  Err <- gsub("\"", "'", Err, fixed = TRUE)
  if (!exists(Name, envir = parent.frame(gens.), inherits = F)) {Bank <- NULL}
  else {Bank <- get(Name, envir = parent.frame(gens.))}
  Bank <- unique(c(Bank, Err))
  assign(Name, Bank, envir = parent.frame(gens.))
  NULL
}

#' @rdname errs
#' @export
bank_lgc <- function(..., na. = F, extras. = NULL) {
  Dots  <- named_dots(...)
  Names <- names(Dots)
  ND    <- ...length()
  NN    <- length(Names)
  VDT   <- ...length() > 0
  VNM   <- f0(!VDT            , T,
           f0(ND != NN        , F,
           f0(any(Names == ""), F, isEQ(Names, unique(Names)))))
  VLG   <- f0(NN == 0, T, all(sapply(Dots, cmp_lgc_scl)))
  VNA   <- isTF(na.)
  VEX   <- f0(xnll(extras.), T, cmp_atm(extras.))
  E     <- NULL
  if (!VDT) {E <- c(E, "\n  * [...] is empty.")}
  if (!VNM) {E <- c(E, "\n  * All arguments in [...] must be named uniquely without using blank strings.")}
  if (!VLG) {E <- c(E, "\n  * All arguments in [...] must be non-NA logical scalars.")}
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VEX) {E <- c(E, "\n  * [extras.] must be NULL or complete and atomic.")}
  if (xdef(E)) {stop(E)}
  ex. <- xdef(extras.)
  if (ex.) {
    EX <- extras. <- av(extras.)
    N  <- length(EX)
    if (is.character(EX)) {EX <- paste0("\"", EX, "\"")}
    if      (N == 1) {EX <- c(", or ", EX)}
    else if (N == 2) {EX <- c(", ", EX[1], ", or ", EX[2])}
    else {EX <- c(", ", paste(EX[1:(N - 1)], collapse = ", "), ", or ", EX[N])}
  }
  else {EX <- ""}
  EF <- f0(na. | ex., ", FALSE", " or FALSE")
  EN <- f0(na., f0(ex., ", NA", ", or NA"), "")
  E <- c("must be TRUE", EF, EN, EX, ".")
  for (i in 1:ND) {
    x  <- ...elt(i)
    VX <- isTF(x)
    if (!VX & na.) {VX <- isNa(x)}
    if (!VX & ex.) {VX <- isIN(x, EX)}
    if (!VX) {bank_err("[", Names[i], "] ", E, gens. = 1)}
  }
  NULL
}

#' @rdname errs
#' @export
bank_not <- function(...) {
  Named <- named_dots(...)
  Blank <- unnamed_dots(...)
  Error <- paste0(as.character(unlist(Blank, T, F)), collapse = "")
  Names <- names(Named)
  VDT  <- ...length() > 0
  VNN  <- f0(!VDT, T, length(Named) > 0)
  VNB  <- f0(!VDT, T, length(Blank) > 0)
  VNM  <- f0(!VNN, T, !any(Names == "") & isEQ(Names, unique(Names)))
  VLG  <- f0(length(Names) == 0, T, all(sapply(Named, cmp_lgc_scl)))
  VER  <- any(grepl("{@}", Error, fixed = TRUE))
  E <- NULL
  if (!VDT) {E <- c(E, "\n  * [...] is empty.")}
  if (!VNN) {E <- c(E, "\n  * At least one argument in [...] must be named.")}
  if (!VNB) {E <- c(E, "\n  * At least one argument in [...] must be unnamed.")}
  if (!VNM) {E <- c(E, "\n  * Named arguments in [...] must be uniquely named (without using a blank string as a name).")}
  if (!VLG) {E <- c(E, "\n  * Named arguments in [...] must be non-NA logical scalars.")}
  if (!VER) {E <- c(E, "\n  * An unnamed argument in [...] must contain the escape sequence '{@}' for inserting the names of named arguments.")}
  if (xdef(E)) {stop(E)}
  for (i in 1:length(Named)) {
    if (!Named[[1]]) {
      E <- gsub("{@}", paste0("[", Names[i], "]"), Error)
      bank_err(E, gens. = 1)
    }
  }
  NULL
}

#' @rdname errs
#' @export
bank_pop <- function(...) {
  Named <- named_dots(...)
  Blank <- unnamed_dots(...)
  Names <- names(Named)
  ND    <- ...length()
  NN    <- length(Names)
  NB    <- length(Blank)
  VD    <- ND > 0
  VB    <- f0(!VD, T, NB == 0)
  VN    <- f0(ND != NN, F, !any(Names == "") & isEQ(Names, unique(Names)))
  E     <- NULL
  if (!VD) {E <- c(E, "\n  * [...] is empty.")}
  if (!VB) {E <- c(E, "\n  * All arguments in [...] must be named.")}
  if (!VN) {E <- c(E, "\n  * Names of arguments in [...] must be unique without using blank strings.")}
  if (xdef(E)) {stop(E)}
  for (i in 1:length(Named)) {
    if (xnil(Named[[1]])) {
      bank_err("[", Names[i], "] is NULL or empty.", gens. = 1)
    }
  }
  NULL
}

#' @rdname errs
#' @export
bank_funs <- function(funs., ...) {
  if (is.character(funs.)) {FUNS <- av(strsplit(funs., "|", TRUE))}
  Names <- ...names()
  ND    <- ...length()
  NN    <- length(Names)
  VD    <- ND > 0
  VN    <- f0(ND != NN, F, !any(Names == "") & isEQ(Names, unique(Names)))
  VF    <- all(sapply(FUNS, is_prop_fun))
  E  <- NULL
  if (!VD) {E <- c(E, "\n  * [...] is empty.")}
  if (!VN) {E <- c(E, "\n  * All arguments in [...] must be uniquely named without using blank strings.")}
  if (!VF) {E <- c(E, "\n  * After splitting on pipes, [funs] contains a value not found in prop_funs().")}
  if (!xnll(E)) {stop(E)}
  E <- paste0("[", Names, "] must be ", define_combos(funs.))
  for (i in 1:ND) {
    V <- FALSE
    for (fun in FUNS) {V <- V | eval(parse(text = paste0(fun, "(...elt(i))")))}
    if (!V) {bank_err(E[i], gens. = 1)}
  }
  NULL
}

#' @rdname errs
#' @export
bank_prop <- function(prop., ..., nas. = F) {
  Names <- ...names()
  ND    <- ...length()
  NN    <- length(Names)
  VPR   <- f0(!cmp_chr_scl(prop.), F, is_valid_props(prop.))
  VDT   <- ND > 0
  VNM   <- f0(ND != NN, F, !any(Names == "") & isEQ(Names, unique(Names)))
  VNA   <- isTF(nas.)
  E     <- NULL
  if (!VPR) {E <- c(E, "\n  * [prop.] is not a valid property specification.")}
  if (!VDT) {E <- c(E, "\n  * [...] is empty.")}
  if (!VNM) {E <- c(E, "\n  * All arguments in [...] must be uniquely named without using blank strings.")}
  if (!VNA) {E <- c(E, "\n  * [nas.] must be TRUE or FALSE.")}
  if (!xnll(E)) {stop(E)}
  E <- paste0("[", Names, "] must be ", define_combos(prop.), ".")
  for (i in 1:ND) {
    V <- F
    if (nas.) {V <- isNa(...elt(i))}
    if (!V ) {V <- is_prop(...elt(i), prop.)}
    if (!V) {bank_err(E[i], gens. = 1)}
  }
  NULL
}

#' @rdname errs
#' @export
bank_vals <- function(...) {
  Args  <- named_dots(...)
  Vals  <- unnamed_dots(...)
  Names <- names(Args)
  VD <- ...length() > 0
  VA <- f0(!VD, T, length(Args) > 0)
  VV <- f0(!VD, T, length(Vals) > 0)
  VN <- f0(!VD | !VA, T, !any(Names == "") & isEQ(Names, unique(Names)))
  VM <- f0(!VD, T, all(sapply(Args, pop_atm)))
  VP <- f0(!VV, T, all(sapply(Vals, pop_atm)))
  E <- NULL
  if (!VD) {E <- c(E, "\n  * [...] is empty.")}
  if (!VA) {E <- c(E, "\n  * At least one argument in [...] must be named.")}
  if (!VV) {E <- c(E, "\n  * At least one argument in [...] must be unnamed.")}
  if (!VN) {E <- c(E, "\n  * Named arguments in [...] must be uniquely named without using blank strings.")}
  if (!VM) {E <- c(E, "\n  * Named arguments in [...] must be non-empty and atomic.")}
  if (!VP) {E <- c(E, "\n  * Unnamed arguments in [...] must be non-empty and atomic.")}
  if (xdef(E)) {stop(E)}
  Values <- NULL
  for (Val in Vals) {
    if (any(is.na(Val))) {Values <- c(Values, 'NA')}
    Val <- Val[!is.na(Val)]
    if (length(Val) > 0) {
      if (is.character(Val)) {Values <- c(Values, paste0("\"", Val, "\""))}
      else {Values <- c(Values, as.character(Val))}
    }
  }
  Values <- oxford_vals.(unique(Values))
  for (i in 1:length(Args)) {
    Arg   <- Args[[i]]
    Name  <- Names[i]
    Valid <- F
    for (j in 1:length(Vals)) {
      if (!Valid) {Valid <- isIN(Arg, Vals[[j]])}
    }
    if (!Valid) {bank_err("[", Name, "] must be ", Values, ".", gens. = 1)}
  }
  NULL
}

#' @rdname errs
#' @export
bank_dots <- function(prop., ..., named. = F) {
  Dots  <- list(...)
  Named <- named_dots(...)
  Blank <- unnamed_dots(...)
  N  <- names(Named)
  VP <- f0(!cmp_chr_scl(prop.), F, is_valid_props(prop.))
  VD <- ...length() > 0
  VN <- isTF(named.)
  VM <- f0(!VD | !VN          , T,
        f0(!named.            , T,
        f0(length(Blank) > 0  , F,
        f0(any(names(N) == ""), F, isEQ(N, unique(N))))))
  E  <- NULL
  if (!VP) {E <- c(E, "\n  * [prop.] must be a character scalar containing a valid property specification (see is_valid_props).")}
  if (!VD) {E <- c(E, "\n  * [...] is empty.")}
  if (!VM) {E <- c(E, "\n  * When [named. = TRUE], all arguments in [...] must be uniquely named without using blank strings.")}
  if (!VN) {E <- c(E, "\n  * [named.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  VD <- sapply(Dots, is_prop, prop = prop.)
  if (!VD) {bank_err("All arguments in [...] must be ", define_combos(prop.), ".", gens. = 1)}
  NULL
}

#' @rdname errs
#' @export
bank_when <- function(whens., values., ...) {
  N <- ...names()
  VW <- pop_atm(whens. )
  VV <- pop_atm(values.)
  VD <- ...length() == 2
  VN <- f0(!VD, T, f0(length(N) == 2, F, f0(any(N == ""), F, N[1] != N[2])))
  VS <- xscl(..1) & xscl(..2)
  V1 <- f0(VW & VS, compatible(whens. , ..1), T)
  V2 <- f0(VW & VS, compatible(values., ..2), T)
  E  <- NULL
  if (!VW) {E <- c(E, "\n  * [whens.] must be non-empty and atomic.")}
  if (!VV) {E <- c(E, "\n  * [values.] must be non-empty and atomic.")}
  if (!VD) {E <- c(E, "\n  * There must be two arguments in [...]")}
  if (!VN) {E <- c(E, "\n  * Arguments in [...] must be uniquely named without using blank strings.")}
  if (!V1) {E <- c(E, "\n  * [whens.] and [..1] are not of compatible modes.")}
  if (!V2) {E <- c(E, "\n  * [values.] and [..2] are not of compatible modes.")}
  if (!VS) {E <- c(E, "\n  * Both arguments in [...] must be atomic and scalar.")}
  if (xdef(E)) {stop(E)}
  N1 <- paste0("[", N[1], "]")
  N2 <- paste0("[", N[2], "]")
  W  <- ..1
  V  <- ..2
  if (is.character(W)) {W <- paste0("\"", W, "\"")}
  if (is.character(V)) {V <- paste0("\"", V, "\"")}
  if (W %in% whens.) {
    if (!(V %in% values.)) {
      W <- oxford_vals.(W)
      V <- oxford_vals.(V)
      bank_err("When ", N1, " is ", W, ", ", N2, " must be ", V, ".", gens. = 1)
    }
  }
  NULL
}

#' @rdname errs
#' @export
bank_pats <- function(pats., ...) {
  Dots  <- named_dots(...)
  Blank <- unnamed_dots(...)
  Names <- names(Dots)
  NDT <- length(Dots )
  NBL <- length(Blank)
  NPT <- length(pats.)
  AVL <- cmp_chr_avl(pats.)
  NPP <- f0(AVL, lengths(pats.), 0)
  NUN <- length(unique(Names))
  VPT <- f0(AVL, all(sapply(av(pats.), is_valid_props)), F)
  VEQ <- all(NPP == NDT)
  VUN <- NDT == NUN
  E   <- NULL
  if (!AVL   ) {E <- c(E, "\n  * [pats] must be a complate atomic vlist (see is_cmp_vlist).")}
  if (NPT < 2) {E <- c(E, "\n  * The number of patterns in [pats] is less than 2.")}
  if (!VPT   ) {E <- c(E, "\n  * All values in [pats] must be valid property specification (see is_valid_props)")}
  if (NDT < 2) {E <- c(E, "\n  * The number of arguments in [...] is less than 2.")}
  if (NBL > 0) {E <- c(E, "\n  * All arguments in [...] must be named.")}
  if (!VUN   ) {E <- c(E, "\n  * Arguments in [...] must be uniquely named.")}
  if (!VEQ   ) {E <- c(E, "\n  * Elements of [pats] must have length equal to the number of arguments in [...].")}
  if (xdef(E)) {stop(E)}
  NC <- rep.int(0, NDT)
  for (i in 1:NPT) {
    Props <- pats.[[i]]
    Valid <- T
    for (j in 1:NDT) {
      Var  <- Names[j]
      Prop <- Props[j]
      NC[j] <- max(NC[j], nchar(Prop))
      if (Valid) {Valid <- Valid & is_prop(Var, Prop)}
    }
    if (Valid) {return(NULL)}
  }
  NC <- NC + 2
  WD <- nchar(paste0("PATTERN ", NPT))
  for (i in 1:NDT) {
    N <- NC[i]
    Names[i] <- pad(Names[i], n. = N)
    Pats <- pats.[[i]]
    for (j in 1:NPT) {
      Pat <- Pats[j]
      Pat <- pad(paste0("\"", Pat, "\""), n. = N)
      Pats[j] <- Pat
    }
    Lab  <- pad(paste0("PATTERN ", j), n. = WD)
    Pats <- paste0(c(Lab, Pats), collapse = " | ")
    pats.[[i]] <- Pats
  }
  Lab  <- pad("ARGUMENT", n. = WD)
  Vars <- paste0(c(Lab, Names), collapse = " | ")
  E <- c("The pattern of values of ", ox_and(paste0("[", Names, "] ")) ,
         "does not match any of the valid patterns in [pats.], given as",
         "\n     ", paste0(c(Vars, av(Pats)), collapse = "\n     ")    )
  bank_err(E, gens. = 1)
}

#' @rdname errs
#' @export
bank_fail <- function(x) {
  x <- failsafe(x)
  if (isERR(x)) {bank_err(paste0(av(x), collapse = ""), gens. = 1)}
  NULL
}
