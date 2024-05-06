# rest of package ####

#' @encoding UTF-8
#' @family utils
#' @title Flatten `...` Into an Atomic Vector of the Consituent Atomic Elements of `...`
#' @param ... Objects to be flattened to an attomic vector.
#' @return An atomic vector (or `NULL`)
#' @export
av <- function(...) {
  x <- base::unlist(base::list(...), use.names = F)
  attributes(x) <- NULL
  x
}

#' @encoding UTF-8
#' @family utils
#' @title Split a Property Combos into a Vector of Single Properties
#' @description Splits a single property combo (underscore separated) into its constituent single properties.
#' @param x A character scalar.
#' @return Either `NULL` or a character vector (returns `NULL` when `x` is not of mode `'character'` or is of length zero).
#' @export
combo2props <- function(x) {
  if (base::is.character(x)) {
    if (base::length(uj::spec2combos(x)) == 1) {
      x <- uj::av(base::strsplit(x, "_", fixed = T))
      x <- x[x != ""]
      if (base::length(x) > 0) {
        if (base::any(base::is.na(x))) {NULL} else if (base::any(base::nchar(x) != 3)) {NULL} else {base::unique(base::tolower(x))}
      } else {NULL}
    } else {NULL}
  } else {NULL}
}

#' @encoding UTF-8
#' @family utils
#' @title Convert a Properties Specification Character Scalar or Vector to a Character Vector of Single Properties
#' @description Splits each element of `x` along both `|` and `_` to create a character vector of single constituent
#'   properties, removes any blank values, reduces the result to unique values, converts all remaining values to
#'   lowercase, and sorts the remaining values.
#' @param x A character vector
#' @return Either `NULL` or a non-empty character vector of unique, sorted, lowercase, 3-character values.
#'
#'   Returns `NULL` if any of the following criteria are met:
#'   \itemize{
#'     \item `x` is not of mode `'character'`.
#'     \item Any resulting value is `NA`.
#'     \item Any resulting value is not a 3-character string.
#'     \item `x` resolves to a zero-length character vector.
#'   }
#' @export
spec2props <- function(x) {
  if (base::is.character(x)) {
    x <- uj::av(base::strsplit(x, "|", fixed = T))
    x <- uj::av(base::strsplit(x, "_", fixed = T))
    x <- x[x != ""]
    if (base::length(x) > 0) {
      if (base::any(base::is.na(x))) {NULL} else if (base::any(base::nchar(x) != 3)) {NULL} else {base::unique(base::tolower(x))}
    } else {NULL}
  } else {NULL}
}

#' @encoding UTF-8
#' @family utils
#' @title Convert a Property Spec to a Vector of Property Combos
#' @description Splits a property spec along pipes to place each single property or property combo in a separate element (property combos are multiple properties separated by underscores).
#' @param x A character vector or scalar.
#' @return A character vector or `NULL` (returns `NULL` when `x` is not of mode `'character'` or is of length zero).
#' @export
spec2_combos <- function(x) {
  if (base::is.character(x)) {
    x <- uj::av(base::strsplit(x, "|", fixed = T))
    x <- x[x != ""]
    if (base::length(x) > 0) {base::unique(base::tolower(x))} else {NULL}
  } else {NULL}
}

#' @encoding UTF-8
#' @family utils
#' @title Does Evaluating `x` Create an Error?
#' @param x Object to be evaluated.
#' @return A logical scalar.
#' @export
is_err <- function(x) {rlang::is_error(tryCatch(base::identity(x), error = function(e) e, finally = NULL))}

#' @encoding UTF-8
#' @family utils
#' @title Does Evaluating `x` NOT Create an Error?
#' @param x Object to be evaluated.
#' @return A logical scalar.
#' @export
not_err <- function(x) {!uj::is_err(x)}

#' @encoding UTF-8
#' @family utils
#' @title What Is the Immediate Calling Function
#' @return A character scalar.
#' @export
caller <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  uj::stack2funs(stack[3])
}

#' @encoding UTF-8
#' @family utils
#' @title What Is the List of Calling Functions?
#' @return A character vector.
#' @export
callers <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  uj::stack2funs(stack[3:base::length(stack)])
}


#' @encoding UTF-8
#' @family utils
#' @title Reduce the Call Stack to Just a Vector of Function Names
#' @param stack A character vector.
#' @return A character vector.
#' @export
stack2funs <- function(stack) {
  fun <- function(x) {
    x <- uj::av(base::strsplit(x, "(", fixed = T))[1]
    x <- uj::av(base::strsplit(x, ":::", fixed = T))
    x <- uj::av(base::strsplit(x, "::" , fixed = T))
    x <- uj::av(base::strsplit(x, " ", fixed = T))
    x <- x[[base::length(x)]]
    if (x == "") {"..??.."} else {x}
  }
  base::sapply(stack, fun)
}

#' @encoding UTF-8
#' @family utils
#' @title Reduce the Call Stack to Just a Vector of Package Names
#' @param stack A character vector.
#' @return A character vector.
#' @export
stack2pkgs <- function(stack) {
  pkg <- function(x) {
    y <- uj::av(base::strsplit(x, ":::", fixed = T))[1]
    y <- uj::av(base::strsplit(y, "::" , fixed = T))[1]
    y <- uj::av(base::strsplit(y, " ", fixed = T))[1]
    if (x == uj::stack2funs(x)) {y <- "..??.."}
    y
  }
  base::sapply(stack, pkg)
}

#' @encoding UTF-8
#' @family utils
#' @title List the Calling Function, the Calling Package, and the Call Stack as a Vector of Function Names
#' @param fun,pkg A character scalar.
#' @param stack A character vector.
#' @param .fun An alternate function specification.
#' @param .stack An alternate call stack specification.
#' @return A character vector.
#' @export
fun_pkg_stack <- function(fun, pkg, stack, .fun, .stack) {
  if (fun   == "") {fun   <- .fun     }
  if (pkg   == "") {pkg   <- "..??.." }
  if (stack == "") {stack <- .stack   }
  base::list(fun = uj::stack2funs(fun), pkg = uj::stack2pkgs(pkg), stack = uj::stack2funs(stack))
}

#' @encoding UTF-8
#' @family utils
#' @title Evaluate Any Object, Returning a Valid Result, Always
#' @param x An object to evaluate, even if doing so would result in an error,.
#' @param def The value to return if evaluating `x` results in an error.
#' @return Either `x` or `def`.
#' @export

#' @encoding UTF-8
#' @family utils
#' @title Evaluate Whether `x` and `y` Are of Compatible Modes
#' @param x,y Objects to evaluate.
#' @return A logical scalar.
#' @export
compat <- function(x, y) {
  if (base::is.character(x) & base::is.character(y)) {TRUE}
  else if (base::is.logical(x) & base::is.logical(y)) {TRUE}
  else if (base::is.numeric(x) & base::is.numeric(y)) {TRUE}
  else if (base::is.ordered(x) & base::is.ordered(y)) {
    xLevels <- base::levels(x)
    yLevels <- base::levels(y)
    if (base::length(xLevels) != base::length(yLevels)) {FALSE}
    else {base::all(xLevels == yLevels)}
  }
  else if (base::is.factor(x) & !base::is.ordered(y)) {
    xLevels <- base::levels(x)
    yLevels <- base::levels(y)
    if (base::length(xLevels) != base::length(yLevels)) {FALSE}
    else {base::setequal(xLevels, yLevels)}
  } else {FALSE}
}

#' @encoding UTF-8
#' @family utils
#' @title Simple Oxford-Comma Lists
#' @param x A character vector to be formatted as an Oxford-comma list.
#' @param join A character scalar conjunction for between the next to last and last elements of `x`.
#' @return A character scalar.
#' @export
ox_vals <- function(x, join) {
  n <- base::length(x)
  if (n == 1) {x} else if (n == 2) {base::paste0(x[1], " ", join, " ", x[2])} else {base::paste0(base::paste0(x[1:(n - 1)], collapse = ", "), ", ", join, " ", x[n])}
}

# alert utils ####

#' @encoding UTF-8
#' @family alert_utils
#' @title Conver the Call Stack to a Character Vector of Function Names
#' @param stack A character vector call stack
#' @return A character vector
#' @export
fmt_stack <- function(stack) {
  fun <- function(x) {
    def <- "..unknown.."
    x <- uj::ssplit(x, d = "(")[1]
    x <- uj::ssplit(x, d = ":::")
    x <- uj::ssplit(x, d = "::")
    x <- base::trimws(x, which = "both")
    x <- x[x != ""]
    uj::f0(uj::N(x) == 0, def, x[1])
  }
  default <- "..??.."
  stack <- base::sapply(stack, fun)
  valid <- stack != default
  stack <- stack[valid]
  stack[stack == "..command.line.."] <- "[command.line]"
  stack
}

#' @encoding UTF-8
#' @family alert_utils
#' @title Collapse and Glue `...` Arguments with Space as Default Delimiter
#' @inheritParams g
#' @inheritDotParams g
#' @return A character scalar.
#' @examples
#' glu(letters)
#' glu(letters, 0:9, d = "")
#' @export
glu <- function(..., d = " ") {uj::g(d, ...)}

#' @encoding UTF-8
#' @family alert_utils
#' @title Split Strings along Pipes and Trim Whitespace from Both Sides of All Resulting Elements
#' @param x An object of mode character.
#' @param d Character scalar text delimiter.
#' @return A character vector
#' @examples
#' ssplit(" Just | a | little | thing ")
#' ssplit("  Just  |  a  |  little  |  thing  ")
#' @export
ssplit <- function(x, d = "|") {base::trimws(uj::av(base::strsplit(x, d, fixed = T)), which = "both", whitespace = "[ ]")}

#' @encoding UTF-8
#' @family alert_utils
#' @title Validate Formatting Argument Values
#' @param st Optional style spec.
#' @param bg Optional background color spec.
#' @param fg Optional foreground color spec.
#' @param d Character scalar text delimiter.
#' @param nullst Logical scalar indicating whether a `NULL` style spec is valid.
#' @param nullbg Logical scalar indicating whether a `NULL` background color spec is valid.
#' @param nullfg Logical scalar indicating whether a `NULL` foreground color spec is valid.
#' @examples
#' fmt_errs()
#' fmt_errs(d = 100)
#' fmt_errs(st = "", bg = "", fg = "")
#' fmt_errs(st = "q", bg = "q", fg = "q")
#' fmt_errs(nullst = F, nullbg = F, nullfg = F)
#' @export
fmt_errs <- function(st = NULL, bg = NULL, fg = NULL, d = " ", nullst = TRUE, nullbg = TRUE, nullfg = TRUE) {
  fun  <- uj::caller()
  st   <- uj::failsafe(st)
  bg   <- uj::failsafe(bg)
  fg   <- uj::failsafe(fg)
  d    <- uj::failsafe(d)
  okST <- uj::f0(base::is.null(st), nullst, uj::.unq_chr_vec(st, valid = base::c(uj::.sts(), base::toupper(uj::.sts()))))
  okBG <- uj::f0(base::is.null(bg), nullbg, uj::.cmp_chr_scl(bg, valid = base::c(uj::.bgs(), base::toupper(uj::.bgs()))))
  okFG <- uj::f0(base::is.null(fg), nullfg, uj::.cmp_chr_scl(fg, valid = base::c(uj::.fgs(), base::toupper(uj::.fgs()))))
  okD  <- uj::.cmp_chr_scl(d)
  errs <- NULL
  if (!okBG) {errs <- base::c(errs, "[bg] must be a character scalar from bg_vals().")}
  if (!okFG) {errs <- base::c(errs, "[fg] must be a character scalar from fg_vals().")}
  if (!okST) {errs <- base::c(errs, "[st] must be a unique character vec from st_vals().")}
  if (!okD ) {errs <- base::c(errs, "[d] must be a non-NA character scalar.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, fun = fun, pkg = "dlg")}
}

#' @describeIn match_alias Matches any valid alias for background color.
#' @export
match_bg <- function(x) {
  if      (x %in% uj::.blu()) {"blu"}
  else if (x %in% uj::.cyn()) {"cyn"}
  else if (x %in% uj::.grn()) {"grn"}
  else if (x %in% uj::.blk()) {"blk"}
  else if (x %in% uj::.mag()) {"mag"}
  else if (x %in% uj::.red()) {"red"}
  else if (x %in% uj::.wht()) {"wht"}
  else if (x %in% uj::.yel()) {"yel"}
  else                         {"def"}
}

#' @describeIn match_alias Matches any valid alias for foreground color.
#' @export
match_fg <- function(x) {
  if      (x %in% uj::.blu()) {"blu"}
  else if (x %in% uj::.cyn()) {"cyn"}
  else if (x %in% uj::.grn()) {"grn"}
  else if (x %in% uj::.blk()) {"blk"}
  else if (x %in% uj::.mag()) {"mag"}
  else if (x %in% uj::.red()) {"red"}
  else if (x %in% uj::.sil()) {"sil"}
  else if (x %in% uj::.wht()) {"wht"}
  else if (x %in% uj::.yel()) {"yel"}
  else                         {"def"}
}

#' @describeIn match_alias Matches any valid alias for style.
#' @export
match_st <- function(x) {
  if      (x %in% uj::.bld()) {"bld"}
  else if (x %in% uj::.itl()) {"itl"}
  else if (x %in% uj::.pln()) {"pln"}
  else if (x %in% uj::.und()) {"und"}
  else                         {"def"}
}

#' @encoding UTF-8
#' @family alert_utils
#' @title Validate a Formatting Spec (of form "[bg.color]|[fg.color]|[style]")
#' @param x A character scalar formatting spec.
#' @return A logical scalar
#' @examples
#' .ok_fmt("")
#' .ok_fmt(NULL)
#' .ok_fmt("q|r|b")
#' .ok_fmt("y|r|b")
#' .ok_fmt("  yellow | red | bold  ")
#' @export
ok_fmt <- function(x) {
  if (base::is.null(x)) {return(F)}
  else if (base::is.character(x) & base::length(x) == 1) {
    if (!base::is.na(x)) {if (x == "") {return(T)}}
  }
  x <- uj::ssplit(x)
  if (base::length(x) != 3 | !base::is.character(x)) {F}
  else {x[1] %in% uj::.bgs() & x[2] %in% uj::.fgs() & base::all(x[3] %in% uj::.sts())}
}

#' @encoding UTF-8
#' @family alert_utils
#' @title Choose from a List of Options with a Given Message and a Variety of Options
#' @param opts An atomic vector of options to choose from.
#' @param msg   character scalar message to prompt the selection.
#' @param all  Logical scalar indicating whether it is valid to select all options.
#' @param none Logical scalar indicating whether it is valid to select none of the options.
#' @param min  Non-negative integer scalar minimum number of options to select.
#' @param max  Positive integer scalar maximum numver of options to select.
#' @param ft   Character scalar title formatting spec.
#' @param fs   Character scalar subtitle formatting spec.
#' @param fun  Character scalar name of calling function.
#' @param stk  Character vector call stack.
#' @param clr  Logical scalar indicating whether to clear the console before alerting the user to the selection.
#' @param can  Logical scalar indicating whether cancellation of selection is valid.
#' @return An atomic vector.
#' @examples
#' \dontrun{
#'   choose_from(letters, "What letter?", T, N, 0, 26, "b|s|u", "w|b|p", "console", "console", F)
#'   choose_from(0:9    , "What number?", F, F, 1, 1 , "r|y|b", "y|r|i", "console", "console", T, .cancel = F)
#' }
#'
#' @export
choose_from <- function(opts, msg, all, none, min, max, ft, fs, fun, stk, clr, can = T) {
  if (clr) {uj::xcon()}
  if (ft != "") {
    tbg <- base::strsplit(ft, "|", fixed = T)[[1]][1]
    tfg <- base::strsplit(ft, "|", fixed = T)[[1]][2]
    tst <- base::strsplit(ft, "|", fixed = T)[[1]][3]
  } else {tbg <- tfg <- tst <- NULL}
  if (fs != "") {
    sbg <- base::strsplit(fs, "|", fixed = T)[[1]][1]
    sfg <- base::strsplit(fs, "|", fixed = T)[[1]][2]
    sst <- base::strsplit(fs, "|", fixed = T)[[1]][3]
  } else {sbg <- sfg <- NULL}
  uj::alert(msg, title = "response required", ft = ft, fs = fs, clear = clr)
  if (can) {
    if (TRUE) {base::cat(uj::txt("\nCODE   OPTION         ", bg = sbg, fg = sfg, st = sst))}
    if (TRUE) {base::cat("\n   X = { CANCEL }")}
    if (none) {base::cat("\n   N = { NONE }")}
    if (all ) {base::cat("\n   A = { ALL }")}
  } else {
    if (TRUE) {base::cat(uj::txt("\nCODE   OPTION         ", bg = sbg, fg = sfg, st = sst))}
    if (none) {base::cat("\n   N = { NONE }")}
    if (all ) {base::cat("\n   A = { ALL }")}
  }
  for (i in 1:base::length(opts)) {
    code <- base::as.character(i)
    pref <- uj::p0(base::rep.int(" ", 4 - base::nchar(code)), collapse = "")
    inf  <- " = "
    opt  <- base::gsub(" ", " ", opts[i], fixed = T)
    base::cat("\n", pref, code, inf, opt, sep = "")
  }
  base::cat("\n\n")
  if      (min == 1 & max == 1) {base::cat(uj::txt("Enter the code for"                       , min,            "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  else if (min ==     max     ) {base::cat(uj::txt("Enter a comma separated list of codes for", min,            "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  else                          {base::cat(uj::txt("Enter a comma separated list of codes for", min, "to", max, "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  ans <- base::toupper(base::trimws(base::strsplit(base::readline(), ",", fixed = TRUE)[[1]], which = "both"))
  if (base::length(ans) == 1) {
    if (can  & ans == "X") {uj::stopperr("Canceled by user.", fun = fun, pkg = "dlg", stack = stk)}
    if (none & ans == "N") {return(NULL)}
    if (all  & ans == "A") {return(opts)}
    ans <- base::as.numeric(ans)
    if (!uj::cmp_psw_scl(ans)) {uj::stopperr("Invalid selection code"                                         , fun = fun, pkg = "dlg", stack = stk)}
    if (1 < min               ) {uj::stopperr("Too few options selected."                                      , fun = fun, pkg = "dlg", stack = stk)}
    if (ans > uj::N(opts)    ) {uj::stopperr("Selection code is greater than the number of available options.", fun = fun, pkg = "dlg", stack = stk)}
  } else {
    ans <- base::as.numeric(ans)
    if (!uj::cmp_psw_vec(ans)       ) {uj::stopperr("Unrecognized selection code(s)."                                  , fun = fun, pkg = "dlg", stack = stk)}
    if (uj::N(ans) < min            ) {uj::stopperr("Too few options selected."                                        , fun = fun, pkg = "dlg", stack = stk)}
    if (uj::N(ans) > max            ) {uj::stopperr("Too many options selected."                                       , fun = fun, pkg = "dlg", stack = stk)}
    if (base::any(ans > uj::N(opts))) {uj::stopperr("A selection code is greater than the number of available options.", fun = fun, pkg = "dlg", stack = stk)}
  }
  opts[ans]
}

#' @encoding UTF-8
#' @title Color Aliases
#' @description Each color is assigned a set of aliases, all of which will work in this package.
#' @return A character vector.
#' @examples
#' .blk()
#' .blu()
#' .cyn()
#' .grn()
#' .mag()
#' .red()
#' .sil()
#' .wht()
#' .yel()
#' .bgs()
#' .fgs()
#' @export
color_aliases <- function() {utils::help("color_aliases", package = "dlg")}

#' @encoding UTF-8
#' @title Style Aliases
#' @description Each style is assigned a set of aliases, all of which will work in this package.
#' @return A character vector.
#' @examples
#' .bld()
#' .itl()
#' .pln()
#' .def()
#' .und()
#' .res()
#' .sts()
#' @export
style_aliases <- function() {utils::help("style_aliases", package = "dlg")}

#' @encoding UTF-8
#' @family alert_utils
#' @title Match Color or Style Aliases
#' @description Returns the Official Color or Style Designation from Any Alias
#' @param x A character scalar alias.
#' @return A 3-letter character scalar.
#' @examples
#' cat("\n\nBACKGROUND COLORS"); for (b in 1:bgs()) {cat("\n'", match_bg(b), "' << '", b, "'", sep = "")}
#' cat("\n\nFOREGROUND COLORS"); for (f in 1:bgs()) {cat("\n'", match_fg(f), "' << '", f, "'", sep = "")}
#' cat("\n\nTEXT STYLES"      ); for (s in 1:sts()) {cat("\n'", match_st(s), "' << '", s, "'", sep = "")}
#' @export
match_alias <- function() {utils::help("match_alias", package = "dlg")}

#' @describeIn color_aliases Aliases for "black".
#' @export
.blk <- function() {base::c("k", "blk", "black")}

#' @describeIn color_aliases Aliases for "blue".
#' @export
.blu <- function() {base::c("b", "blu", "blue")}

#' @describeIn color_aliases Aliases for "cyan".
#' @export
.cyn <- function() {base::c("c", "cyn", "cyan")}

#' @describeIn color_aliases Aliases for "green".
#' @export
.grn <- function() {base::c("g", "grn", "green")}

#' @describeIn color_aliases Aliases for "magenta".
#' @export
.mag <- function() {base::c("m", "mag", "magenta")}

#' @describeIn color_aliases Aliases for "red".
#' @export
.red <- function() {base::c("r", "red")}

#' @describeIn color_aliases Aliases for "silver".
#' @export
.sil <- function() {base::c("s", "sil", "slv", "silver", "gray", "grey", "gry")}

#' @describeIn color_aliases Aliases for "white".
#' @export
.wht <- function() {base::c("w", "wht", "white")}

#' @describeIn color_aliases Aliases for "yellow".
#' @export
.yel <- function() {base::c("y", "yel", "ylw", "yellow")}

#' @describeIn color_aliases All aliases for background colors.
#' @export
.bgs <- function() {base::sort(base::c(uj::.blk(), uj::.blu(), uj::.cyn(), uj::.grn(), uj::.mag(), uj::.red(), uj::.wht(), uj::.yel(), uj::.def()))}

#' @describeIn color_aliases All aliases for foreground colors.
#' @export
.fgs <- function() {base::sort(base::c(uj::.bgs(), uj::.sil()))}

#' @describeIn style_aliases All aliases for bold style.
#' @export
.bld <- function() {base::c("b", "bo", "bld", "bold", "bolded", "s", "str", "strong", "strengthened")}

#' @describeIn style_aliases All aliases for italic style.
#' @export
.itl <- function() {base::c("i", "it", "itl", "italic", "italics", "italicized", "e", "em", "emph", "emphasis", "emphasized")}

#' @describeIn style_aliases All aliases for plain style
#' @export
.pln <- function() {base::c("p", "pl", "pln", "plain")}

#' @describeIn style_aliases All aliases for default style
#' @export
.def <- function() {base::c("d", "df", "def", "default")}

#' @describeIn style_aliases All aliases for underline style.
#' @export
.und <- function() {base::c("u", "un", "und", "underline", "underlined")}

#' @describeIn style_aliases All aliases for resetting styles and colors back to default values.
#' @export
.res <- function() {base::c("r", "res", "reset")}

#' @describeIn style_aliases All aliases for styles.
#' @export
.sts <- function() {base::sort(base::c(uj::.bld(), uj::.itl(), uj::.pln(), uj::.def(), uj::.und(), uj::.res()))}

