# failsafe ####

# are x and y of compatible (sortable or unsortable) modes
.compat <- function(x, y) {
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

# are x and y of comparable (sortable) modes
.compar <- function(x, y) {
  if (base::is.character(x) & base::is.character(y)) {TRUE}
  else if (base::is.logical(x) & base::is.logical(y)) {TRUE}
  else if (base::is.numeric(x) & base::is.numeric(y)) {TRUE}
  else if (base::is.ordered(x) & base::is.ordered(y)) {
    xLevels <- base::levels(x)
    yLevels <- base::levels(y)
    if (base::length(xLevels) != base::length(yLevels)) {FALSE}
    else {base::all(xLevels == yLevels)}
  } else {FALSE}
}

# are x and y of recyclable lengths (1, max(c(length(x), length(y))))
.mismatch_n <- function(x, y) {
  nx <- base::length(x)
  ny <- base::length(y)
  nxy <- base::c(nx, ny)
  nValid <- base::c(1, nxy)
  nx == 0 | ny == 0 | !base::all(nxy %in% nValid)
}

# checkerr ####

# check whether Dots are named
.all_named <- function(...) {
	nDots <- base::...length()
	DotNames <- base::...names()
	if (nDots == 0) {F} else if (base::length(DotNames) != nDots) {F} else {!base::any(base::is.na(DotNames))}
}

# simple oxford comma lists
.ox_vals <- function(x, join) {
	N <- base::length(x)
	if (N == 1) {x} else if (N == 2) {base::paste0(x[1], " ", join, " ", x[2])} else {base::paste0(base::paste0(x[1:(N - 1)], collapse = ", "), ", ", join, " ", x[N])}
}

# ppp_specs ####

.spec2combos <- function(x) {
	if (base::is.character(x)) {
		x <- uj::av(base::strsplit(x, "|", fixed = T))
		x <- x[x != ""]
		if (base::length(x) > 0) {base::unique(base::tolower(x))} else {NULL}
	} else {NULL}
}

.spec2props <- function(x) {
	if (base::is.character(x)) {
		x <- uj::av(base::strsplit(x, "|", fixed = T))
		x <- uj::av(base::strsplit(x, "_", fixed = T))
		x <- x[x != ""]
		if (base::length(x) > 0) {
			if (base::any(base::is.na(x))) {NULL} else if (base::any(base::nchar(x) != 3)) {NULL} else {base::unique(base::tolower(x))}
		} else {NULL}
	} else {NULL}
}

.combo2props <- function(x) {
	if (base::is.character(x)) {
		if (base::length(uj:::.spec2combos(x)) == 1) {
			x <- uj::av(base::strsplit(x, "_", fixed = T))
			x <- x[x != ""]
			if (base::length(x) > 0) {
				if (base::any(base::is.na(x))) {NULL} else if (base::any(base::nchar(x) != 3)) {NULL} else {base::unique(base::tolower(x))}
			} else {NULL}
		} else {NULL}
	} else {NULL}
}

# ppp_fast ####

.check_VALID <- function(x, test, .VALID) {

  if (test) {
    x <- uj::av(x)
    .VALID <- uj::av(.VALID)
    if (base::length(.VALID) == 0) {T} else {uj::all_in(x, .VALID)}
  } else {F}
}

# n ####

.n_errs <- function(.N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .NA = FALSE, .A = FALSE) {
	Errors <- NULL
	if (!base::is.null(.N) & (!uj:::.cmp_nnw_vec(.N))) {Errors <- base::c(Errors, "[.N] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec).")}
	if (!base::is.null(.MIN) & (!uj:::.cmp_nnw_scl(.MIN))) {Errors <- base::c(Errors, "[.MIN] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
	if (!base::is.null(.MAX) & (!uj:::.cmp_nnw_scl(.MAX))) {Errors <- base::c(Errors, "[.MAX] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
	if (!uj::fs_tf(.EQ)) {Errors <- base::c(Errors, "[.EQ] must be TRUE or FALSE.")}
	if (!uj::fs_tf(.NA)) {Errors <- base::c(Errors, "[.NA] must be TRUE or FALSE.")}
	if (!uj::fs_tf(.A)) {Errors <- base::c(Errors, "[.A] must be TRUE or FALSE.")}
	Errors
}

# delim ####

.delim_errs <- function(args, .A) {
	N <- uj::N(args$Dots)
	Ns <- uj::ns(args$Dots)
	Two <- "D" %in% base::names(args)
	Dots <- N > 0
	Pops <- uj::f0(!Dots, T, base::all(Ns > 0))
	CCSd <- uj:::.cmp_chr_scl(args$d)
	CCSD <- uj::f0(!Two, T, uj:::.cmp_chr_scl(args$D))
	Atms <- uj::f0(!CCSd, T, base::all(base::sapply(args$Dots, uj::atm_vec)))
	Recs <- uj::f0(.A | !Pops, T, uj::recyclable_ns(base::max(Ns) / Ns))
	Errors <- NULL
	if (!Dots) {Errors <- base::c(Errors, "[...] is empty.")}
	if (!Pops) {Errors <- base::c(Errors, "An argument in [...] is of length 0.")}
	if (!CCSd) {Errors <- base::c(Errors, "[d] must be a complete character scalar (?cmp_chr_scl).")}
	if (!CCSD) {Errors <- base::c(Errors, "[D] must be a complete character scalar (?cmp_chr_scl).")}
	if (!Atms) {Errors <- base::c(Errors, "Arguments in [...] must be atomic vecs (?pop_vec).")}
	if (!Recs) {Errors <- base::c(Errors, "Arguments in [...] are not of recyclable lengths (?recyclable).")}
	Errors
}

# color ####

.color_errs <- function(x = "red", y = "red", lighten = 0, darken = 0, r = 0, g = 0, b = 0, a = 0, h = 0, s = 0, v = 0, wx = 1, wy = 1, comp = T, .NA = F) {
	Fun <- uj::caller()
	Errors <- NULL
	if (!uj:::.cmp_clr_vec(x) ) {Errors <- base::c(Errors, "[x] must be contain only valid character color representations.")}
	if (!uj:::.cmp_clr_vec(y) ) {Errors <- base::c(Errors, "[y] must be contain only valid character color representations.")}
	if (!uj:::.cmp_ppn_vec(lighten)) {Errors <- base::c(Errors, "[lighten] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(darken)) {Errors <- base::c(Errors, "[darken] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(b)) {Errors <- base::c(Errors, "[b] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(g)) {Errors <- base::c(Errors, "[g] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(h)) {Errors <- base::c(Errors, "[h] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(r)) {Errors <- base::c(Errors, "[r] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(s)) {Errors <- base::c(Errors, "[s] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(v)) {Errors <- base::c(Errors, "[v] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!(.NA & uj::.NA0(a))) {if (!uj:::.cmp_ppn_vec(a)) {Errors <- base::c(Errors, "[a] must be a complete proportion vec (?cmp_ppn_vec).")}}
	if (!uj:::.cmp_pos_vec(wx)) {Errors <- base::c(Errors, "[wx] must be a complete positive numeric vec (?cmp_pos_vec).")}
	if (!uj:::.cmp_pos_vec(wy)) {Errors <- base::c(Errors, "[wy] must be a complete positive numeric vec (?cmp_pos_vec).")}
	if (!uj:::.cmp_lgl_scl(comp)) {Errors <- base::c(Errors, "[comp] must be TRUE or FALSE.")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, .FUN = Fun, .PKG = "uj")}
}

# crayon ####

.glue_args <- function(.D, ...) {base::paste0(uj::av(...), collapse = .D)}

.crayons_errs <- function(.ST = NULL, .BG = NULL, .FG = NULL, .D = " ", .OK.NULL.ST = TRUE, .OK.NULL.BG = TRUE, .OK.NULL.FG = TRUE) {
	Fun <- uj::caller()
	.ST <- uj::failsafe(.ST)
	.BG <- uj::failsafe(.BG)
	.FG <- uj::failsafe(.FG)
	.D <- uj::failsafe(.D)
	OkST <- uj::f0(base::is.null(.ST), .OK.NULL.ST, uj:::.unq_chr_vec(.ST, .VALID = base::c(uj::v(CrayonStyles), base::toupper(uj::v(CrayonStyles)))))
	OkBg <- uj::f0(base::is.null(.BG), .OK.NULL.BG, uj:::.cmp_chr_scl(.BG, .VALID = base::c(uj::v(CrayonBackgroundColors), base::toupper(uj::v(CrayonBackgroundColors)))))
	OkFg <- uj::f0(base::is.null(.FG), .OK.NULL.FG, uj:::.cmp_chr_scl(.FG, .VALID = base::c(uj::v(CrayonForegroundColors), base::toupper(uj::v(CrayonForegroundColors)))))
	OkD <- uj:::.cmp_chr_scl(.D)
	Errors <- NULL
	if (!OkBg) {Errors <- base::c(Errors, "[.BG] must be a character scalar from bg_vals().")}
	if (!OkFg) {Errors <- base::c(Errors, "[.FG] must be a character scalar from fg_vals().")}
	if (!OkST) {Errors <- base::c(Errors, "[.ST] must be a unique character vec from st_vals().")}
	if (!OkD ) {Errors <- base::c(Errors, "[.D] must be a non-NA character scalar.")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, .FUN = Fun, .PKG = "uj")}
}

.match_bg_val <- function(x) {
  if      (x %in% uj::v(CrayonBlue)   ) {"blu"}
  else if (x %in% uj::v(CrayonCyan)   ) {"cyn"}
  else if (x %in% uj::v(CrayonGreen)  ) {"grn"}
  else if (x %in% uj::v(CrayonBlack)  ) {"blk"}
  else if (x %in% uj::v(CrayonMagenta)) {"mag"}
  else if (x %in% uj::v(CrayonRed)    ) {"red"}
  else if (x %in% uj::v(CrayonWhite)  ) {"wht"}
  else if (x %in% uj::v(CrayonYellow) ) {"ylw"}
  else                                  {"def"}
}

.match_fg_val <- function(x) {
  if      (x %in% uj::v(CrayonBlue)   ) {"blu"}
  else if (x %in% uj::v(CrayonCyan)   ) {"cyn"}
  else if (x %in% uj::v(CrayonGreen)  ) {"grn"}
  else if (x %in% uj::v(CrayonBlack)  ) {"blk"}
  else if (x %in% uj::v(CrayonMagenta)) {"mag"}
  else if (x %in% uj::v(CrayonRed)    ) {"red"}
  else if (x %in% uj::v(CrayonSilver) ) {"sil"}
  else if (x %in% uj::v(CrayonWhite)  ) {"wht"}
  else if (x %in% uj::v(CrayonYellow) ) {"ylw"}
  else                                  {"def"}
}

.match_st_val <- function(x) {
  if      (x %in% uj::v(CrayonBold)     ) {"bld"}
  else if (x %in% uj::v(CrayonItalic)   ) {"itl"}
  else if (x %in% uj::v(CrayonPlain)    ) {"pln"}
  else if (x %in% uj::v(CrayonUnderline)) {"und"}
  else                                    {"def"}
}

# dialog ####

.chr <- function(..., .D = " ") {base::paste0(uj::av(...), collapse = .D)}
.trm <- function(..., .D = " ") {base::trimws(uj:::.chr(..., .D = .D), which = "both")}
.ssplit <- function(x) {base::trimws(uj::av(base::strsplit(x, "|", fixed = T)), which = "both", whitespace = "[ ]")}

.ok_fmt <- function(x) {
	if (base::is.null(x)) {return(F)} else if (base::is.character(x) & base::length(x) == 1) {if (!base::is.na(x)) {if (x == "") {return(TRUE)}}}
	x <- uj:::.ssplit(x)
	uj::f0(base::length(x) != 3, F, uj::f0(!(x[1] %in% uj::v(CrayonBackgroundColors)), F, uj::f0(!(x[2] %in% uj::v(CrayonForegroundColors)), F, base::all(x[3] %in% uj::v(CrayonStyles)))))
}

.cat_choose_list <- function(options, message, .ALL, .NONE, .MIN, .MAX, .FT, .FS, .FUN, .STACK, .CLEAR, .CANCEL = T) {
  if (.CLEAR) {uj::xconsole()}
  if (.FT != "") {
    TitleBG <- base::strsplit(.FT, "|", fixed = T)[[1]][1]
    TitleFG <- base::strsplit(.FT, "|", fixed = T)[[1]][2]
    TitleST <- base::strsplit(.FT, "|", fixed = T)[[1]][3]
  } else {TitleBG <- TitleFG <- TitleST <- NULL}
  if (.FS != "") {
    SubBG <- base::strsplit(.FS, "|", fixed = T)[[1]][1]
    SubFG <- base::strsplit(.FS, "|", fixed = T)[[1]][2]
  } else {SubBG <- SubFG <- NULL}
  uj::alert(message, Title = "response required", .FT = .FT, .FS = .FS, .CLEAR = .CLEAR)
  base::cat("\n")
  if (.CANCEL) {
    if (TRUE ) {base::cat(uj::txt("CODE   OPTION         ", .BG = SubBG, .FG = SubFG, .ST = "bold"))}
    if (TRUE ) {base::cat(      "\n   X   { CANCEL }")}
    if (.NONE) {base::cat(      "\n   N   { NONE }")}
    if (.ALL ) {base::cat(      "\n   A   { ALL }")}
  } else {
    if (TRUE ) {base::cat(uj::txt("CODE   OPTION         ", .BG = SubBG, .FG = SubFG, .ST = "bold"))}
    if (.NONE) {base::cat(      "\n   N   { NONE }")}
    if (.ALL ) {base::cat(      "\n   A   { ALL }")}
  }
  for (i in 1:base::length(options)) {
    Code <- base::as.character(i)
    Prefix <- base::paste0(base::rep.int(" ", 4 - base::nchar(Code)), collapse = "")
    Infix <- "   "
    Option <- base::gsub(" ", " ", options[i], fixed = T)
    base::cat("\n", Prefix, Code, Infix, Option, sep = "")
  }
  cat("\n\n")
  if (.MIN == 1 & .MAX == 1) {base::cat(uj::txt("Enter the code for 1 of the above options:", .BG = TitleBG, .FG = TitleFG, .ST = TitleST))}
  else if (.MIN == .MAX) {base::cat(uj::txt("Enter a comma separated list of codes for", .MIN, "of the above options:", .D = " ", .BG = TitleBG, .FG = TitleFG, .ST = TitleST))}
  else {base::cat(uj::txt("Enter a comma separated list of codes for", .MIN, "to", .MAX, "of the above options:", .D = " ", .BG = TitleBG, .FG = TitleFG, .ST = TitleST))}
  Answer <- base::toupper(base::trimws(base::strsplit(base::readline(), ",", fixed = TRUE)[[1]], which = "both"))
  if (base::length(Answer) == 1) {
    if (.CANCEL & Answer == "X") {uj::stopperr("Canceled by user.", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
    if (.NONE & Answer == "N") {return(NULL)}
    if (.ALL & Answer == "A") {return(options)}
    Answer <- base::as.numeric(Answer)
    if (!uj::cmp_psw_scl(Answer)) {uj::stopperr("Invalid selection code", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
    if (1 < .MIN) {uj::stopperr("Too few options selected.", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
    if (Answer > uj::N(options)) {uj::stopperr("Selection code is greater than the number of available options.", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
  } else {
    Answer <- base::as.numeric(Answer)
    if (!uj::cmp_psw_vec(Answer)) {uj::stopperr("Unrecognized selection code(s).", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
    if (uj::N(Answer) < .MIN) {uj::stopperr("Too few options selected.", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
    if (uj::N(Answer) > .MAX) {uj::stopperr("Too many options selected.", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
    if (base::any(Answer > uj::N(options))) {uj::stopperr("A selection code is greater than the number of available options.", .FUN = .FUN, .PKG = "uj", .STACK = .STACK)}
  }
  options[Answer]
}

# declare ####

.labs_ok <- function(x) {
	Alpha <- base::c(letters, LETTERS)
	Nums <- base::as.character(0:9)
	OK <- base::substr(x, 1, 1) %in% Alpha
	OK[!OK] <- base::substr(x[!OK], 1, 1) == "." & base::substr(x[!OK], 2, 2) %in% Alpha
	if (base::all(OK)) {
	  nChars <- base::nchar(x)
		OK <- OK & base::substr(x, nChars, nChars) %in% base::c(Alpha, Nums)
		uj::f0(!base::all(OK), F, base::all(uj::av(base::strsplit(x, "", fixed = T)) %in% base::c(Alpha, Nums, ".", "_")))
	} else {F}
}

# callers ####

.getfun <- function(x) {
  Default <- "..unknown.."
  x <- uj::av(base::strsplit(x, ":::", fixed = T))
  x <- uj::av(base::strsplit(x, "::", fixed = T))
  N <- base::length(x)
  if (N == 0) {x <- ""} else if (N > 1) {x <- x[2]}
  x <- uj::av(base::strsplit(x, "(", fixed = T))
  x <- uj::av(base::strsplit(x, " ", fixed = T))
  N <- base::length(x)
  if (N == 0) {x <- ""} else if (N > 1) {x <- x[1]}
  if (x == "") {Default} else {x}
}

.getpkg <- function(x) {
  Default <- "..??.."
  x <- uj::av(base::strsplit(x, ":::", fixed = T))
  x <- uj::av(base::strsplit(x, "::", fixed = T))
  N <- base::length(x)
  if (N < 2) {Default} else {x[1]}
}

.getsep <- function(x) {if (base::length(uj::av(base::strsplit(x, ":::", fixed = T))) > 1) {":::"} else {"::"}}

.stack2pkgfuns <- function(stack, maxlen, .VEC = T, .PKG.ONLY = F, .FUN.ONLY = F) {
  Default <- "..??.."
  Funs <- base::sapply(stack, uj:::.getfun)
  Pkgs <- base::sapply(stack, uj:::.getpkg)
  Seps <- base::sapply(stack, uj:::.getsep)
  OK <- Funs != Default
  Funs <- Funs[OK]
  Pkgs <- Pkgs[OK]
  Seps <- Seps[OK]
  Pkgs[Pkgs == "..R.."] <- "[R]"
  Funs[Funs == "..command.line.."] <- "[command.line]"
  Pkgs[Pkgs == Default] <- "[??]"
  Funs[Funs == Default] <- "[??]"
  if (.PKG.ONLY) {Pkgs} else if (.FUN.ONLY) {Funs} else if (.VEC) {base::paste0(Pkgs, Seps, Funs)} else {tibble::tibble(Pkg = Pkgs, Fun = Funs)}
}

.stack2funs <- function(stack, maxlen) {
  Default <- "..??.."
  Funs <- base::sapply(stack, uj:::.getfun)
  OK <- Funs != Default
  Funs <- Funs[OK]
  if (base::length(Funs) == 0) {Funs <- Default}
  Funs[Funs == "..command.line.."] <- "[command.line]"
  Funs[Funs == Default] <- "[??]"
  Funs
}

.stack2pkgs <- function(stack, maxlen) {
  Default <- "..??.."
  Funs <- base::sapply(stack, uj:::.getfun)
  Pkgs <- base::sapply(stack, uj:::.getpkg)
  Ok <- Funs != Default
  Pkgs <- Pkgs[Ok]
  if (base::length(Pkgs) == 0) {Pkgs <- Default}
  Pkgs[Pkgs == "..R.."] <- "[R]"
  Pkgs[Pkgs == Default] <- "[??]"
  Pkgs
}

.fun_pkg_stack <- function(fun, pkg, stack, .FUN, .STACK) {
  if (fun == "") {fun <- .FUN}
  if (pkg == "") {pkg <- "..??.."}
  if (stack == "") {stack <- .STACK}
  fun <- uj:::.stack2funs(fun, 1000)
  pkg <- uj:::.stack2pkgs(pkg, 1000)
  stack <- base::paste0(uj:::.stack2pkgfuns(stack, .MAX.LEN = 35, .VEC = T), collapse = " >> ")
  base::list(fun = fun, pkg = pkg, stack = stack)
}

# meets ####

.meets_errs <- function(x, ...) {
	if (base::...length() == 0) {return(NULL)}
  Dots <- base::list(...)
  Names <- base::names(Dots)
  Valid <- base::c('.N', '.NR', '.NC', '.MIN', '.MINR', '.MINC', '.MAX', '.MAXR', '.MAXC', '.VALS', '.LT', '.LE', '.GE', '.GT')
  Errors <- NULL
	if (base::length(Names) != base::length(Dots)) {Errors <- base::c(Errors, "All [...] arguments must be named.")}
	if (base::length(Names) != base::length(base::unique(Names))) {Errors <- base::c(Errors, "Names of [...] arguments must be unique.")}
	if (!base::all(Names %in% Valid)) {Errors <- base::c(Errors, base::paste0("All names of [...] arguments must be from c('.N', '.NR', '.NC', '.MIN', '.MINR', '.MINC', '.MAX', '.MAXR', '.MAXC', '.VALS', '.LT', '.LE', '.GE', '.GT')."))}
	if (!base::is.null(Errors)) {uj::stopper(Errors, .FUN = "meets", .PKG = "uj")}
  if (".N" %in% Names) {if (!uj:::.cmp_nnw_vec(Dots$.N)) {Errors <- base::c(Errors, "[.N] must be a non-negative whole-number vector (?uj::cmp_nnw_vec) if supplied.")}}
  if (".MIN" %in% Names) {if (!uj:::cmp_nnw_scl(Dots$.MIN)) {Errors <- base::c(Errors, "[.MIN] must be a non-negative whole-number scalar (?uj::cmp_nnw_scl) if supplied.")}}
  if (".MAX" %in% Names) {if (!uj:::cmp_nnw_scl(Dots$.MAX)) {Errors <- base::c(Errors, "[.MAX] must be a non-negative whole-number scalar (?uj::cmp_nnw_scl) if supplied.")}}
  if (".NR" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.NR] is supplied")}
    if (!uj:::.cmp_nnw_vec(Dots$.NR)) {Errors <- base::c(Errors, "[.NR] must be a non-negative whole-number vector (?uj::cmp_nnw_vec) if supplied.")}
  }
  if (".MINR" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MINR] is supplied")}
    if (!uj:::cmp_nnw_scl(Dots$.MINR)) {Errors <- base::c(Errors, "[.MINR] must be a non-negative whole-number scalar (?uj::cmp_nnw_scl) if supplied.")}
  }
  if (".MAXR" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MAXR] is supplied")}
    if (!uj:::cmp_nnw_scl(Dots$.MAXR)) {Errors <- base::c(Errors, "[.MAXR] must be a non-negative whole-number scalar (?uj::cmp_nnw_scl) if supplied.")}
  }
  if (".NC" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.NC] is supplied")}
    if (!uj:::.cmp_nnw_vec(Dots$.NC)) {Errors <- base::c(Errors, "[.NC] must be a non-negative whole-number vector (?uj::cmp_nnw_vec) if supplied.")}
  }
  if (".MINC" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MINC] is supplied")}
    if (!uj:::cmp_nnw_scl(Dots$.MINC)) {Errors <- base::c(Errors, "[.MINC] must be a non-negative whole-number scalar (?uj::cmp_nnw_scl) if supplied.")}
  }
  if (".MAXC" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MAXC] is supplied")}
    if (!uj:::cmp_nnw_scl(Dots$.MAXC)) {Errors <- base::c(Errors, "[.MAXC] must be a non-negative whole-number scalar (?uj::cmp_nnw_scl) if supplied.")}
  }
  if (".VALS" %in% Names) {
    if (!uj:::.cmp_atm_vec(Dots$.VALS)) {Errors <- base::c(Errors, "[.VALS] must be a complete atomic vector (?uj::cmp_atm_vec) if supplied.")}
    else if (!uj:::.compat(x, Dots$.VALS)) {Errors <- base::c(Errors, "[x] and [.VALS] must be compatible modes.")}
  }
  if (".LT" %in% Names) {
    if (!uj:::.cmp_srt_scl(Dots$.LT)) {Errors <- base::c(Errors, "[.LT] must be a complete sortable scalar (?uj::cmp_srt_scl).")}
    else if (!uj:::.compar(x, Dots$.LT)) {Errors <- base::c(Errors, "[x] and [.LT] must be of comparable, sortable modes.")}
  }
  if (".LE" %in% Names) {
    if (!uj:::.cmp_srt_scl(Dots$.LE)) {Errors <- base::c(Errors, "[.LE] must be a complete sortable scalar (?uj::cmp_srt_scl).")}
    else if (!uj:::.compar(x, Dots$.LE)) {Errors <- base::c(Errors, "[x] and [.LE] must be of comparable, sortable modes.")}
  }
  if (".GE" %in% Names) {
    if (!uj:::.cmp_srt_scl(Dots$.GE)) {Errors <- base::c(Errors, "[.GE] must be a complete sortable scalar (?uj::cmp_srt_scl).")}
    else if (!uj:::.compar(x, Dots$.GE)) {Errors <- base::c(Errors, "[x] and [.VALS] must be of comparable, sortable modes.")}
  }
  if (".GT" %in% Names) {
    if (!uj:::.cmp_srt_scl(Dots$.GT)) {Errors <- base::c(Errors, "[.GT] must be a complete sortable scalar (?uj::cmp_srt_scl).")}
    else if (!uj:::.compar(x, Dots$.GT)) {Errors <- base::c(Errors, "[x] and [.GT] must be of comparable, sortable modes.")}
  }
  if (!base::is.null(Errors)) {uj::stopper(Errors, .FUN = "meets", .PKG = "uj")}
}

# value_exists ####

.value_errs <- function(name, .ERR = TRUE, .GENS = 1) {
  Fun <- uj::caller()
	Errors <- NULL
	if (!uj:::.cmp_chr_scl(name)) {Errors <- base::c(Errors, "[name] must be a complete character scalar (?cmp_chr_scl).")}
	if (!uj:::.cmp_psw_scl(.GENS)) {Errors <- base::c(Errors, "[.GENS] must be a positive whole number scalar (?cmp_psw_scl).")}
	if (!uj:::.cmp_lgl_scl(.ERR)) {Errors <- base::c(Errors, "[.ERR] must be TRUE or FALSE.")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, FUN = Fun, .PKG = "uj")}
}

# na ####

.ok_vals <- function(x) {!base::is.na(x)}

# rd ####

.rd_filename <- function(file, type) {
	if      (type == "csv") {Prompt <- "comma separated values (.csv) text file"}
	else if (type == "tsv") {Prompt <- "tab separated values (.tsv) text file"}
	else if (type == "xsv") {Prompt <- "delimited text file"}
	if (base::is.null(file)) {file <- uj::choose_doc(Prompt)}
	if (!uj:::.cmp_chr_vec(file)) {uj::stopperr("[file] must be NULL or a complete character vec (?cmp_chr_vec)", FUN = base::paste0("rd_", type), .PKG = "uj")}
  file <- base::paste0(file, collapse = "")
	if (!base::file.exists(file)) {uj::stopperr(base::paste0("the specified file ('", file, "') does not exist."), FUN = base::paste0("rd_", type), .PKG = "uj")}
  file
}

# pals ####

.pal_errs <- function(tot, unq, max.unq, .STACK) {
  Fun <- uj::caller()
  Errs <- NULL
  if (!uj:::.cmp_psw_scl(tot)) {Errs <- base::c(Errs, "[tot] must be a positive whole number scalar.")}
  if (!uj:::.cmp_psw_scl(unq)) {Errs <- base::c(Errs, "[unq] must be a positive whole number scalar.")}
  if (!base::is.null(Errs)) {uj::stopperr(Errs, .FUN = Fun, .PKG = "uj", .STACK = .STACK)}
  if (unq > max.unq) {uj::stopperr("[unq] is greater than the number of unique values available for the designated palette.", .FUN = Fun, .PKG = "uj", .STACK = .STACK)}
}

.pal_vals <- function(type, tot, unq) {
	Vals <- uj::run("uj::v(", type, ")")[1:unq]
	while (base::length(Vals) < tot) {Vals <- base::c(Vals, Vals)}
	Vals[1:tot]
}

# ipat ####

.pat_errs <- function(x, pat, .STACK) {
	Errors <- NULL
	Fun <- uj::caller()
	if (!uj:::.cmp_chr_vec(x)) {base::c(Errors, "[x] must be a complete character vec (?cmp_chr_vec).")}
	if (!uj:::.cmp_str_scl(pat)) {base::c(Errors, "[pat] must be a complete string scalar (?cmp_str_scl).")}
	uj::stopperr(Errors, .FUN = Fun, .PKG = "uj", .STACK = .STACK)
}

# Nth ####

.nth_errs <- function(x, n, .SCL.COUNT, .STACK) {
  OkN <- uj::f0(.SCL.COUNT, uj:::.cmp_psw_scl(n), uj:::.cmp_psw_vec(n))
	Fun <- uj::caller()
	Errors <- NULL
	if (!uj:::.pop_vec(x)) {Errors <- base::c(Errors, "[x] is not a populated vector (?pop_vec).")}
	if (.SCL.COUNT & !OkN) {Errors <- base::c(Errors, "[n] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
	if (!.SCL.COUNT & !OkN) {Errors <- base::c(Errors, "[n] must be a complete positive whole-number vector (?cmp_psw_vec).")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, .FUN = Fun, .PKG = "uj", .STACK = .STACK)}
	if (base::any(n > base::length(x))) {uj::stopperr(base::paste0(uj::f0(.SCL.COUNT, "", "The largest value in")," [n] is greater than the number of elements in [x]."), FUN = Fun, .PKG = "uj", .STACK = .STACK)}
}

# r ####

.r_errs <- function(Fun, .STACK, ..., r = NULL, e = NULL) {
	Errors <- NULL
	if (!base::all(base::sapply(base::list(...), uj:::.atm_vec))) {Errors <- base::c(Errors, "Arguments in [...] must be atomic vecs (?atm_vec).")}
	if (!base::ifelse(base::is.null(r), T, uj:::.cmp_psw_scl(r))) {Errors <- base::c(Errors, "[r] must be a positive whole-number scalar (?cmp_psw_scl).")}
	if (!base::ifelse(base::is.null(e), T, uj:::.cmp_psw_scl(e))) {Errors <- base::c(Errors, "[e] must be a positive whole-number scalar (?cmp_psw_scl).")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, .FUN = Fun, .PKG = 'uj', .STACK = .STACK)}
}

# dialog ####

