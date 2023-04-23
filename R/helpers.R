# failsafe ####

# are x and y of compatible (sortable or unsortable) modes
.compat <- function(X, Y) {
  if (base::is.character(X) & base::is.character(Y)) {TRUE}
  else if (base::is.logical(X) & base::is.logical(Y)) {TRUE}
  else if (base::is.numeric(X) & base::is.numeric(Y)) {TRUE}
  else if (base::is.ordered(X) & base::is.ordered(Y)) {
    xLevels <- base::levels(X)
    yLevels <- base::levels(Y)
    if (base::length(xLevels) != base::length(yLevels)) {FALSE}
    else {base::all(xLevels == yLevels)}
  }
  else if (base::is.factor(X) & !base::is.ordered(Y)) {
    xLevels <- base::levels(X)
    yLevels <- base::levels(Y)
    if (base::length(xLevels) != base::length(yLevels)) {FALSE}
    else {base::setequal(xLevels, yLevels)}
  } else {FALSE}
}

# are x and y of comparable (sortable) modes
.compar <- function(X, Y) {
  if (base::is.character(X) & base::is.character(Y)) {TRUE}
  else if (base::is.logical(X) & base::is.logical(Y)) {TRUE}
  else if (base::is.numeric(X) & base::is.numeric(Y)) {TRUE}
  else if (base::is.ordered(X) & base::is.ordered(Y)) {
    xLevels <- base::levels(X)
    yLevels <- base::levels(Y)
    if (base::length(xLevels) != base::length(yLevels)) {FALSE}
    else {base::all(xLevels == yLevels)}
  } else {FALSE}
}

# are x and y of recyclable lengths (1, max(c(length(x), length(y))))
.mismatch_n <- function(X, Y) {
  nX <- base::length(X)
  nY <- base::length(Y)
  nXY <- base::c(nX, nY)
  nValid <- base::c(1, nXY)
  nX == 0 | nY == 0 | !base::all(nXY %in% nValid)
}

# checkerr ####

# check whether Dots are named
.all_named <- function(...) {
	nDots <- base::...length()
	DotNames <- base::...names()
	if (nDots == 0) {F} else if (base::length(DotNames) != nDots) {F} else {!base::any(base::is.na(DotNames))}
}

# simple oxford comma lists
.ox_vals <- function(X, Join) {
	N <- base::length(X)
	if (N == 1) {X} else if (N == 2) {base::paste0(X[1], " ", Join, " ", X[2])} else {base::paste0(base::paste0(X[1:(N - 1)], collapse = ", "), ", ", Join, " ", X[N])}
}

# ppp_specs ####

.spec2combos <- function(X) {
	if (base::is.character(X)) {
		X <- uj::av(base::strsplit(X, "|", fixed = T))
		X <- X[X != ""]
		if (base::length(X) > 0) {base::unique(base::tolower(X))} else {NULL}
	} else {NULL}
}

.spec2props <- function(X) {
	if (base::is.character(X)) {
		X <- uj::av(base::strsplit(X, "|", fixed = T))
		X <- uj::av(base::strsplit(X, "_", fixed = T))
		X <- X[X != ""]
		if (base::length(X) > 0) {
			if (base::any(base::is.na(X))) {NULL} else if (base::any(base::nchar(X) != 3)) {NULL} else {base::unique(base::tolower(X))}
		} else {NULL}
	} else {NULL}
}

.combo2props <- function(X) {
	if (base::is.character(X)) {
		if (base::length(uj:::.spec2combos(X)) == 1) {
			X <- uj::av(base::strsplit(X, "_", fixed = T))
			X <- X[X != ""]
			if (base::length(X) > 0) {
				if (base::any(base::is.na(X))) {NULL} else if (base::any(base::nchar(X) != 3)) {NULL} else {base::unique(base::tolower(X))}
			} else {NULL}
		} else {NULL}
	} else {NULL}
}

# N ###

.n_errs <- function(N = NULL, Min = NULL, Max = NULL, Eq = FALSE, A = FALSE, Na = FALSE) {
	Errors <- NULL
	if (!base::is.null(N) & (!uj:::.cmp_nnw_vec(N))) {Errors <- base::c(Errors, "[N] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec).")}
	if (!base::is.null(Min) & (!uj:::.cmp_nnw_scl(min))) {Errors <- base::c(Errors, "[Min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
	if (!base::is.null(Max) & (!uj:::.cmp_nnw_scl(max))) {Errors <- base::c(Errors, "[max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
	if (!uj::fs_tf(Eq)) {Errors <- base::c(Errors, "[Eq] must be TRUE or FALSE.")}
	if (!uj::fs_tf(A)) {Errors <- base::c(Errors, "[A] must be TRUE or FALSE.")}
	if (!uj::fs_tf(Na)) {Errors <- base::c(Errors, "[Na] must be TRUE or FALSE.")}
	Errors
}

# delim ####

.delim_errs <- function(Args, A) {
	N <- uj::N(Args$Dots)
	Ns <- uj::ns(Args$Dots)
	Two <- "D" %in% base::names(Args)
	Dots <- N > 0
	Pops <- uj::f0(!Dots, T, base::all(Ns > 0))
	CCSd <- uj:::.cmp_chr_scl(Args$d)
	CCSD <- uj::f0(!Two, T, uj:::.cmp_chr_scl(Args$D))
	Atms <- uj::f0(!CCSd, T, base::all(base::sapply(Args$Dots, uj::atm_vec)))
	Recs <- uj::f0(A | !Pops, T, uj::recyclable_ns(base::max(Ns) / Ns))
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

.color_errs <- function(X = "red", Y = "red", Lighten = 0, Darken = 0, R = 0, G = 0, B = 0, A = 0, H = 0, S = 0, V = 0, WX = 1, WY = 1, Comp = T, OkNaA = F) {
	Fun <- uj::caller()
	Errors <- NULL
	if (!uj:::.cmp_clr_vec(X) ) {Errors <- base::c(Errors, "[X] must be contain only valid character color representations.")}
	if (!uj:::.cmp_clr_vec(Y) ) {Errors <- base::c(Errors, "[Y] must be contain only valid character color representations.")}
	if (!uj:::.cmp_ppn_vec(Lighten)) {Errors <- base::c(Errors, "[Lighten] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(Darken)) {Errors <- base::c(Errors, "[Darken] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(B)) {Errors <- base::c(Errors, "[B] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(G)) {Errors <- base::c(Errors, "[G] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(H)) {Errors <- base::c(Errors, "[H] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(R)) {Errors <- base::c(Errors, "[R] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(S)) {Errors <- base::c(Errors, "[S] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(V)) {Errors <- base::c(Errors, "[V] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!(OkNaA & uj:::.NAV(A))) {if (!uj:::.cmp_ppn_vec(A)) {Errors <- base::c(Errors, "[A] must be a complete proportion vec (?cmp_ppn_vec).")}}
	if (!uj:::.cmp_pos_vec(WX)) {Errors <- base::c(Errors, "[wX] must be a complete positive numeric vec (?cmp_pos_vec).")}
	if (!uj:::.cmp_pos_vec(WY)) {Errors <- base::c(Errors, "[wY] must be a complete positive numeric vec (?cmp_pos_vec).")}
	if (!uj:::.cmp_lgl_scl(Comp)) {Errors <- base::c(Errors, "[Comp] must be TRUE or FALSE.")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, FUN = Fun, PKG = "uj")}
}

# crayon ####

.glue_args <- function(D, ...) {base::paste0(uj::av(...), collapse = D)}

.crayons_errs <- function(ST = NULL, BG = NULL, FG = NULL, D = " ", OkNullST = TRUE, OkNullBG = TRUE, OkNullFG = TRUE) {
	Fun <- uj::caller()
	ST <- uj::failsafe(ST)
	BG <- uj::failsafe(BG)
	FG <- uj::failsafe(FG)
	D <- uj::failsafe(D)
	OkST <- uj::f0(base::is.null(ST), OkNullST, uj:::.unq_chr_vec(ST, Valid = base::c(uj::v(CrayonStyles), base::toupper(uj::v(CrayonStyles)))))
	OkBg <- uj::f0(base::is.null(BG), OkNullBG, uj:::.cmp_chr_scl(BG, Valid = base::c(uj::v(CrayonBackgroundColors), base::toupper(uj::v(CrayonBackgroundColors)))))
	OkFg <- uj::f0(base::is.null(FG), OkNullFG, uj:::.cmp_chr_scl(FG, Valid = base::c(uj::v(CrayonForegroundColors), base::toupper(uj::v(CrayonForegroundColors)))))
	OkD <- uj:::.cmp_chr_scl(D)
	Errors <- NULL
	if (!OkBg) {Errors <- base::c(Errors, "[BG] must be a character scalar from bg_vals().")}
	if (!OkFg) {Errors <- base::c(Errors, "[FG] must be a character scalar from fg_vals().")}
	if (!OkST) {Errors <- base::c(Errors, "[ST] must be a unique character vec from st_vals().")}
	if (!OkD) {Errors <- base::c(Errors, "[D] must be a non-NA character scalar.")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, FUN = Fun, PKG = "uj")}
}

.match_bg_val <- function(X) {
  if      (X %in% uj::v(CrayonBlue)   ) {"blu"}
  else if (X %in% uj::v(CrayonCyan)   ) {"cyn"}
  else if (X %in% uj::v(CrayonGreen)  ) {"grn"}
  else if (X %in% uj::v(CrayonBlack)  ) {"blk"}
  else if (X %in% uj::v(CrayonMagenta)) {"mag"}
  else if (X %in% uj::v(CrayonRed)    ) {"red"}
  else if (X %in% uj::v(CrayonWhite)  ) {"wht"}
  else if (X %in% uj::v(CrayonYellow) ) {"ylw"}
  else                                  {"def"}
}

.match_fg_val <- function(X) {
  if      (X %in% uj::v(CrayonBlue)   ) {"blu"}
  else if (X %in% uj::v(CrayonCyan)   ) {"cyn"}
  else if (X %in% uj::v(CrayonGreen)  ) {"grn"}
  else if (X %in% uj::v(CrayonBlack)  ) {"blk"}
  else if (X %in% uj::v(CrayonMagenta)) {"mag"}
  else if (X %in% uj::v(CrayonRed)    ) {"red"}
  else if (X %in% uj::v(CrayonSilver) ) {"sil"}
  else if (X %in% uj::v(CrayonWhite)  ) {"wht"}
  else if (X %in% uj::v(CrayonYellow) ) {"ylw"}
  else                                  {"def"}
}

.match_st_val <- function(X) {
  if      (X %in% uj::v(CrayonBold)     ) {"bld"}
  else if (X %in% uj::v(CrayonItalic)   ) {"itl"}
  else if (X %in% uj::v(CrayonPlain)    ) {"pln"}
  else if (X %in% uj::v(CrayonUnderline)) {"und"}
  else                                    {"def"}
}

# dialog ####

.chr <- function(..., D = " ") {base::paste0(uj::av(...), collapse = D)}
.trm <- function(..., D = " ") {base::trimws(uj:::.chr(..., D = D), which = "both")}
.ssplit <- function(X) {base::trimws(uj::av(base::strsplit(X, "|", fixed = T)), which = "both", whitespace = "[ ]")}

.ok_fmt <- function(X) {
	if (base::is.null(X)) {return(F)} else if (base::is.character(X) & base::length(X) == 1) {if (!base::is.na(X)) {if (X == "") {return(TRUE)}}}
	X <- uj:::.ssplit(X)
	uj::f0(base::length(X) != 3, F, uj::f0(!(X[1] %in% uj::v(CrayonBackgroundColors)), F, uj::f0(!(X[2] %in% uj::v(CrayonForegroundColors)), F, base::all(X[3] %in% uj::v(CrayonStyles)))))
}

.cat_choose_list <- function(Options, Message, All, None, MinN, MaxN, FT, FS, FUN, STACK, .clear, .cancel = T) {
  if (.clear) {uj::xconsole()}
  if (FT != "") {
    TitleBG <- base::strsplit(FT, "|", fixed = T)[[1]][1]
    TitleFG <- base::strsplit(FT, "|", fixed = T)[[1]][2]
    TitleST <- base::strsplit(FT, "|", fixed = T)[[1]][3]
  } else {TitleBG <- TitleFG <- TitleST <- NULL}
  if (FS != "") {
    SubBG <- base::strsplit(FS, "|", fixed = T)[[1]][1]
    SubFG <- base::strsplit(FS, "|", fixed = T)[[1]][2]
  } else {SubBG <- SubFG <- NULL}
  uj::alert(Message, Title = "response required", FT = FT, FS = FS, .clear = .clear)
  base::cat("\n")
  if (.cancel) {
    if (TRUE) {base::cat(uj::txt("CODE   OPTION         ", BG = SubBG, FG = SubFG, ST = "bold"))}
    if (TRUE) {base::cat(      "\n   X   { CANCEL }")}
    if (None) {base::cat(      "\n   N   { NONE }")}
    if (All ) {base::cat(      "\n   A   { ALL }")}
  } else {
    if (TRUE) {base::cat(uj::txt("CODE   OPTION         ", BG = SubBG, FG = SubFG, ST = "bold"))}
    if (None) {base::cat(      "\n   N   { NONE }")}
    if (All ) {base::cat(      "\n   A   { ALL }")}
  }
  for (i in 1:base::length(Options)) {
    Code <- base::as.character(i)
    Prefix <- base::paste0(base::rep.int(" ", 4 - base::nchar(Code)), collapse = "")
    Infix <- "   "
    Option <- base::gsub(" ", " ", Options[i], fixed = T)
    base::cat("\n", Prefix, Code, Infix, Option, sep = "")
  }
  cat("\n\n")
  if (MinN == 1 & MaxN == 1) {base::cat(uj::txt("Enter the code for 1 of the above options:", BG = TitleBG, FG = TitleFG, ST = TitleST))}
  else if (MinN == MaxN) {base::cat(uj::txt("Enter a comma separated list of codes for", MinN, "of the above options:", D = " ", BG = TitleBG, FG = TitleFG, ST = TitleST))}
  else {base::cat(uj::txt("Enter a comma separated list of codes for", MinN, "to", MaxN, "of the above options:", D = " ", BG = TitleBG, FG = TitleFG, ST = TitleST))}
  Answer <- base::toupper(base::trimws(base::strsplit(base::readline(), ",", fixed = TRUE)[[1]], which = "both"))
  if (base::length(Answer) == 1) {
    if (.cancel & Answer == "X") {uj::stopperr("Canceled by user.", FUN = FUN, PKG = "uj", STACK = STACK)}
    if (None & Answer == "N") {return(NULL)}
    if (All & Answer == "A") {return(Options)}
    Answer <- base::as.numeric(Answer)
    if (!uj::cmp_psw_scl(Answer)) {uj::stopperr("Invalid selection code", FUN = FUN, PKG = "uj", STACK = STACK)}
    if (1 < MinN) {uj::stopperr("Too few options selected.", FUN = FUN, PKG = "uj", STACK = STACK)}
    if (Answer > uj::N(Options)) {uj::stopperr("Selection code is greater than the number of available options.", FUN = FUN, PGK = "uj", STACK = STACK)}
  } else {
    Answer <- base::as.numeric(Answer)
    if (!uj::cmp_psw_vec(Answer)) {uj::stopperr("Unrecognized selection code(s).", FUN = FUN, PKG = "uj", STACK = STACK)}
    if (uj::N(Answer) < MinN) {uj::stopperr("Too few options selected.", FUN = FUN, PKG = "uj", STACK = STACK)}
    if (uj::N(Answer) > MaxN) {uj::stopperr("Too many options selected.", FUN = FUN, PKG = "uj", STACK = STACK)}
    if (base::any(Answer > uj::N(Options))) {uj::stopperr("A selection code is greater than the number of available options.", FUN = FUN, PGK = "uj", STACK = STACK)}
  }
  Options[Answer]
}

# declare ####

.labs_ok <- function(X) {
	Alpha <- base::c(letters, LETTERS)
	Nums <- base::as.character(0:9)
	OK <- base::substr(X, 1, 1) %in% Alpha
	OK[!OK] <- base::substr(X[!OK], 1, 1) == "." & base::substr(X[!OK], 2, 2) %in% Alpha
	if (base::all(OK)) {
	  nChars <- base::nchar(X)
		OK <- OK & base::substr(X, nChars, nChars) %in% base::c(Alpha, Nums)
		uj::f0(!base::all(OK), F, base::all(uj::av(base::strsplit(X, "", fixed = T)) %in% base::c(Alpha, Nums, ".", "_")))
	} else {F}
}

# callers ####

.getfun <- function(X) {
  Default <- "..unknown.."
  X <- uj::av(base::strsplit(X, ":::", fixed = T))
  X <- uj::av(base::strsplit(X, "::", fixed = T))
  N <- base::length(X)
  if (N == 0) {X <- ""} else if (N > 1) {X <- X[2]}
  X <- uj::av(base::strsplit(X, "(", fixed = T))
  X <- uj::av(base::strsplit(X, " ", fixed = T))
  N <- base::length(X)
  if (N == 0) {X <- ""} else if (N > 1) {X <- X[1]}
  if (X == "") {Default} else {X}
}

.getpkg <- function(X) {
  Default <- "..??.."
  X <- uj::av(base::strsplit(X, ":::", fixed = T))
  X <- uj::av(base::strsplit(X, "::", fixed = T))
  N <- base::length(X)
  if (N < 2) {Default} else {X[1]}
}

.getsep <- function(X) {if (base::length(uj::av(base::strsplit(X, ":::", fixed = T))) > 1) {":::"} else {"::"}}

.stack2pkgfuns <- function(Stack, MaxLen, Vec = T, PkgOnly = F, FunOnly = F) {
  Default <- "..??.."
  Funs <- base::sapply(Stack, uj:::.getfun)
  Pkgs <- base::sapply(Stack, uj:::.getpkg)
  Seps <- base::sapply(Stack, uj:::.getsep)
  OK <- Funs != Default
  Funs <- Funs[OK]
  Pkgs <- Pkgs[OK]
  Seps <- Seps[OK]
  Pkgs[Pkgs == "..R.."] <- "[R]"
  Funs[Funs == "..command.line.."] <- "[command.line]"
  Pkgs[Pkgs == Default] <- "[??]"
  Funs[Funs == Default] <- "[??]"
  if (PkgOnly) {Pkgs} else if (FunOnly) {Funs} else if (Vec) {base::paste0(Pkgs, Seps, Funs)} else {tibble::tibble(Pkg = Pkgs, Fun = Funs)}
}

.stack2funs <- function(Stack, MaxLen) {
  Default <- "..??.."
  Funs <- base::sapply(Stack, uj:::.getfun)
  OK <- Funs != Default
  Funs <- Funs[OK]
  if (base::length(Funs) == 0) {Funs <- Default}
  Funs[Funs == "..command.line.."] <- "[command.line]"
  Funs[Funs == Default] <- "[??]"
  Funs
}

.stack2pkgs <- function(Stack, MaxLen) {
  Default <- "..??.."
  Funs <- base::sapply(Stack, uj:::.getfun)
  Pkgs <- base::sapply(Stack, uj:::.getpkg)
  Ok <- Funs != Default
  Pkgs <- Pkgs[Ok]
  if (base::length(Pkgs) == 0) {Pkgs <- Default}
  Pkgs[Pkgs == "..R.."] <- "[R]"
  Pkgs[Pkgs == Default] <- "[??]"
  Pkgs
}

.fun_pkg_stack <- function(Fun, Pkg, Stack, FUN, STACK) {
  if (Fun == "") {Fun <- FUN}
  if (Pkg == "") {Pkg <- "..??.."}
  if (Stack == "") {Stack <- STACK}
  Fun <- uj:::.stack2funs(Fun, 1000)
  Pkg <- uj:::.stack2pkgs(Pkg, 1000)
  Stack <- base::paste0(uj:::.stack2pkgfuns(Stack, MaxLen = 35, Vec = T), collapse = " >> ")
  base::list(Fun = Fun, Pkg = Pkg, Stack = Stack)
}

# meets ####

.meets_errs <- function(X, ...) {
	if (base::...length() == 0) {return(NULL)}
	Vars <- base::c('.n', '.nr', '.nc', '.min', '.minr', '.minc', '.max', '.maxr', '.maxc', '.vals', '.lt', '.le', '.le', '.ge', '.gt')
	Dots <- base::list(...)
	DotNames  <- base::names(Dots)
	PopNames  <- DotNames[DotNames != ""]
	UnqNames  <- base::unique(PopNames[PopNames != ""])
	AllUnique <- uj::f0(base::is.null(DotNames), T, base::setequal(PopNames, UnqNames))
	NoBlanks  <- uj::f0(base::is.null(DotNames), T, !base::any(DotNames == ""))
	AllValid  <- uj::f0(base::length(PopNames) == 0, T, base::all(DotNames %in% Vars))
	Errors <- NULL
	if (!AllValid) {Errors <- base::c(Errors, "Names of arguments in [...] must be in c('.n', '.nr', '.nc', '.min', '.minr', '.minc', '.max', '.maxr', '.maxc', '.vals', '.lt', '.le', '.ge', '.gt').")}
	if (!NoBlanks) {Errors <- base::c(Errors, "All arguments in [...] must be named.")}
	if (!AllUnique) {Errors <- base::c(Errors, "Arguments in [...] must be named uniquely.")}
	if (!base::is.null(Errors)) {return(Errors)}

	okVec <- function(Dot) {uj::f0(base::is.null(Dot), T, uj::f0(!base::is.atomic(Dot) | base::length(Dot) == 0, F, uj::f0(base::is.vector(Dot), T, uj::f0(!base::is.array(Dot), F, base::length(base::dim(Dot)) < 2))))}
	okScl <- function(Dot) {uj::f0(base::is.null(Dot), T, uj::f0(!okVec(Dot), F, base::length(Dot) == 1))}
	okIndVec <- function(Dot) {uj::f0(base::is.null(Dot), T, uj::f0(!okVec(Dot), F, !base::any(base::is.na(Dot)) & base::all(Dot >= 0 & base::all(Dot == base::round(Dot)))))}
	okIndScl <- function(Dot) {uj::f0(base::is.null(Dot), T, uj::f0(!okScl(Dot), F, !base::is.na(Dot) & Dot >= 0 & uj::rounded(Dot)))}
	okSrtVec <- function(Obj, Dot) {
	  ObjWild <- uj::f0(base::is.null(Obj), T, uj::f0(!base::is.atomic(Obj), F, uj::f0(base::length(Obj) == 0, T, base::all(base::is.na(Obj)))))
		ObjChr <- base::is.character(Obj); DotChr <- base::is.character(Dot)
		ObjNum <- base::is.numeric(Obj); DotNum <- base::is.numeric(Dot)
		ObjLgl <- base::is.logical(Obj); DotLgl <- base::is.logical(Dot)
		ObjOrd <- base::is.ordered(Obj); DotOrd <- base::is.ordered(Dot)
		ObjLevs <- uj::f0(!ObjOrd, NULL, base::levels(Obj)); ObjLevCount <- base::length(ObjLevs)
		DotLevs <- uj::f0(!DotOrd, NULL, base::levels(Dot)); DotLevCount <- base::length(DotLevs)
		uj::f0(base::is.null(Dot), T,
						uj::f0(!okVec(Dot), F,
									uj::f0(!DotNum & !DotLgl & !DotChr & !DotOrd, F,
													uj::f0(!base::is.atomic(Obj), T,
																uj::f0(ObjWild, T,
																				uj::f0(ObjChr & DotChr, T,
																							uj::f0(ObjNum & DotNum, T,
																											uj::f0(ObjLgl & DotLgl, T,
																														uj::f0(!ObjOrd | !DotOrd, F,
																																		uj::f0(ObjLevCount != DotLevCount, F,
																																					base::all(ObjLevs == DotLevs)))))))))))
	}
	okSrtScl <- function(Obj, Dot) {uj::f0(base::is.null(Dot), T, uj::f0(!okSrtVec(Obj, Dot), F, base::length(Dot) == 1))}
	lt <- Dots$.lt; min  <- Dots$.min ; max  <- Dots$.max ; n  <- Dots$.Nn
	le <- Dots$.le; minr <- Dots$.minr; maxr <- Dots$.maxr; nr <- Dots$.nr
	ge <- Dots$.ge; minc <- Dots$.minc; maxc <- Dots$.maxc; nc <- Dots$.nc
	gt <- Dots$.gt; vals <- Dots$.vals
	base::c(
		uj::f0(okIndVec(n), NULL, "[.n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
		uj::f0(okIndVec(nr), NULL, "[.nr] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
		uj::f0(okIndVec(nc), NULL, "[.nc] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
		uj::f0(okIndScl(min), NULL, "[.min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(max), NULL, "[.max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(minr), NULL, "[.minr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(minc), NULL, "[.minc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(maxr), NULL, "[.maxr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(maxc), NULL, "[.maxc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okSrtScl(X, lt), NULL, "[.lt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [X] (?comparable)."),
		uj::f0(okSrtScl(X, le), NULL, "[.le] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [X] (?comparable)."),
		uj::f0(okSrtScl(X, ge), NULL, "[.ge] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [X] (?comparable)."),
		uj::f0(okSrtScl(X, gt), NULL, "[.gt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [X] (?comparable)."),
		uj::f0(okSrtVec(X, vals), NULL, "[.vals] must be NULL or a complete atomic vec (?cmp_vec) comparable with [X] (?comparable).")
	)
}

# value_exists ####

.value_errs <- function(Name, Err = TRUE, Gens = 1) {
  Fun <- uj::caller()
	Errors <- NULL
	if (!uj:::.cmp_chr_scl(Name)) {Errors <- base::c(Errors, "[Name] must be a complete character scalar (?cmp_chr_scl).")}
	if (!uj:::.cmp_psw_scl(Gens)) {Errors <- base::c(Errors, "[Gens] must be a positive whole number scalar (?cmp_psw_scl).")}
	if (!uj:::.cmp_lgl_scl(Err)) {Errors <- base::c(Errors, "[Err] must be TRUE or FALSE.")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, FUN = Fun, PKG = "uj")}
}

# na ####

.ok_vals <- function(X) {!base::is.na(X)}

# rd ####

.rd_filename <- function(File, Type) {
	if      (Type == "csv") {Prompt <- "comma separated values (.csv) text file"}
	else if (Type == "tsv") {Prompt <- "tab separated values (.tsv) text file"}
	else if (Type == "xsv") {Prompt <- "delimited text file"}
	if (base::is.null(File)) {File <- uj::choose_doc(Prompt)}
	if (!uj:::.cmp_chr_vec(File)) {uj::stopperr("[File] must be NULL or a complete character vec (?cmp_chr_vec)", FUN = base::paste0("rd_", Type), PKG = "uj")}
  File <- base::paste0(File, collapse = "")
	if (!base::file.exists(File)) {uj::stopperr(base::paste0("the specified file ('", File, "') does not exist."), FUN = base::paste0("rd_", Type), PKG = "uj")}
  File
}

# pals ####

.pal_errs <- function(N, Stack) {
  Fun <- uj::caller()
  if (!uj:::.cmp_psw_scl(N)) {uj::stopperr("[N] must be a positive whole-number scalar.", FUN = Fun, PKG = "uj", STACK = Stack)}
}

.pal_vals <- function(Type, N) {
	X <- uj::run("uj::v(", Type, ")")
	while (base::length(X) < N) {X <- base::c(X, X)}
	X[1:N]
}

# ipat ####

.pat_errs <- function(X, Pat, Stack) {
	Errors <- NULL
	Fun <- uj::caller()
	if (!uj:::.cmp_chr_vec(X)) {base::c(Errors, "[X] must be a complete character vec (?cmp_chr_vec).")}
	if (!uj:::.cmp_str_scl(Pat)) {base::c(Errors, "[pat] must be a complete string scalar (?cmp_str_scl).")}
	uj::stopperr(Errors, FUN = Fun, PKG = "uj", STACK = Stack)
}

# Nth ####

.Nth_errs <- function(X, N, SclCount, Stack) {
  OkN <- uj::f0(SclCount, uj:::.cmp_psw_scl(N), uj:::.cmp_psw_vec(N))
	Fun <- uj::caller()
	Errors <- NULL
	if (!uj:::.pop_vec(X)) {Errors <- base::c(Errors, "[X] is not a populated vector (?pop_vec).")}
	if (SclCount & !OkN) {Errors <- base::c(Errors, "[N] must be a complete positive whole-number scalar (?cmp_psw_sco).")}
	if (!SclCount & !OkN) {Errors <- base::c(Errors, "[N] must be a complete positive whole-number vector (?cmp_psw_vec).")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, FUN = Fun, PKG = "uj", STACK = Stack)}
	if (base::any(N > base::length(X))) {uj::stopperr(base::paste0(uj::f0(SclCount, "", "The largest value in")," [N] is greater than the number of elements in [x]."), FUN = Fun, PKG = "uj")}
}

# r ####

.r_errs <- function(Fun, Stack, ..., R = NULL, E = NULL) {
	Errors <- NULL
	if (!base::all(base::sapply(base::list(...), uj:::.atm_vec))) {Errors <- base::c(Errors, "Arguments in [...] must be atomic vecs (?atm_vec).")}
	if (!base::ifelse(base::is.null(R), T, uj:::.cmp_psw_scl(R))) {Errors <- base::c(Errors, "[R] must be a positive whole-number scalar (?cmp_psw_scl).")}
	if (!base::ifelse(base::is.null(E), T, uj:::.cmp_psw_scl(E))) {Errors <- base::c(Errors, "[E] must be a positive whole-number scalar (?cmp_psw_scl).")}
	if (!base::is.null(Errors)) {uj::stopperr(Errors, FUN = Fun, PKG = 'uj', STACK = Stack)}
}

# dialog ####

