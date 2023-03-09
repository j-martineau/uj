# failsafe ####

# are x and y of compatible (sortable or unsortable) modes
.compat <- function(x, y) {
  if      (base::is.character(x) & base::is.character(y)) {TRUE}
  else if (base::is.logical(x) & base::is.logical(y)) {TRUE}
  else if (base::is.numeric(x) & base::is.numeric(y)) {TRUE}
  else if (base::is.ordered(x) & base::is.ordered(y)) {
    lvx <- base::levels(x)
    lvy <- base::levels(y)
    if (base::length(lvx) != base::length(lvy)) {FALSE}
    else {base::all(lvx == lvy)}
  }
  else if (base::is.factor(x) & uj::is_fac(y)) {
    lvx <- base::levels(x)
    lvy <- base::levels(y)
    if (base::length(lvx) != base::length(lvy)) {FALSE}
    else {base::setequal(lvx, lvy)}
  } else {FALSE}
}

# are x and y of comparable (sortable) modes
.compar <- function(x, y) {
  if      (base::is.character(x) & base::is.character(y)) {TRUE}
  else if (base::is.logical(x) & base::is.logical(y)) {TRUE}
  else if (base::is.numeric(x) & base::is.numeric(y)) {TRUE}
  else if (base::is.ordered(x) & base::is.ordered(y)) {
    lvx <- base::levels(x)
    lvy <- base::levels(y)
    if (base::length(lvx) != base::length(lvy)) {FALSE}
    else {base::all(lvx == lvy)}
  } else {FALSE}
}

# are x and y of recyclable lengths (1, max(c(length(x), length(y))))
.mismatch_n <- function(x, y) {
  nx <- base::length(x)
  ny <- base::length(y)
  nxy <- base::c(nx, ny)
  nvalid <- base::c(1, nxy)
  nx == 0 | ny == 0 | !base::all(nxy %in% nvalid)
}

# checkerr ####

# check whether dots are named
.all_named <- function(...) {
	ndots <- base::...length()
	labs <- base::...names()
	if (ndots == 0) {F} else if (base::length(labs) != ndots) {F} else {!base::any(base::is.na(labs))}
}

# simple oxford comma lists
.ox_vals <- function(x, join) {
	n <- base::length(x)
	if (n == 1) {x} else if (n == 2) {base::paste0(x[1], " ", join, " ", x[2])} else {base::paste0(base::paste0(x[1:(n - 1)], collapse = ", "), ", ", join, " ", x[n])}
}

# get function, package, and stack for throwing an error
.fun_pkg_stack <- function(f, p, s, c1, cs) {
	pkg_fun <- function(stack, lim, vec = T, pkg.only = F, fun.only = F) {
		bad <- "< bad >"
		unknown1 <- "..unknown.."
		unknown2 <- "{ unknown }"
		command1 <- "..command.line.."
		command2 <- "{ command line }"
		posP <- base::regexpr("(" , stack, fixed = T) - 1
		pos2 <- base::regexpr("::", stack, fixed = T) - 1
		pos3 <- base::regexpr(":::", stack, fixed = T) - 1
		iP <- posP > 0
		i2 <- pos2 > 0
		i3 <- pos3 > 0
		fun <- pkg <- base::rep.int(unknown1, uj::N(stack))
		pkg[i2] <- base::substr(stack[i2], 1, pos2[i2])
		pkg[i3] <- base::substr(stack[i3], 1, pos3[i3])
		fun.start <- base::pmax(1, pos2, pos3)
		fun[iP] <- base::substr(stack[iP], fun.start[iP], posP[iP])
		nch.pkg <- base::nchar(pkg)
		nch.fun <- base::nchar(fun)
		abb.pkg <- lim < nch.pkg
		abb.fun <- lim < nch.fun
		last.pkg <- base::pmin(lim, nch.pkg - 3)
		last.fun <- base::pmin(lim, nch.fun - 3)
		pkg[abb.pkg] <- base::paste0(base::substr(pkg[abb.pkg], 1, last.pkg[abb.pkg]), "...")
		fun[abb.fun] <- base::paste0(base::substr(fun[abb.fun], 1, last.fun[abb.fun]), "...")
		pkg <- base::sapply(pkg, uj:::.bad_if_not_name)
		fun <- base::sapply(fun, uj:::.bad_if_not_name)
		wild <- base::c(bad, unknown1)
		bad.fun <- fun %in% wild
		pkg <- pkg[!bad.fun]
		fun <- fun[!bad.fun]
		i2 <- i2[!bad.fun]
		i3 <- i3[!bad.fun]
		pkg[pkg %in% wild] <- unknown1
		pkg[fun == command1] <- command2
		fun[fun == command1] <- command2
		if (base::length(pkg) == 0) {
			pkg <- fun <- "{ command line }"
			i3 <- F
		} else {pkg[pkg == unknown1] <- unknown2}
		if (!pkg.only & !fun.only) {
			if (vec) {
				sep <- base::rep("::", uj::N(pkg))
				sep[i3] <- base::paste0(sep[i3], ":")
				base::paste0(pkg, sep, fun)
			} else {base::list(pkg = pkg, fun = fun)}
		} else if (pkg.only) {pkg}
		else {fun}
	}
	f <- uj::failsafe(f)
	p <- uj::failsafe(p)
	s <- uj::failsafe(s)
	if (!uj:::.cmp_chr_scl(f)) {f <- ""}
	if (!uj:::.cmp_chr_scl(p)) {p <- ""}
	if (!uj:::.cmp_chr_scl(s)) {s <- ""}
	if (base::length(f) != 1) {f <- "{ command line }"}
	if (base::length(p) != 1) {p <- "{ unknown }"}
	if (base::length(s) == 1) {if (s == "") {s <- uj::callers()}}
	if (f == "") {f <- pkg_fun(caller, 100, F)$fun[1]}
	if (p == "") {p <- pkg_fun(caller, 100, F)$pkg[1]}
	stack <- pkg_fun(s, 35, T)
	if (base::length(s) > 2) {s <- s[2:base::length(s)]} else {s <- "command line"}
	s <- base::paste0("callstack = { ", base::paste0(s, collapse = " > "), " }")
	base::list(fun = f, pkg = p, stack = s)
}

# PPP ####

# ppp families
.mmm <- base::c("atm", "ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")
.ccc <- base::c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls")
.sss <- base::c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")
.bbb <- base::c("atm", "def", "fun", "nil", "nll", "pop", "rcr")
.iii <- base::c("cmp", "dup", "mss", "na0", "ok0", "prt", "unq")
.eee <- base::c("e0D", "e1D", "e2D", "eHD", "eUD")
.ddd <- base::c("d0D", "d1D", "d2D", "dHD")
.ppp <- base::unique(base::sort(base::c(uj:::.bbb, uj:::.ccc, uj:::.ddd, uj:::.eee, uj:::.iii, uj:::.mmm, uj:::.sss)))

# PPP families
.MMM <- base::toupper(uj:::.mmm)
.CCC <- base::toupper(uj:::.ccc)
.SSS <- base::toupper(uj:::.sss)
.BBB <- base::toupper(uj:::.bbb)
.III <- base::toupper(uj:::.iii)
.EEE <- base::toupper(uj:::.eee)
.DDD <- base::toupper(uj:::.ddd)
.PPP <- base::unique(base::sort(base::c(uj:::.BBB, uj:::.CCC, uj:::.DDD, uj:::.EEE, uj:::.III, uj:::.MMM, uj:::.SSS)))
.ATM <- function(x) {if (base::is.atomic(x)) {!base::is.null(x)} else {F}}
.DEF <- function(x) {!base::is.null(x)}
.FUN <- function(x) {
	if (!base::is.function(x)) {
		if (base::length(x) == 1 & base::is.character(x)) {
			x <- tryCatch(base::match.fun(x), error = function(x) e, finally = NULL)
			!testthat::is.error(x)
		} else {F}
	} else {T}
}

# basic
.NIL <- function(x) {base::length(x) == 0}
.NLL <- function(x) {uj::null(x)}
.POP <- function(x) {base::length(x) > 0}
.RCR <- function(x) {base::is.recursive(x)}

# xclass
.ARR <- function(x) {base::is.array(x)}
.DTF <- function(x) {base::is.data.frame(x)}
.GEN <- function(x) {base::is.array(x) | base::is.vector(x)}
.MAT <- function(x) {base::is.matrix(x)}
.SCL <- function(x) {if (base::length(x) != 1) {F} else {base::is.vector(x) | base::is.array(x)}}
.VLS <- function(x) {if (base::is.data.frame(x)) {F} else {base::is.list(x)}}
.MVC <- function(x) {if (base::length(x) < 2) {F} else if (base::is.vector(x)) {T} else if (!base::is.array(x)) {F} else {base::length(base::which(base::dim(x) > 1)) == 1}}
.VEC <- function(x) {if (base::length(x) == 0) {F} else if (base::is.vector(x)) {T} else if (!base::is.array(x)) {F} else {base::length(base::which(base::dim(x) > 1)) < 2}}

# defined D
.D0D <- function(x) {uj::null(x)}
.D1D <- function(x) {base::is.vector(x)}
.D2D <- function(x) {base::is.matrix(x) | base::is.data.frame(x)}
.DHD <- function(x) {if (is.array(x)) {base::length(base::dim(x)) > 2} else {F}}

# effective D
.EUD <- function(x) {base::length(x) == 0}
.E2D <- function(x) {base::length(base::which(base::dim(x) > 1)) == 2}
.EHD <- function(x) {base::length(base::which(base::dim(x) > 1)) > 2}
.E0D <- function(x) {if (base::is.atomic(x)) {base::length(x) == 1} else if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 1} else if (base::is.list(x)) {base::length(x) == 1} else {F}}
.E1D <- function(x) {
	if (base::is.atomic(x)) {
		if (base::is.vector(x)) {base::length(x) > 1}
		else if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) == 1}
		else {F}
	} else if (base::is.data.frame(x)) {
		if (base::NROW(x) == 1) {base::NCOL(x) > 1}
		else if (base::NCOL(x) == 1) {base::NROW(x) > 1}
		else {F}
	} else if (base::is.list(x)) {base::length(x) > 1}
	else {F}
}

# integrity
.NA0 <- function(x) {if (!base::is.atomic(x)) {F} else if (base::length(x) == 1) {F} else {base::is.na(x)}}
.OK0 <- function(x) {if (!base::is.atomic(x)) {F} else if (base::length(x) == 1) {F} else {!base::is.na(x)}}
.DUP <- function(x) {if (!base::is.atomic(x)) {if (base::length(x) == 0) {F} else if (base::any(base::is.na(x))) {F} else {base::length(x) != base::length(base::unique(x))}} else {F}}
.UNQ <- function(x) {if (!base::is.atomic(x)) {if (base::length(x) == 0) {F} else if (base::any(base::is.na(x))) {F} else {base::length(x) == base::length(base::unique(x))}} else {F}}

.CMP <- function(x) {
	if (base::length(x) == 0) {F}
	else if (base::is.atomic(x)) {!base::any(base::is.na(x))}
	else if (base::is.data.frame(x)) {base::all(base::apply(x, 2, uj:::.CMP))}
	else if (base::is.list(x)) {base::all(base::sapply(x, uj:::.CMP))}
	else {F}
}

.MSS <- function(x) {
	if (base::length(x) == 0) {F}
	else if (base::is.atomic(x)) {base::all(base::is.na(x))}
	else if (base::is.data.frame(x)) {base::all(base::apply(x, 2, uj:::.MSS))}
	else if (base::is.list(x)) {base::all(base::sapply(x, uj:::.MSS))}
	else {F}
}

.PRT <- function(x) {
	if (base::length(x) == 0) {F}
	else if (base::is.atomic(x)) {base::any(base::is.na(x)) & base::any(!base::is.na(x))}
	else if (base::is.data.frame(x)) {
		if (base::NROW(x) == 0) {F}
		else if (base::NCOL(x) == 0) {F}
		else if (!base::all(base::apply(x, 2, base::is.atomic))) {F}
		else {uj:::.PRT(base::unlist(x, T, F))}
	} else if (base::is.list(x)) {
		if (base::any(base::lengths(x) == 0)) {F}
		else if (!base::all(base::sapply(x, base::is.atomic))) {F}
		else {uj:::.PRT(base::unlist(x, T, F))}
	} else {F}
}

# xmode
.CH1 <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.character(x)) {
					x <- x[!base::is.na(x)]
					if (base::length(x) == 0) {T}
					else {base::all(base::nchar(x) == 1)}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.CH3 <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.character(x)) {
					x <- x[!base::is.na(x)]
					if (base::length(x) == 0) {T}
					else {base::all(base::nchar(x) == 3)}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.CHR <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (base::is.character(x)) {T}
			else {base::all(base::is.na(x))}
		} else {T}
	} else {F}
}

.STR <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.character(x)) {
					x <- x[!base::is.na(x)]
					if (base::length(x) == 0) {T}
					else {base::all(base::nchar(x) > 0)}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.CLR <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				x <- x[!base::is.na(x)]
				if (base::length(x) > 0) {!uj::is_err(grDevices::col2rgb(x))}
				else {F}
			} else {T}
		} else {T}
	} else {F}
}

.FAC <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (base::is.factor(x)) {T}
			else {base::all(base::is.na(x))}
		} else {T}
	} else {F}
}

.ORD <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (base::is.ordered(x)) {T}
			else {base::all(base::is.na(x))}
		} else {T}
	} else {F}
}

.UNO <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (base::is.factor(x) & !base::is.ordered(x)) {T}
			else {base::all(base::is.na(x))}
		} else {T}
	} else {F}
}

.IND <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				x <- x[!base::is.na(x)]
				if (base::is.numeric(x)) {base::all(x == base::round(x))}
				else {base::is.logical(x)}
			} else {T}
		} else {T}
	} else {F}
}

.NST <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {!base::is.character(x) & !base::is.numeric(x) & !base::is.ordered(x)} else {T}
		} else {T}
	} else {F}
}

.SRT <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {base::is.character(x) | base::is.numeric(x) | base::is.ordered(x)} else {T}
		} else {T}
	} else {F}
}

.LGL <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (base::is.logical(x)) {T} else {base::all(base::is.na(x))}
		} else {T}
	} else {F}
}

.NUM <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (base::is.numeric(x)) {T} else {base::all(base::is.na(x))}
		} else {T}
	} else {F}
}

.WHL <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::all(x == base::round(x))
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.ODD <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					if (base::all(x == base::round(x))) {
						x <- x / 2
						base::any(x != base::round(x))
					} else {F}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.EVN <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					if (base::all(x == base::round(x))) {
						x <- x / 2
						base::all(x == base::round(x))
					} else {F}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.NGW <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					if (base::all(x == base::round(x))) {base::all(x < 0)} else {F}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.NNW <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					if (base::all(x == base::round(x))) {base::all(x >= 0)} else {F}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.NPW <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					if (base::all(x == base::round(x))) {base::all(x <= 0)} else {F}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.PSW <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					if (base::all(x == base::round(x))) {base::all(x > 0)} else {F}
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.FRC <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::any(x != base::round(x))
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.NEG <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::all(x < 0)
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.POS <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::all(x > 0)
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.NNW <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::all(x >= 0)
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.NPS <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::all(x <= 0)
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.PCT <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::all(x >= 0 | x <= 100)
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

.PPN <- function(x) {
	if (base::is.atomic(x)) {
		if (base::length(x) > 0) {
			if (!base::all(base::is.na(x))) {
				if (base::is.numeric(x)) {
					x <- x[!base::is.na(x)]
					base::all(x >= 0 | x <= 1)
				} else {F}
			} else {T}
		} else {T}
	} else {F}
}

# shape
.COL <- function(x) {if (base::is.data.frame(x) | base::is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) == 1} else {F}}
.ROW <- function(x) {if (base::is.data.frame(x) | base::is.matrix(x)) {base::NROW(x) == 1 & base::NCOL(x) > 1} else {F}}
.PNT <- function(x) {if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 1} else {base::length(x) == 1}}
.SLD <- function(x) {if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) > 2} else {F}}
.SQR <- function(x) {if (base::is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) > 1 & base::NROW(x) == base::NCOL(x)} else {F}}
.EMP <- function(x) {if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 0} else if (base::length(x) == 0) {!base::is.null(x)} else {F}}
.RCT <- function(x) {if (base::length(x) > 1) {if (base::is.data.frame(x) | is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) > 1} else {F}} else {F}}

.LIN <- function(x) {
	if (base::length(x) > 1) {
		if (base::is.data.frame(x)) {if (base::NROW(x) == 1 & base::NCOL(x) > 1) {T} else {base::NROW(x) > 1 & base::NCOL(x) == 1}}
	  else if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) == 2}
		else if (base::is.list(x)) {T}
		else if (base::is.vector(x)) {T}
	} else {F}
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

## .xxx_yyy ####
.atm_scl <- function(x) {base::is.atomic(x) & base::length(x) == 1}
.atm_mat <- function(x) {base::is.atomic(x) & base::is.matrix(x)}
.atm_vls <- function(x) {if (base::is.list(x) & !base::is.data.frame(x) & base::length(x) > 0) {base::all(base::sapply(x, base::is.atomic))} else {F}}
.atm_vec <- function(x) {base::is.atomic(x) & uj:::.VEC(x)}
.atm_dtf <- function(x) {if (!base::is.data.frame(x)) {F} else if (base::nrow(x) == 0 | base::ncol(x) == 0) {F} else {base::all(base::apply(x, 2, base::is.atomic))}}
.chr_arr <- function(x) {base::is.array(x) & base::is.character(x)}
.chr_dtf <- function(x) {uj::f0(base::is.data.frame(x), base::all(base::apply(x, 2, base::is.character)), F)}
.chr_vec <- function(x) {uj:::.VEC(x) & base::is.character(x)}
.chr_vls <- function(x) {uj::f0(base::is.list(x) & !is.data.frame(x), base::all(base::sapply(x, base::is.charsacter)), F)}
.cmp_chr <- function(x) {if (base::is.character(x) & base::length(x) > 0) {!base::any(base::is.na(x))} else {F}}
.cmp_num <- function(x) {if (base::is.numeric(x) & base::length(x) > 0) {!base::any(base::is.na(x))} else {F}}
.cmp_scl <- function(x) {if (base::is.atomic(x) & base::length == 1) {!base::is.na(x)} else {F}}
.cmp_vec <- function(x) {if (!base::is.atomic(x) | base::length(x) == 0) {F} else if (!uj:::.VEC(x)) {F} else {!base::any(base::is.na(x))}}
.lgl_scl <- function(x) {base::logical(x) & base::length(x) == 1}
.pop_atm <- function(x) {base::is.atomic(x) & base::length(x) > 0}
.pop_chr <- function(x) {base::length(x) > 0 & base::is.character(x)}
.pop_dtf <- function(x) {base::is.data.frame(x) & base::NROW(x) > 0 & base::NCOL(x) > 0}
.pop_mat <- function(x) {base::is.matrix(x) & base::length(x) > 0}
.pop_srt <- function(x) {base::length(x) > 0 & (base::is.character(x) | base::is.numeric(x) | base::is.ordered(x))}
.pop_vec <- function(x) {uj:::.VEC(x) & base::length(x) > 0}
.pop_vls <- function(x) {uj:::.VLS(x) & base::length(x) > 0}

## .xxx_yyy_zzz ####
.cmp_atm_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.atomic(x)) {F} else if (base::any(base::is.na(x))) {F}else {T}}
.cmp_ch1_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {base::nchar(x) == 1}}
.cmp_ch1_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.character(x)) {F} else if (base::any(base::is.na(x))) {F} else {base::all(base::nchar(x) == 1)}}
.cmp_ch3_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {base::nchar(x) == 3}}
.cmp_ch3_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {base::all(base::nchar(x) == 3)}}
.cmp_chr_scl <- function(x, valid = NULL) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else if (!base::is.null(valid)) {x %in% valid} else {T} }
.cmp_chr_vec <- function(x, valid = NULL) {if (!uj:::.VEC(x)) {F} else if (!base::is.character(x)) {F} else if (base::any(base::is.na(x))) {F} else if (!base::is.null(valid)) {base::all(x %in% valid)} else {T}}
.cmp_clr_vec <- function(x) {uj:::.cmp_vec(x) & uj:::.CLR(x)}
.cmp_lgl_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.logical(x)) {F} else {!base::is.na(x)}}
.cmp_lgl_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.logical(x)) {F} else {!base::any(base::is.na(x))}}
.cmp_nng_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else {x < 0}}
.cmp_nnw_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else if (x < 0) {F} else {x == round(x)}}
.cmp_nnw_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else if (base::any(x < 0)) {F} else {base::all(x == round(x))}}
.cmp_num_scl <- function(x, valid = NULL) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else if (!base::is.null(valid)) {x %in% valid} else {T}}
.cmp_num_vec <- function(x, valid = NULL) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else if (!base::is.null(valid)) {base::all(x %in% valid)} else {T}}
.cmp_pos_vec <- function(x) {if (base::length(x) == 0) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else {base::all(x >= 0 & x <= 1)}}
.cmp_ppn_vec <- function(x) {if (base::length(x) == 0) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else {base::all(x >= 0 & x <= 1)}}
.cmp_psw_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else if (x <= 0) {F} else {x == round(x)}}
.cmp_psw_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else if (base::any(x <= 0)) {F} else {base::all(x == round(x))}}
.cmp_srt_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x) & !base::is.character(x) & !base::is.ordered(x)) {F} else {base::any(base::is.na(x))}}
.cmp_str_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {!base::any(x == "")}}
.cmp_atm_mvc <- function(x) {if (!uj:::.MVC(x)) {F} else if (!base::is.atomic(x)) {F} else if (base::any(base::is.na(x))) {F} else {T}}
.unq_atm_mvc <- function(x) {uj::f0(uj:::.cmp_atm_mvc(x), base::length(x) == base::length(base::unique(x)), F)}
.unq_atm_vec <- function(x) {uj::f0(uj:::.cmp_atm_vec(x), base::length(x) == base::length(base::unique(x)), F)}
.unq_ch1_vec <- function(x) {uj::f0(uj:::.cmp_ch1_vec(x), base::length(x) == base::length(base::unique(x)), F)}
.unq_ch3_vec <- function(x) {uj::f0(uj:::.cmp_ch3_vec(x), base::length(x) == base::length(base::unique(x)), F)}
.unq_chr_vec <- function(x, valid = NULL) {uj::f0(uj:::.cmp_chr_vec(x), base::length(x) == base::length(base::unique(x)), uj::f0(base::is.null(valid), T, base::all(x %in% valid)))}
.unq_nnw_vec <- function(x) {uj::f0(uj:::.cmp_nnw_vec(x), base::length(x) == base::length(base::unique(x)), F)}
.unq_num_vec <- function(x, valid = NULL) {uj::f0(uj:::.cmp_num_vec(x), base::length(x) == base::length(base::unique(x)), uj::f0(base::is.null(valid), T, base::all(x %in% valid)))}
.unq_psw_vec <- function(x) {uj::f0(uj:::.cmp_psw_vec(x), base::length(x) == base::length(base::unique(x)), F)}
.unq_str_vec <- function(x) {uj::f0(uj:::.cmp_str_vec(x), base::length(x) == base::length(base::unique(x)), F)}

# N ###

.n_errs <- function(n = NULL, min = NULL, max = NULL, eq = FALSE, a = FALSE, na = FALSE) {
	errs <- NULL
	if (!base::is.null(n) & (!uj:::.cmp_nnw_vec(n))) {errs <- base::c(errs, "[n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec).")}
	if (!base::is.null(min) & (!uj:::.cmp_nnw_scl(min))) {errs <- base::c(errs, "[min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
	if (!base::is.null(max) & (!uj:::.cmp_nnw_scl(max))) {errs <- base::c(errs, "[max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
	if (!uj::fs_tf(eq)) {errs <- base::c(errs, "[eq] must be TRUE or FALSE.")}
	if (!uj::fs_tf(a)) {errs <- base::c(errs, "[a] must be TRUE or FALSE.")}
	if (!uj::fs_tf(na)) {errs <- base::c(errs, "[na] must be TRUE or FALSE.")}
	errs
}

# delim ####

.delim_errs <- function(args, a) {
	n <- uj::N(args$dots)
	ns <- uj::ns(args$dots)
	two <- "D" %in% base::names(args)
	dots <- n > 0
	pops <- uj::f0(!dots, T, base::all(ns > 0))
	ccsd <- uj:::.cmp_chr_scl(args$d)
	ccsD <- uj::f0(!two, T, uj:::.cmp_chr_scl(args$D))
	atms <- uj::f0(!ccsd, T, base::all(base::sapply(args$dots, uj::atm_vec)))
	recs <- uj::f0(a | !pops, T, uj::recyclable_ns(base::max(ns) / ns))
	errs <- NULL
	if (!dots) {errs <- base::c(errs, "[...] is empty.")}
	if (!pops) {errs <- base::c(errs, "An argument in [...] is of length 0.")}
	if (!ccsd) {errs <- base::c(errs, "[d] must be a complete character scalar (?cmp_chr_scl).")}
	if (!ccsD) {errs <- base::c(errs, "[D] must be a complete character scalar (?cmp_chr_scl).")}
	if (!atms) {errs <- base::c(errs, "Arguments in [...] must be atomic vecs (?pop_vec).")}
	if (!recs) {errs <- base::c(errs, "Arguments in [...] are not of recyclable lengths (?recyclable).")}
	errs
}

# color ####

.color_errs <- function(x = "red", y = "red", lighten = 0, darken = 0, r = 0, g = 0, b = 0, a = 0, h = 0, s = 0, v = 0, wx = 1, wy = 1, comp = T, a.na.ok = F) {
	fun <- uj::caller()
	errs <- NULL
	if (!uj:::.cmp_clr_vec(x) ) {errs <- base::c(errs, "[x] must be contain only valid character color representations.")}
	if (!uj:::.cmp_clr_vec(y) ) {errs <- base::c(errs, "[y] must be contain only valid character color representations.")}
	if (!uj:::.cmp_ppn_vec(lighten)) {errs <- base::c(errs, "[lighten] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(darken)) {errs <- base::c(errs, "[darken] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(b)) {errs <- base::c(errs, "[b] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(g)) {errs <- base::c(errs, "[g] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(h)) {errs <- base::c(errs, "[h] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(r)) {errs <- base::c(errs, "[r] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(s)) {errs <- base::c(errs, "[s] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(v)) {errs <- base::c(errs, "[v] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!uj:::.cmp_ppn_vec(v)) {errs <- base::c(errs, "[v] must be a complete proportion vec (?cmp_ppn_vec).")}
	if (!(a.na.ok & uj:::.NA0(a))) {if (!uj:::.cmp_ppn_vec(a)) {errs <- base::c(errs, "[a] must be a complete proportion vec (?cmp_ppn_vec).")}}
	if (!uj:::.cmp_pos_vec(wx)) {errs <- base::c(errs, "[wx] must be a complete positive numeric vec (?cmp_pos_vec).")}
	if (!uj:::.cmp_pos_vec(wy)) {errs <- base::c(errs, "[wy] must be a complete positive numeric vec (?cmp_pos_vec).")}
	if (!uj:::.cmp_lgl_scl(comp)) {errs <- base::c(errs, "[comp] must be TRUE or FALSE.")}
	if (!base::is.null(errs)) {uj::stopperr(errs, FUN = fun, PKG = "uj")}
}

# crayon ####

.glue_args <- function(d, ...) {base::paste0(uj::av(...), collapse = d)}
.st_bld <- base::c("b", "bo", "bld", "bold", "bolded", "s", "st", "str", "strong")
.st_def <- base::c("d", "def", "default")
.st_itl <- base::c("i", "it", "itl", "ital", "italic", "italics", "italicized", "e", "em", "emp", "emph", "emphasis", "emphasized")
.st_pln <- base::c("p", "pl", "pln", "plain", "r", "re", "res", "reset")
.st_und <- base::c("u", "un", "und", "under", "underline", "underlined")
.xg_blu <- base::c("b", "blu", "blue")
.xg_cyn <- base::c("c", "cyn", "cyan")
.xg_def <- base::c("d", "def", "default")
.xg_grn <- base::c("g", "grn", "green")
.xg_blk <- base::c("k", "blk", "black")
.xg_mag <- base::c("m", "mag", "magenta")
.xg_red <- base::c("r", "red")
.xg_wht <- base::c("w", "wht", "white")
.xg_ylw <- base::c("y", "ylw", "yellow")
.xg_sil <- base::c("s", "gry", "grey", "gray", "sil", "slv", "silver")
.bg_all <- base::sort(base::c(uj:::.xg_blu, uj:::.xg_cyn, uj:::.xg_def, uj:::.xg_grn, uj:::.xg_blk, uj:::.xg_mag, uj:::.xg_red, uj:::.xg_wht, uj:::.xg_ylw))
.fg_all <- base::sort(base::c(uj:::.xg_blu, uj:::.xg_cyn, uj:::.xg_def, uj:::.xg_grn, uj:::.xg_blk, uj:::.xg_mag, uj:::.xg_red, uj:::.xg_wht, uj:::.xg_ylw, uj:::.xg_sil))
.st_all <- base::sort(base::c(uj:::.st_bld, uj:::.st_def, uj:::.st_itl, uj:::.st_pln, uj:::.st_und))

.crayon_errs <- function(st = NULL, bg = NULL, fg = NULL, d = " ", st.null.ok = TRUE, bg.null.ok = TRUE, fg.null.ok = TRUE) {
	fun <- uj::caller()
	st <- uj::failsafe(st)
	bg <- uj::failsafe(bg)
	fg <- uj::failsafe(fg)
	d <- uj::failsafe(d)
	ok.st <- uj::f0(base::is.null(st), st.null.ok, uj:::.unq_chr_vec(st, valid = base::c(uj:::.st_all, base::toupper(uj:::.st_all))))
	ok.bg <- uj::f0(base::is.null(bg), bg.null.ok, uj:::.cmp_chr_scl(bg, valid = base::c(uj:::.bg_all, base::toupper(uj:::.bg_all))))
	ok.fg <- uj::f0(base::is.null(fg), fg.null.ok, uj:::.cmp_chr_scl(fg, valid = base::c(uj:::.fg_all, base::toupper(uj:::.fg_all))))
	ok.d <- uj:::.cmp_chr_scl(d)
	errs <- NULL
	if (!ok.bg) {errs <- base::c(errs, "[bg] must be a character scalar from bg_vals().")}
	if (!ok.fg) {errs <- base::c(errs, "[fg] must be a character scalar from fg_vals().")}
	if (!ok.st) {errs <- base::c(errs, "[st] must be a unique character vec from st_vals().")}
	if (!ok.d) {errs <- base::c(errs, "[d] must be a non-NA character scalar.")}
	if (!base::is.null(errs)) {uj::stopperr(errs, FUN = fun, PKG = "uj")}
}

# dialog ####

.chr <- function(..., d = " ") {base::paste0(uj::av(...), collapse = d)}
.trm <- function(..., d = " ") {base::trimws(uj:::.chr(..., d = d), which = "both")}
.ssplit <- function(x) {uj:::.trm(uj::av(base::strsplit(x, "|", fixed = T)))}

.ok_fmt <- function(x) {
	if (base::is.null(x)) {return(F)}
	x <- uj:::.ssplit(x)
	uj::f0(base::length(x) != 3, F, uj::f0(!(x[1] %in% uj:::.bg_all), F, uj::f0(!(x[2] %in% uj:::.fg_all), F, base::all(x[3] %in% uj:::.st_all))))
}

# declare ####

.labs_ok <- function(x) {
	alpha <- base::c(letters, LETTERS)
	nums <- base::as.character(0:9)
	ok <- base::substr(x, 1, 1) %in% alpha
	ok[!ok] <- base::substr(x[!ok], 1, 1) == "." & base::substr(x[!ok], 2, 2) %in% alpha
	if (base::all(ok)) {
		nch <- base::nchar(x)
		ok <- ok & base::substr(x, nch, nch) %in% base::c(alpha, nums)
		uj::f0(!base::all(ok), F, base::all(uj::av(base::strsplit(x, "", fixed = T)) %in% base::c(alpha, nums, ".", "_")))
	} else {F}
}

# callers ####

.bad_if_not_name <- function(x) {if (base::nchar(x) == 0) {"< bad >"} else if (base::all(x %in% base::c(base::letters, base::LETTERS, 0:9, ".", "_"))) {x} else {"< bad >"}}

.pkg_fun <- function(stack, lim, vec = T, pkg.only = F, fun.only = F) {
	bad <- "< bad >"
	unknown1 <- "..unknown.."
	unknown2 <- "{ unknown }"
	command1 <- "..command.line.."
	command2 <- "{ command line }"
	posP <- base::regexpr("(" , stack, fixed = T) - 1
	pos2 <- base::regexpr("::", stack, fixed = T) - 1
	pos3 <- base::regexpr(":::", stack, fixed = T) - 1
	iP <- posP > 0
	i2 <- pos2 > 0
	i3 <- pos3 > 0
	fun <- pkg <- base::rep.int(unknown1, base::length(stack))
	pkg[i2] <- base::substr(stack[i2], 1, pos2[i2])
	pkg[i3] <- base::substr(stack[i3], 1, pos3[i3])
	fun.start <- base::pmax(1, pos2, pos3)
	fun[iP] <- base::substr(stack[iP], fun.start[iP], posP[iP])
	nch.pkg <- base::nchar(pkg)
	nch.fun <- base::nchar(fun)
	abb.pkg <- lim < nch.pkg
	abb.fun <- lim < nch.fun
	last.pkg <- base::pmin(lim, nch.pkg - 3)
	last.fun <- base::pmin(lim, nch.fun - 3)
	pkg[abb.pkg] <- base::paste0(base::substr(pkg[abb.pkg], 1, last.pkg[abb.pkg]), "...")
	fun[abb.fun] <- base::paste0(base::substr(fun[abb.fun], 1, last.fun[abb.fun]), "...")
	pkg <- base::sapply(pkg, uj:::.bad_if_not_name)
	fun <- base::sapply(fun, uj:::.bad_if_not_name)
	wild <- base::c(bad, unknown1)
	bad.fun <- fun %in% wild
	pkg <- pkg[!bad.fun]
	fun <- fun[!bad.fun]
	i2 <- i2[!bad.fun]
	i3 <- i3[!bad.fun]
	pkg[pkg %in% wild] <- unknown1
	pkg[fun == command1] <- command2
	fun[fun == command1] <- command2
	if (base::length(pkg) == 0) {
		pkg <- fun <- "{ command line }"
		i3 <- F
	} else {pkg[pkg == unknown1] <- unknown2}
	if (!pkg.only & !fun.only) {
		if (vec) {
			sep <- base::rep("::", base::length(pkg))
			sep[i3] <- base::paste0(sep[i3], ":")
			base::paste0(pkg, sep, fun)
		} else {base::list(pkg = pkg, fun = fun)}
	} else if (pkg.only) {pkg}
	else {fun}
}

# meets ####

.meets_errs <- function(x, ...) {
	if (base::...length() == 0) {return(NULL)}
	vars <- base::c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt')
	dots <- base::list(...)
	dot.names  <- base::names(dots)
	pop.names  <- dot.names[dot.names != ""]
	unq.names  <- base::unique(pop.names[pop.names != ""])
	all.unique <- uj::f0(base::is.null(dot.names), T, base::setequal(pop.names, unq.names))
	no.blanks  <- uj::f0(base::is.null(dot.names), T, !base::any(dot.names == ""))
	all.valid  <- uj::f0(base::length(pop.names) == 0, T, base::all(dot.names %in% vars))
	errs <- NULL
	if (!all.valid) {errs <- base::c(errs, "Names of arguments in [...] must be in c('n', 'nr', 'nc', 'min', 'minr', 'minc', 'max', 'maxr', 'maxc', 'vals', 'lt', 'le', 'le', 'ge', 'gt').")}
	if (!no.blanks) {errs <- base::c(errs, "All arguments in [...] must be named.")}
	if (!all.unique) {errs <- base::c(errs, "Arguments in [...] must be named uniquely.")}
	if (!base::is.null(errs)) {return(errs)}

	okVec <- function(dot) {uj::f0(base::is.null(dot), T, uj::f0(!base::is.atomic(dot) | base::length(dot) == 0, F, uj::f0(base::is.vector(dot), T, uj::f0(!base::is.array(dot), F, base::length(base::dim(dot)) < 2))))}
	okScl <- function(dot) {uj::f0(base::is.null(dot), T, uj::f0(!okVec(dot), F, base::length(dot) == 1))}
	okIndVec <- function(dot) {uj::f0(base::is.null(dot), T, uj::f0(!okVec(dot), F, !base::any(base::is.na(dot)) & base::all(dot >= 0 & base::all(dot == base::round(dot)))))}
	okIndScl <- function(dot) {uj::f0(base::is.null(dot), T, uj::f0(!okScl(dot), F, !base::is.na(dot) & dot >= 0 & uj::rounded(dot)))}
	okSrtVec <- function(obj, dot) {
	  obj.wild <- uj::f0(base::is.null(obj), T, uj::f0(!base::is.atomic(obj), F, uj::f0(base::length(obj) == 0, T, base::all(base::is.na(obj)))))
		obj.chr <- base::is.character(obj); dot.chr <- base::is.character(dot)
		obj.num <- base::is.numeric(obj); dot.num <- base::is.numeric(dot)
		obj.lgl <- base::is.logical(obj); dot.lgl <- base::is.logical(dot)
		obj.ord <- base::is.ordered(obj); dot.ord <- base::is.ordered(dot)
		obj.levs <- uj::f0(!obj.ord, NULL, base::levels(obj)); obj.nlevs <- base::length(obj.levs)
		dot.levs <- uj::f0(!dot.ord, NULL, base::levels(dot)); dot.nlevs <- base::length(dot.levs)
		uj::f0(base::is.null(dot), T,
						uj::f0(!okVec(dot), F,
									uj::f0(!dot.num & !dot.lgl & !dot.chr & !dot.ord, F,
													uj::f0(!base::is.atomic(obj), T,
																uj::f0(obj.wild, T,
																				uj::f0(obj.chr & dot.chr, T,
																							uj::f0(obj.num & dot.num, T,
																											uj::f0(obj.lgl & dot.lgl, T,
																														uj::f0(!obj.ord | !dot.ord, F,
																																		uj::f0(obj.nlevs != dot.nlevs, F,
																																					base::all(obj.levs == dot.levs)))))))))))
	}
	okSrtScl <- function(obj, dot) {uj::f0(base::is.null(dot), T, uj::f0(!okSrtVec(obj, dot), F, base::length(dot) == 1))}
	lt <- dots$lt; min  <- dots$min ; max  <- dots$max ; n  <- dots$n
	le <- dots$le; minr <- dots$minr; maxr <- dots$maxr; nr <- dots$nr
	ge <- dots$ge; minc <- dots$minc; maxc <- dots$maxc; nc <- dots$nc
	gt <- dots$gt; vals <- dots$vals
	base::c(
		uj::f0(okIndVec(n), NULL, "[n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
		uj::f0(okIndVec(nr), NULL, "[nr] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
		uj::f0(okIndVec(nc), NULL, "[nc] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec)."),
		uj::f0(okIndScl(min), NULL, "[min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(max), NULL, "[max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(minr), NULL, "[minr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(minc), NULL, "[minc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(maxr), NULL, "[maxr] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okIndScl(maxc), NULL, "[maxc] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
		uj::f0(okSrtScl(x, lt), NULL, "[lt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
		uj::f0(okSrtScl(x, le), NULL, "[le] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
		uj::f0(okSrtScl(x, ge), NULL, "[ge] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
		uj::f0(okSrtScl(x, gt), NULL, "[gt] must be NULL or a non-NA sortable atomic scalar (?cmp_srt_scl) comparable with [x] (?comparable)."),
		uj::f0(okSrtVec(x, vals), NULL, "[vals] must be NULL or a complete atomic vec (?cmp_vec) comparable with [x] (?comparable).")
	)
}

# value_exists ####

.value_errs <- function(name, err = TRUE, gens = 1) {
	fun <- uj::caller()
	errs <- NULL
	if (!uj:::.cmp_chr_scl(name)) {errs <- base::c(errs, "[name] must be a complete character scalar (?cmp_chr_scl).")}
	if (!uj:::.cmp_psw_scl(gens)) {errs <- base::c(errs, "[gens] must be a positive whole number scalar (?cmp_psw_scl).")}
	if (!uj:::.cmp_lgl_scl(err)) {errs <- base::c(errs, "[err] must be TRUE or FALSE.")}
	if (!base::is.null(errs)) {uj::stopperr(errs, FUN = fun, PKG = "uj")}
}

# na ####

.ok_vals <- function(x) {!base::is.na(x)}

# rd ####

.rd_filename <- function(file, type) {
	if      (type == "csv") {prompt <- "comma separated values (.csv) text file"}
	else if (type == "tsv") {prompt <- "tab separated values (.tsv) text file"}
	else if (type == "xsv") {prompt <- "delimited text file"}
	if (base::is.null(file)) {file <- uj::choose_doc(prompt)}
	if (!uj:::.cmp_chr_vec(file)) {uj::stopperr("[file] must be NULL or a complete character vec (?cmp_chr_vec)", FUN = base::paste0("rd_", type), PKG = "uj")}
	file <- base::paste0(file, collapse = "")
	if (!base::file.exists(file)) {uj::stopperr(base::paste0("the specified file ('", file, "') does not exist."), FUN = base::paste0("rd_", type), PKG = "uj")}
	file
}

# pals ####

.pal_errs <- function(n, stack) {
  fun <- uj::caller()
  if (!uj:::.cmp_psw_scl(n)) {uj::stopperr("[n] must be a positive whole-number scalar.", FUN = fun, PKG = "uj", STACK = stack)}
}

.pal_vals <- function(type, n) {
	x <- uj::run("uj::v(", type, ")")
	while (base::length(x) < n) {x <- base::c(x, x)}
	x[1:n]
}

# ipat ####

.pat_errs <- function(x, pat, stack) {
	errs <- NULL
	fun <- uj::caller()
	if (!uj:::.cmp_chr_vec(x)) {base::c(errs, "[x] must be a complete character vec (?cmp_chr_vec).")}
	if (!uj:::.cmp_str_scl(pat)) {base::c(errs, "[pat] must be a complete string scalar (?cmp_str_scl).")}
	uj::stopperr(errs, FUN = fun, PKG = "uj", STACK = stack)
}

# Nth ####

.Nth_errs <- function(x, N, N.scl, stack) {
	ok.N <- uj::f0(N.scl, uj:::.cmp_psw_scl(N), uj:::.cmp_psw_vec(N))
	fun <- uj::caller()
	errs <- NULL
	if (!uj:::.pop_vec(x)) {errs <- base::c(errs, "[x] is not a populated vector (?pop_vec).")}
	if (N.scl & !ok.N) {errs <- base::c(errs, "[N] must be a complete positive whole-number scalar (?cmp_psw_sco).")}
	if (!N.scl & !ok.N) {errs <- base::c(errs, "[N] must be a complete positive whole-number vector (?cmp_psw_vec).")}
	if (!base::is.null(errs)) {uj::stopperr(errs, FUN = fun, PKG = "uj", STACK = stack)}
	if (base::any(N > base::length(x))) {uj::stopperr(base::paste0(uj::f0(N.scl, "", "The largest value in")," [N] is greater than the number of elements in [x]."), FUN = fun, PKG = "uj")}
}

# r ####

.r_errs <- function(fun, stack, ..., r = NULL, e = NULL) {
	errs <- NULL
	if (!base::all(base::sapply(base::list(...), uj:::.atm_vec))) {errs <- base::c(errs, "Arguments in [...] must be atomic vecs (?atm_vec).")}
	if (!base::ifelse(base::is.null(x), T, uj:::.cmp_psw_scl(x))) {errs <- base::c(errs, "[r] must be a positive whole-number scalar (?cmp_psw_scl).")}
	if (!base::ifelse(base::is.null(e), T, uj:::.cmp_psw_scl(e))) {errs <- base::c(errs, "[e] must be a positive whole-number scalar (?cmp_psw_scl).")}
	if (!base::is.null(errs)) {uj::stopperr(errs, FUN = fun, PKG = 'uj', STACK = stack)}
}
