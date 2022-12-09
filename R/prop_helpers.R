.iatm <- function(x) {is.atomic(x) & !is.null(x)}
.idef <- function(x) {!is.null(x)}
.ifun <- function(x) {f0(is.function(x), T, f0(length(x) != 1 | !is.character(x), F, f0(is.na(x), F, !isERR(match.fun(x)))))}
.inil <- function(x) {length(x) == 0}
.inll <- function(x) {is.null(x)}
.ipop <- function(x) {length(x) > 0}
.ircr <- function(x) {is.recursive(x)}
.bbbs <- c("atm", "def", "fun", "nil", "nll", "pop", "rcr")

.iarr <- function(x) {is.array(x)}
.idtf <- function(x) {is.data.frame(x)}
.igen <- function(x) {is.array(x) | is.vector(x)}
.imat <- function(x) {is.matrix(x)}
.imvc <- function(x) {f0(length(x) < 2, F, f0(is.vector(x), T, is.array(x) & length(which(dim(x) > 1)) == 1))}
.iscl <- function(x) {f0(length(x) != 1, F, is.array(x) | is.vector(x))}
.ivec <- function(x) {f0(length(x) == 0, F, f0(is.vector(x), T, is.array(x) & length(which(dim(x) > 1)) < 2))}
.ivls <- function(x) {is.list(x) & !is.data.frame(x)}
.cccs <- c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls")

.id0D <- function(x) {is.null(x)}
.id1D <- function(x) {is.vector(x)}
.id2D <- function(x) {is.matrix(x) | is.data.frame(x)}
.idHD <- function(x) {length(dim(x)) > 2}
.ddds <- c("d0D", "d1D", "d2D", "dHD")

.ie0D <- function(x) {NROW(x) * NCOL(x) == 1 & length(x) == 1}
.ie1D <- function(x) {1 %in% c(NROW(x), NCOL(x)) & NROW(x) * NCOL(x) > 1}
.ie2D <- function(x) {f0(!is.array(x) & !is.data.frame(x), F, length(which(dim(x) > 1)) == 2)}
.ieHD <- function(x) {f0(!is.array(x), F, length(which(dim(x) > 1)) > 2)}
.ieUD <- function(x) {length(x) == 0}
.eees <- c("e0D", "e1D", "e2D", "eHD", "eUD")

.icmp <- function(x) {f0(length(x) == 0 | !is.atomic(x), F, !any(is.na(x)))}
.imss <- function(x) {f0(length(x) == 0 | !is.atomic(x), F, all(is.na(x)))}
.inas <- function(x) {f0(length(x) != 1 | !is.atomic(x), F, is.na(x))}
.ioks <- function(x) {f0(length(x) != 1 | !is.atomic(x), F, !is.na(x))}
.iprt <- function(x) {f0(length(x) < 2 | !is.atomic(x), F, {x <- is.na(x); any(x) & !all(x)})}
.iiis <- c("cmp", "mss", "nas", "oks", "prt")

.is_mmm <- function(x) {!is.null(x) | !is.atomic(x)}
.is_MMM <- function(x) {f0(length(x) == 0, T, all(is.na(x)))}
.ich1 <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 1))))}
.ich3 <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 3))))}
.ichr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.character(x)))}
.iclr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, !any(c("error", "simpleError") %in% class(failsafe(col2rgb(x[!is.na(x)])))))))}
.ievn <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x / 2) == x / 2)})))}
.ifac <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.factor(x)))}
.ifrc <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; any(round(x) != round(x))})))}
.iind <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.logical(x) | all()))}
.ilgl <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.logical(x)))}
.ineg <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] < 0))))}
.ingw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x < 0 & round(x) == x)})))}
.inng <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] < 0))))}
.innw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & round(x) == x)})))}
.inps <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] > 0))))}
.inpw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x <= 0 & round(x) == x)})))}
.inst <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, !(is.character(x) | is.numeric(x) | is.ordered(x))))}
.inum <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.numeric(x)))}
.iodd <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)] + 1; all(round(x / 2) == x / 2)})))}
.iord <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.ordered(x)))}
.ipct <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 100)})))}
.ipos <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] > 0))))}
.ippn <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 1)})))}
.ipsw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x > 0 & round(x) == x)})))}
.isrt <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.character(x) | is.numeric(x) | is.ordered(x)))}
.istr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, !any(x[!is.na(x)] == ""))))}
.iuno <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.factor(x) & !is.ordered(x)))}
.iwhl <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x) == x)})))}
.mmms <- c("ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")

.icol <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow(x) > 1 & ncol(x) == 1)}
.iemp <- function(x) {length(x) == 0 & !is.null(x)}
.ilin <- function(x) {.ie1D(x)}
.ipnt <- function(x) {.ie0D(x)}
.irct <- function(x) {f0(!.ie2D(x), F, is.matrix(x) | is.data.frame(x))}
.irow <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow(x) == 1 & ncol(x) > 1)}
.isld <- function(x) {.ieUD(x)}
.isqr <- function(x) {is.matrix(x) & NROW(x) > 1 & NCOL(x) > 1 & NROW(x) == NCOL(x)}
.ssss <- c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")

.ppps <- sort(c(.bbbs, .cccs, .ddds, .eees, .iiis, .mmms, .ssss))
.spec_vals <- function(x) {if (!is.character(x)) {return(NULL)} else {x <- unname(unlist(strsplit(x, "|", fixed = T))); x[x != ""]}}
.drop_iprefix <- function(x) {i <- nchar(x) == 4 & substr(x, 1, 1) == "i"; x[i] <- substr(x[i], 2, 4); x}
