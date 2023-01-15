# internal ####

## PPP ####

.mmms <- base::c("atm", "ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")
.cccs <- base::c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls")
.ssss <- base::c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")
.bbbs <- base::c("atm", "def", "fun", "nil", "nll", "pop", "rcr")
.iiis <- base::c("cmp", "dup", "mss", "nas", "oks", "prt", "unq")
.eees <- base::c("e0D", "e1D", "e2D", "eHD", "eUD")
.ddds <- base::c("d0D", "d1D", "d2D", "dHD")
.ppps <- base::unique(base::sort(base::c(.bbbs, .cccs, .ddds, .eees, .iiis, .mmms, .ssss)))

## BBB ####

.iatm <- function(x) {base::is.atomic(x) & !base::is.null(x)}
.idef <- function(x) {!base::is.null(x)}
.ifun <- function(x) {uj::f0(base::is.function(x), T, uj::f0(base::length(x) != 1 | !base::is.character(x), F, uj::f0(base::is.na(x), F, !uj::isERR(base::match.fun(x)))))}
.inil <- function(x) {base::length(x) == 0}
.inll <- function(x) {base::is.null(x)}
.ipop <- function(x) {base::length(x) > 0}
.ircr <- function(x) {base::is.recursive(x)}

## CCC ####

.iarr <- function(x) {base::is.array(x)}
.idtf <- function(x) {base::is.data.frame(x)}
.igen <- function(x) {base::is.array(x) | base::is.vector(x)}
.imat <- function(x) {base::is.matrix(x)}
.imvc <- function(x) {uj::f0(base::is.array(x) & base::length(base::which(base::dim(x) > 1)) == 1, T, uj::f0(base::length(x) < 2, F, uj::f0(base::is.vector(x), T, base::is.atomic(x) & !base::is.vector(x) & !base::is.array(x))))}
.iscl <- function(x) {base::NROW(x) * base::NCOL(x) == 1}
.ivec <- function(x) {uj::f0(base::length(x) == 0, F, uj::f0(base::is.vector(x), T, uj::f0(base::is.array(x) & base::length(base::which(base::dim(x) > 1)) < 2, T, base::is.atomic(x) & !base::is.vector(x) & !base::is.array(x))))}
.ivls <- function(x) {base::is.list(x) & !base::is.data.frame(x)}

## DDD ####

.id0D <- function(x) {base::is.null(x)}
.id1D <- function(x) {base::is.vector(x)}
.id2D <- function(x) {base::is.matrix(x) | base::is.data.frame(x)}
.idHD <- function(x) {base::length(base::dim(x)) > 2}

## EEE ####

.ie0D <- function(x) {base::NROW(x) * base::NCOL(x) == 1 & base::length(x) == 1}
.ie1D <- function(x) {1 %in% base::c(base::NROW(x), base::NCOL(x)) & base::NROW(x) * base::NCOL(x) > 1}
.ieUD <- function(x) {base::length(x) == 0}
.ie2D <- function(x) {uj::f0(!base::is.array(x) & !base::is.data.frame(x), F, base::length(base::which(base::dim(x) > 1)) == 2)}
.ieHD <- function(x) {uj::f0(!base::is.array(x), F, base::length(base::which(base::dim(x) > 1)) > 2)}

## III ####

.icmp <- function(x) {uj::f0(!base::is.atomic(x) | base::length(x) == 0, F, !base::any(base::is.na(x)))}
.imss <- function(x) {uj::f0(!base::is.atomic(x) | base::length(x) == 0, F, !base::any(!base::is.na(x)))}
.iprt <- function(x) {uj::f0(!base::is.atomic(x) | base::length(x) == 0, F,  base::any(base::is.na(x)) & base::any(!base::is.na(x)))}
.inas <- function(x) {uj::f0(!base::is.atomic(x) | base::length(x) != 1, F,  base::is.na(x))}
.ioks <- function(x) {uj::f0(!base::is.atomic(x) | base::length(x) != 1, F, !base::is.na(x))}
.idup <- function(x) {uj::f0(uj:::.icmp(x), !uj::is_unique(x), F)}
.iunq <- function(x) {uj::f0(uj:::.icmp(x), uj::is_unique(x), F)}

## MMM ####

.wild_mmm <- function(x) {uj::f0(base::length(x) == 0, T, base::all(base::is.na(x)))}
.not_mmm <- function(x) {base::is.null(x) | !base::is.atomic(x)}
.ich1 <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.character(x), F, base::all(base::nchar(x[!base::is.na(x)]) == 1))))}
.ich3 <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.character(x), F, base::all(base::nchar(x[!base::is.na(x)]) == 3))))}
.ichr <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, base::is.character(x)))}
.iclr <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.character(x), F, !base::any(base::c("error", "simpleError") %in% base::class(uj::failsafe(grDevices::col2rgb(x[!base::is.na(x)])))))))}
.ievn <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(base::round(x / 2) == x / 2)})))}
.ifac <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, base::is.factor(x)))}
.ifrc <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::any(base::round(x) != base::round(x))})))}
.ilgl <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, base::is.logical(x)))}
.ineg <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, base::all(x[!base::is.na(x)] < 0))))}
.ingw <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(x < 0 & base::round(x) == x)})))}
.inng <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, !base::any(x[!base::is.na(x)] < 0))))}
.innw <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(x >= 0 & base::round(x) == x)})))}
.inps <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, !base::any(x[!base::is.na(x)] > 0))))}
.inpw <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(x <= 0 & base::round(x) == x)})))}
.inst <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, !(base::is.character(x) | base::is.numeric(x) | base::is.ordered(x))))}
.inum <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, base::is.numeric(x)))}
.iodd <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)] + 1; base::all(base::round(x / 2) == x / 2)})))}
.iord <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, base::is.ordered(x)))}
.ipct <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(x >= 0 & x <= 100)})))}
.ipos <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, base::all(x[!base::is.na(x)] > 0))))}
.ippn <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(x >= 0 & x <= 1)})))}
.ipsw <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(x > 0 & base::round(x) == x)})))}
.isrt <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, base::is.character(x) | base::is.numeric(x) | base::is.ordered(x)))}
.istr <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.character(x), F, !base::any(x[!base::is.na(x)] == ""))))}
.iuno <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, base::is.factor(x) & !base::is.ordered(x)))}
.iwhl <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::f0(!base::is.numeric(x), F, {x <- x[!base::is.na(x)]; base::all(base::round(x) == x)})))}
.iind <- function(x) {uj::f0(uj:::.not_mmm(x), F, uj::f0(uj:::.wild_mmm(x), T, uj::ilgl(x) | uj::ipsw(x)))}

## sss ###

.icol <- function(x) {uj::f0(!(base::is.matrix(x) | base::is.data.frame(x)), F, base::nrow(x) > 1 & base::ncol(x) == 1)}
.iemp <- function(x) {base::length(x) == 0 & !base::is.null(x)}
.ilin <- function(x) {uj:::.ie1D(x)}
.ipnt <- function(x) {uj:::.ie0D(x)}
.irct <- function(x) {uj::f0(!uj:::.ie2D(x), F, base::is.matrix(x) | base::is.data.frame(x))}
.irow <- function(x) {uj::f0(!(base::is.matrix(x) | base::is.data.frame(x)), F, base::nrow(x) == 1 & base::ncol(x) > 1)}
.isld <- function(x) {uj:::.ieUD(x)}
.isqr <- function(x) {base::is.matrix(x) & base::NROW(x) > 1 & base::NCOL(x) > 1 & base::NROW(x) == base::NCOL(x)}

## PPP ####

.spec_vals <- function(x) {if (!base::is.character(x)) {return(NULL)} else {x <- base::unname(base::unlist(base::strsplit(x, "|", fixed = T))); x[x != ""]}}
.drop_iprefix <- function(x) {i <- base::nchar(x) == 4 & base::substr(x, 1, 1) == "i"; x[i] <- base::substr(x[i], 2, 4); x}

# exported ####

#' @encoding UTF-8
#' @family properties
#' @title All purpose property checking
#' @description *Property families defined by this package* \tabular{rl}{
#'       \code{\link{bbb}}   \tab basic properties
#'   \cr \code{\link{ccc}}   \tab xclass (extended class)
#'   \cr \code{\link{ddd}}   \tab defined.d (defined dimensionality)
#'   \cr \code{\link{eee}}   \tab effective.d (effective dimensionality)
#'   \cr \code{\link{iii}}   \tab integrity (completeness, uniqueness)
#'   \cr \code{\link{mmm}}   \tab xmode (extended modes)
#'   \cr \code{\link{sss}}   \tab shape (geometric shape)
#' }
#' \cr *Types of property specs* \tabular{rl}{
#'       *single*   \tab \link[=ich3]{3-char} scalars (i.e., `all(nchar(.) == 3)` with all values in `all_props()`. For example, `'ord', 'dtf', 'd1D',` and `'rct'` are *single* property specs.
#'   \cr  *combo*   \tab Character scalars containing multiple underscore-delimited *single* properties, indicating that those *single* properties must co-occur. For example, the *combo* property spec `'ord_dtf_d1D_rct'` (or an equivalent underscore-delimited permutation) indicates that all four *single* properties must co-occur to satisfy the spec.
#'   \cr *option*   \tab Character scalars containing multiple pipe-delimited *combo* and/or *single* property specs. For example, the *option* property spec `'ord|dtf_d1D|dtf_rct'` would be satisfied by an object with *single* property `'ord'`, *combo* property `'dtf_d1D'`, **or** *combo* property `'dtf_rct'`.
#'   \cr   *flex*   \tab A *single*, *combo*, or *option* property spec as defined above.
#' }
#' \cr *Functions for property spec decomposition* \tabular{rl}{
#'       `props_from_combo`   \tab What are the constituent *single* properties of a *combo* property spec?
#'   \cr `combos_from_spec`   \tab What are the constituent *combo* properties in a *flex* property spec?
#'   \cr  `props_from_spec`   \tab What are the *unique* constituent *single* properties in a *flex* property spec?
#' }
#' \cr *Functions to check whether a value is a valid property spec* \tabular{rl}{
#'      `is_prop_combo`   \tab Is `combo` a valid *combo* property spec?
#'   \cr `is_prop_spec`   \tab Is `spec` a valid *flex* property spec?
#'   \cr  `is_prop_fun`   \tab Is `fun` the name of a dedicated property checking function?
#'   \cr      `is_prop`   \tab Is `prop` a valid *single* property?
#' }
#' \cr *Functions to define properties and property specs* \tabular{rl}{
#'      `combo_concise`   \tab How is a *combo* property spec (concisely) defined?
#'   \cr `spec_concise`   \tab How is an *options* property spec (concisely) defined?
#'   \cr `prop_verbose`   \tab How is a *single* property (verbosely) defined?
#'   \cr    `prop_defs`   \tab What are the definitions of all possible *single* properties (returned as a data.frame)?
#' }
#' \cr *Functions to list names of dedicated property-checking functions* \tabular{rl}{
#'     `ppp_or_funs`   \tab What dedicated functions check `x` for either special values or a match to a *flex* property spec?
#'   \cr `prop_funs`   \tab What dedicated functions check `x` for a match to a specific *single* or *combo* property?
#'   \cr `all_props`   \tab What is the complete set of all possible *single* properties?
#' }
#' \cr *Functions to check object against arbitrary property specs* \tabular{rl}{
#'       `nas_or`   \tab Is `x` either `NA` or a match to the *flex* property spec in `spec`?
#'   \cr `nll_or`   \tab Is `x` either `NULL` or a match to the *flex* property spec in `spec`
#'   \cr   `ippp`   \tab Is `x` a match to the *flex* property spec in `spec`?
#' }
#' \cr *Functions to list all of an object's* single *properties across property families* \tabular{rl}{
#'     `ppp`   \tab What are all of `x`'s *single* properties compiled from all property families?
#' }
#' \cr For convenience, property functions from other function families are described in *details*.
#' @details *Functions to list all* single *properties in a property family* \tabular{rl}{
#'       `bbb_props`   \tab \link[=bbb]{basic}
#'   \cr `ccc_props`   \tab \link[=ccc]{xclass}
#'   \cr `ddd_props`   \tab \link[=ddd]{defined.d}
#'   \cr `eee_props`   \tab \link[=eee]{effective.d}
#'   \cr `iii_props`   \tab \link[=iii]{integrity}
#'   \cr `mmm_props`   \tab \link[=mmm]{xmode}
#'   \cr `sss_props`   \tab \link[=sss]{shape}
#' }
#' \cr *Functions to list names of dedicated property-checking functions* \tabular{rl}{
#'       `cmp_mmm_ccc_funs`   \tab integrity = `'cmp'` + xmode + xclass
#'   \cr `unq_mmm_ccc_funs`   \tab integrity = `'unq'` + xmode + xclass
#' } \tabular{rl}{
#'                          \tab  
#'   \cr   `cmp_ccc_funs`   \tab integrity = `'cmp'` + xclass
#'   \cr   `cmp_mmm_funs`   \tab integrity = `'cmp'` + xmode
#'   \cr   `unq_ccc_funs`   \tab integrity = `'unq'` + xclass
#'   \cr   `unq_mmm_funs`   \tab integrity = `'unq'` + xmode
#' } \tabular{rl}{
#'                          \tab  
#'   \cr   `bbb_ccc_funs`   \tab basic + xclass
#'   \cr   `bbb_mmm_funs`   \tab basic + xmode
#'   \cr   `mmm_ccc_funs`   \tab xmode + xclass
#' } \tabular{rl}{
#'                      \tab  
#'   \cr   `bbb_funs`   \tab \link[=bbb]{basic}
#'   \cr   `ccc_funs`   \tab \link[=ccc]{xclass}
#'   \cr   `ddd_funs`   \tab \link[=ddd]{defined.d}
#'   \cr   `eee_funs`   \tab \link[=eee]{effective.d}
#'   \cr   `iii_funs`   \tab \link[=iii]{integrity}
#'   \cr   `mmm_funs`   \tab \link[=mmm]{xmode}
#'   \cr   `sss_funs`   \tab \link[=sss]{shape}
#' }
#' \cr *Dedicated functions to check an object for a specific* combo *or* single *property*
#' \cr\cr For these functions, an uppercase letter repeated three times is a placeholder for the value of an arbitrary single property from the associated property family.
#' \tabular{rl}{
#'       `cmp_MMM_CCC`   \tab integrity = `'cmp'` + xmode=`'MMM'` + xclass = `'CCC'`
#'   \cr `unq_MMM_CCC`   \tab integrity = `'unq'` + xmode=`'MMM'` + xclass = `'CCC'`
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `cmp_CCC`   \tab integrity = `'cmp'` + xclass = `'CCC'`
#'   \cr   `cmp_MMM`   \tab integrity = `'cmp'` + xmode = `'MMM'`
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `unq_CCC`   \tab integrity = `'unq'` + xclass = `'CCC'`
#'   \cr   `unq_MMM`   \tab integrity = `'unq'` + xmode = `'MMM'`
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `BBB_CCC`   \tab basic = `'BBB'` + xclass = `'CCC'`
#'   \cr   `BBB_MMM`   \tab basic = `'BBB'` + xmode = `'MMM'`
#'   \cr   `MMM_CCC`   \tab xmode = `'MMM'` + xclass = `'CCC'`
#'   \cr   `SSS_CCC`   \tab shape = `'SSS'` + xclass = `'CCC'`
#' } \tabular{rl}{
#'                  \tab  
#'   \cr   `iBBB`   \tab \link[=bbb]{basic} = `'BBB'`
#'   \cr   `iCCC`   \tab \link[=ccc]{xclass} = `'CCC'`
#'   \cr   `iDDD`   \tab \link[=ddd]{defined.d} = `'DDD'`
#'   \cr   `iEEE`   \tab \link[=eee]{effective.d} = `'EEE'`
#'   \cr   `iIII`   \tab \link[=iii]{integrity} = `'III'`
#'   \cr   `iMMM`   \tab \link[=mmm]{xmode} = `'MMM'`
#'   \cr   `iSSS`   \tab \link[=sss]{shape} = `'SSS'`
#' }
#' \cr *Functions to check an object against an arbitrary* combo *property specs*
#' \cr\cr For these functions, an uppercase letter repeated three times is a placeholder for the value of an arbitrary single property from the associated property family.
#' \tabular{rl}{
#'       `cmp_mmm_ccc`   \tab integrity = `'cmp'` + arbitrary xmode + arbitrary xclass
#'   \cr `unq_mmm_ccc`   \tab integrity = `'unq'` + arbitrary xmode + arbitrary xclass
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `bbb_ccc`   \tab arbitrary basic property + arbitrary xclass
#'   \cr   `bbb_mmm`   \tab arbitrary basic property + arbitrary xmode
#'   \cr   `mmm_ccc`   \tab arbitrary xmode + arbitrary xclass
#'   \cr   `sss_ccc`   \tab arbitrary shape + arbitrary xclass
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `cmp_ccc`   \tab integrity = `'cmp'` + arbitrary xclass
#'   \cr   `cmp_mmm`   \tab integrity = `'cmp'` + arbitrary xmode
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `unq_ccc`   \tab integrity = `'unq'` + arbitrary xclass
#'   \cr   `unq_mmm`   \tab integrity = `'unq'` + arbitrary xmode
#' }
#' \cr *Functions to check objects against* flex *property specs in a single family* \tabular{rl}{
#'       `ibbb`   \tab \link[=bbb]{basic}
#'   \cr `iccc`   \tab \link[=ccc]{xclass}
#'   \cr `iddd`   \tab \link[=ddd]{defined.d}
#'   \cr `ieee`   \tab \link[=eee]{effective.d}
#'   \cr `iiii`   \tab \link[=iii]{integrity}
#'   \cr `immm`   \tab \link[=mmm]{xmode}
#'   \cr `isss`   \tab \link[=sss]{shape}
#' }
#' \cr *Functions to retrieve all of an object's* single *properties of a specific family* \tabular{rl}{
#'       `bbb`   \tab \link[=bbb]{basic}
#'   \cr `ccc`   \tab \link[=ccc]{xclass}
#'   \cr `ddd`   \tab \link[=ddd]{defined.d}
#'   \cr `eee`   \tab \link[=eee]{effective.d}
#'   \cr `iii`   \tab \link[=iii]{integrity}
#'   \cr `mmm`   \tab \link[=mmm]{xmode}
#'   \cr `sss`   \tab \link[=sss]{shape}
#' }
#' @param as.dtf A non-`NA` logical scalar indicating whether to return the result as a data.frame with column `1` containing property values and column `2` containing the property families.
#' @param x An R object.
#' @param spec A \link[=cmp_chr_scl]{complete character scalar} containing one or more values from `ppp_vals()` separated by pipes and/or underscores. Combinations of properties can be specified by separating them with underscores. Separating properties or combinations of properties with pipes will result in a value of `TRUE` if any of them applies to `x`.
#' @param valid A \link[=cmp_chr_vec]{complete character vec} containing all properties considered valid.
#' @param print A non-`NA` logical scalar indicating whether to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'  \cr   `combos_from_spec`
#'  \cr   `props_from_combo`
#'  \cr   `props_from_spec`
#'  \cr   `ppp_or_funs`
#'  \cr   `all_props`
#'  \cr   `prop_funs`
#'  \cr   `ppp`
#'  \cr\cr *A character scalar*
#'  \cr   `combo_concise`
#'  \cr   `prop_verbose`
#'  \cr   `spec_concise`
#'  \cr\cr *A logical scalar*
#'  \cr   `is_valid_combo`
#'  \cr   `is_valid_prop`
#'  \cr   `is_valid_spec`
#'  \cr   `is_prop_fun`
#'  \cr   `nas_or`
#'  \cr   `nll_or`
#'  \cr   `ippp`
#' @examples
#' nas_or("5", "ch1")
#' nas_or(NA, "ch1")
#'
#' nll_or(NULL, "ch1")
#' nll_or("1", "ch1")
#' nll_or(7, "ch1")
#'
#' all_props()
#' prop_funs()
#'
#' spec_concise("nll|cmp_psw_vls|ch1_scl")
#' combo_concise("cmp_psw_vls")
#' prop_verbose("srt")
#' prop_verbose("nnw")
#'
#' combos_from_spec("nll|cmp_psw_vls|ch1_scl")
#' props_from_spec("nll|cmp_psw_vls|ch1_scl")
#' props_from_combo("cmp_psw_vls")
#'
#' ippp("7", "nll|cmp_psw_vls|ch1_scl")
#' ippp(NULL, "nll|cmp_psw_vls|ch1_scl")
#' ippp("35", "nll|cmp_psw_vls|ch1_scl")
#'
#' is_prop_combo("letter")
#' is_prop_combo("cmp_psw_vls")
#' is_prop_spec("nll|cmp_psw_vls|ch1_scl")
#'
#' is_prop("vls")
#' is_prop("18")
#'
#' is_prop_fun("cmp_psw_vls")
#' is_prop_fun("18")
#'
#' ppp(letters)
#' prop_defs()
#' @export
ppp <- function(x) {base::sort(base::c(uj::bbb(x), uj::ccc(x), uj::ddd(x), uj::eee(x), uj::iii(x), uj::mmm(x), uj::sss(x)))}

#' @rdname ppp
#' @export
ppp_or_funs <- function() {base::c("nas_or", "nll_or")}

#' @rdname ppp
#' @export
all_props <- function(as.dtf = F) {
  if (!uj::isTF(as.dtf)) {stop(uj::format_errs(pkg = "uj", "[as.dtf] must be TRUE or FALSE."))}
  bval <- uj::bbb_props(); bfam <- base::rep("bbb", base::length(bval)); blab <- base::paste0("b_", bval)
  cval <- uj::ccc_props(); cfam <- base::rep("ccc", base::length(cval)); clab <- base::paste0("c_", cval)
  dval <- uj::ddd_props(); dfam <- base::rep("ddd", base::length(dval)); dlab <- base::paste0("d_", dval)
  eval <- uj::eee_props(); efam <- base::rep("eee", base::length(eval)); elab <- base::paste0("e_", eval)
  ival <- uj::iii_props(); ifam <- base::rep("iii", base::length(ival)); ilab <- base::paste0("i_", ival)
  mval <- uj::mmm_props(); mfam <- base::rep("mmm", base::length(mval)); mlab <- base::paste0("m_", mval)
  sval <- uj::sss_props(); sfam <- base::rep("sss", base::length(sval)); slab <- base::paste0("s_", sval)
  val <- base::c(bval, cval, dval, eval, ival, mval, sval)
  fam <- base::c(bfam, cfam, dfam, efam, ifam, mfam, sfam)
  ord <- base::order(base::c(blab, clab, dlab, elab, ilab, mlab, slab))
  if (!as.dtf) {return(val[ord])}
  tibble::tibble(family = fam[ord], ppp = val[ord])
}

#' @rdname ppp
#' @export
prop_funs <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop(uj::format_errs(pkg = "uj", "[as.dtf] must be TRUE or FALSE."))}
    b_fun <-         uj::bbb_funs();   b_fam <- base::rep(        "bbb", base::length(  b_fun));   b_lab <- base::paste0("1_",   b_fam, "_",   b_fun)
    c_fun <-         uj::ccc_funs();   c_fam <- base::rep(        "ccc", base::length(  c_fun));   c_lab <- base::paste0("1_",   c_fam, "_",   c_fun)
    d_fun <-         uj::ddd_funs();   d_fam <- base::rep(        "ddd", base::length(  d_fun));   d_lab <- base::paste0("1_",   d_fam, "_",   d_fun)
    e_fun <-         uj::eee_funs();   e_fam <- base::rep(        "eee", base::length(  e_fun));   e_lab <- base::paste0("1_",   e_fam, "_",   e_fun)
    i_fun <-         uj::iii_funs();   i_fam <- base::rep(        "iii", base::length(  i_fun));   i_lab <- base::paste0("1_",   i_fam, "_",   i_fun)
    m_fun <-         uj::mmm_funs();   m_fam <- base::rep(        "mmm", base::length(  m_fun));   m_lab <- base::paste0("1_",   m_fam, "_",   m_fun)
    s_fun <-         uj::sss_funs();   s_fam <- base::rep(        "sss", base::length(  s_fun));   s_lab <- base::paste0("1_",   s_fam, "_",   s_fun)
   or_fun <-      uj::ppp_or_funs();  or_fam <- base::rep(     "ppp_or", base::length( or_fun));  or_lab <- base::paste0("2_",  or_fam, "_",  or_fun)
   bc_fun <-     uj::bbb_ccc_funs();  bc_fam <- base::rep(    "bbb_ccc", base::length( bc_fun));  bc_lab <- base::paste0("3_",  bc_fam, "_",  bc_fun)
   bm_fun <-     uj::bbb_mmm_funs();  bm_fam <- base::rep(    "bbb_mmm", base::length( bm_fun));  bm_lab <- base::paste0("3_",  bm_fam, "_",  bm_fun)
   mc_fun <-     uj::mmm_ccc_funs();  mc_fam <- base::rep(    "mmm_ccc", base::length( mc_fun));  mc_lab <- base::paste0("3_",  mc_fam, "_",  mc_fun)
   sc_fun <-     uj::sss_ccc_funs();  sc_fam <- base::rep(    "sss_ccc", base::length( sc_fun));  sc_lab <- base::paste0("3_",  sc_fam, "_",  sc_fun)
   cc_fun <-     uj::cmp_ccc_funs();  cc_fam <- base::rep(    "cmp_ccc", base::length( cc_fun));  cc_lab <- base::paste0("4_",  cc_fam, "_",  cc_fun)
   cm_fun <-     uj::cmp_mmm_funs();  cm_fam <- base::rep(    "cmp_mmm", base::length( cm_fun));  cm_lab <- base::paste0("4_",  cm_fam, "_",  cm_fun)
   uc_fun <-     uj::unq_mmm_funs();  uc_fam <- base::rep(    "unq_ccc", base::length( uc_fun));  uc_lab <- base::paste0("4_",  uc_fam, "_",  uc_fun)
   um_fun <-     uj::unq_mmm_funs();  um_fam <- base::rep(    "unq_mmm", base::length( um_fun));  um_lab <- base::paste0("4_",  um_fam, "_",  um_fun)
  cmc_fun <- uj::cmp_mmm_ccc_funs(); cmc_fam <- base::rep("cmp_mmm_ccc", base::length(cmc_fun)); cmc_lab <- base::paste0("5_", cmc_fam, "_", cmc_fun)
  umc_fun <- uj::unq_mmm_ccc_funs(); umc_fam <- base::rep("unq_mmm_ccc", base::length(umc_fun)); umc_lab <- base::paste0("5_", umc_fam, "_", umc_fun)
  fun <- base::c(b_fun, c_fun, d_fun, e_fun, i_fun, m_fun, s_fun, or_fun, bc_fun, bm_fun, mc_fun, sc_fun, cc_fun, cm_fun, uc_fun, um_fun, cmc_fun, umc_fun)
  fam <- base::c(b_fam, c_fam, d_fam, e_fam, i_fam, m_fam, s_fam, or_fam, bc_fam, bm_fam, mc_fam, sc_fam, cc_fam, cm_fam, uc_fam, um_fam, cmc_fam, umc_fam)
  ord <- base::c(b_lab, c_lab, d_lab, e_lab, i_lab, m_lab, s_lab, or_lab, bc_lab, bm_lab, mc_lab, sc_lab, cc_lab, cm_lab, uc_lab, um_lab, cmc_lab, umc_lab)
  ord <- base::order(ord)
  fun <- fun[ord]
  fam <- fam[ord]
  uj::f0(!as.dtf, base::unique(fun[ord]), base::unique(tibble::tibble(family = fam[ord], fun = fun[ord])))
}

#' @rdname ppp
#' @export
is_prop <- function(prop) {
  if (!uj::cmp_ch3_scl(prop)) {return(FALSE)}
  prop %in% uj::all_props()
}

#' @rdname ppp
#' @export
is_prop_fun <- function(fun) {
  if (!uj::cmp_chr_scl(fun)) {stop(uj::format_errs(pkg = "uj", "[fun] must be a complete character scalar (?cmp_chr_scl)."))}
  fun %in% uj::prop_funs()
}

#' @rdname ppp
#' @export
is_prop_spec <- function(spec) {
  if (!uj::cmp_chr_scl(spec)) {return(FALSE)}
  spec <- uj::av(base::strsplit(spec, "_", TRUE))
  spec <- uj::av(base::strsplit(spec, "|", TRUE))
  spec <- base::trimws(spec)
  spec <- spec[spec != ""]
  spec <- uj:::.drop_iprefix(spec)
  if (base::length(spec) == 0) {return(FALSE)}
  base::all(spec %in% uj::all_props())
}

#' @rdname ppp
#' @export
is_prop_combo <- function(combo) {
  if (!uj::cmp_chr_scl(combo)) {return(FALSE)}
  combo <- uj::av(base::strsplit(combo, "_", TRUE))
  combo <- base::trimws(combo)
  combo <- combo[combo != ""]
  combo <- uj:::.drop_iprefix(combo)
  if (base::length(combo) == 0) {return(FALSE)}
  base::all(combo %in% uj::all_props())
}

#' @rdname ppp
#' @export
props_from_spec <- function(spec, valid = uj::all_props()) {
  ok.valid <- uj::f0(uj::cmp_chr_vec(valid), base::all(valid %in% uj::all_props()), F)
  errs <- base::c(uj::f0(uj::cmp_chr_scl(spec), NULL, "[spec] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(ok.valid             , NULL, "[valid] must be a complete character vector (?cmp_chr_vec) containing only values from ppp_vals()."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  combos <- av(base::strsplit(spec, "|", fixed = T))
  singles <- base::trimws(uj::av(base::strsplit(combos, "_", fixed = T)))
  singles <- singles[singles != ""]
  singles <- uj:::.drop_iprefix(singles)
  if (base::length(singles) == 0)     {stop(uj::format_errs(pkg = "uj", "The property spec [spec] is empty after splitting on pipes and underscores."))}
  if (!base::all(singles %in% valid)) {stop(uj::format_errs(pkg = "uj", "The property spec [spec] contains a property not in all_props()."))}
  base::sort(base::unique(singles))
}

#' @rdname ppp
#' @export
combos_from_spec <- function(spec, valid = uj::all_props()) {
  ok.valid <- uj::f0(uj::cmp_chr_vec(valid), base::all(valid %in% uj::all_props()), F)
  errs <- base::c(uj::f0(uj::cmp_chr_scl(spec), NULL, "[spec] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(ok.valid             , NULL, "[valid] must be a complete character vec (?cpm_chr_vec) containing only values from all_props()."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  ppp.singles <- uj::props_from_spec(spec, valid)
  if (!base::all(ppp.singles %in% valid)) {stop(uj::format_errs(pkg = "uj", p0("[spec] contains a property not in c(", uj::g(uj::p0("'", valid, "'"), g = ", "), ").")))}
  out <- base::trimws(uj::av(base::strsplit(spec, "|", fixed = T)))
  out <- out[out != ""]
  out <- uj:::.drop_iprefix(out)
  base::sort(base::unique(out))
}

#' @rdname ppp
#' @export
props_from_combo <- function(combo, valid = uj::all_props()) {
  ok.valid <- uj::f0(uj::cmp_chr_vec(valid), base::all(valid %in% uj::all_props()), F)
  errs <- base::c(uj::f0(uj::cmp_chr_scl(combo), NULL, "[combo] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(ok.valid              , NULL, "[valid] must be a complete character vec (?cpm_chr_vec) containing only values from all_props()."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (base::length(combo) != base::length(uj::av(base::strsplit(combo, "|", fixed = T)))) {stop(uj::format_errs(pkg = "uj", "[combo] contains multiple alternative property specs."))}
  out <- base::trimws(uj::av(base::strsplit(combo, "_", fixed = T)))
  out <- out[out != ""]
  out <- uj:::.drop_iprefix(out)
  if (base::length(out) == 0    ) {stop(uj::format_errs(pkg = "uj", "[combo] contains no property specs."))}
  if (!base::all(out %in% valid)) {stop(uj::format_errs(pkg = "uj", p0("[combo] contains a property not in c(", uj::g(uj::p0("'", valid, "'"), g = ", "), ").")))}
  base::sort(base::unique(out))
}

#' @rdname ppp
#' @export
ippp <- function(x, spec, ...) {
  if (!uj::is_prop_spec(spec)) {stop(uj::format_errs(pkg = "uj", "[spec] specifies a property not in all_props()."))}
  if (!uj::meets(x, ...)) {return(F)}
  all.props <- uj::all_props()
  combos <- uj::combos_from_spec(spec)
  for (combo in combos) {
    is.one <- combo %in% all.props
    is.fun <- uj::is_prop_fun(combo)
    if (!is.one & !is.fun) {
      singles <- uj:::.drop_iprefix(uj::props_from_combo(combo))
      meets <- TRUE
      for (prop in singles) {if (meets) {meets <- meets & base::eval(base::parse(text = base::paste0("i", prop, "(x)")))}}
    } else if (is.one) {meets <- base::eval(base::parse(text = base::paste0("i", combo, "(x)")))}
    else {meets <- base::eval(base::parse(text = base::paste0(combo, "(x)")))}
    if (meets) {return(TRUE)}
  }
  FALSE
}

#' @rdname ppp
#' @export
nll_or <- function(x, spec, ...) {uj::f0(base::is.null(x), T, uj::ippp(x, spec, ...))}

#' @rdname ppp
#' @export
nas_or <- function(x, spec, ...) {uj::f0(uj::isNAS(x), T, uj::ippp(x, spec, ...))}

#' @rdname ppp
#' @export
prop_defs <- function() {
  tibble::tribble(
    ~family, ~value  , ~short                              , ~long,
    "bbb"  , "atm"   , "atomic"                            , "An atomic object",
    "bbb"  , "def"   , "defined"                           , "A defined object (not NULL)",
    "bbb"  , "fun"   , "function or function name"         , "A function object or a character scalar containing a function name",
    "bbb"  , "nil"   , "nil"                               , "A nil object (of length 0, including NULL)",
    "bbb"  , "nll"   , "NULL"                              , "The NULL object",
    "bbb"  , "pop"   , "populated"                         , "A populated object (not of length 0)",
    "bbb"  , "rcr"   , "recursive"                         , "A recursive object (a data.frame or vlist)",
    "ccc"  , "arr"   , "arrary+"                           , "An array or vector",
    "ccc"  , "dtf"   , "data.frame"                        , "A data.frame",
    "ccc"  , "gen"   , "generic"                           , "A generic (vector/array/vlist)",
    "ccc"  , "mat"   , "matrix"                            , "A atomic matrix",
    "ccc"  , "mvc"   , "multivec"                          , "A vector/vlist list of length 2+ or array of length 2+ with multiple index positions in exactly 1 dimension",
    "ccc"  , "scl"   , "scalar"                            , "A vector/vlist/array of length 1",
    "ccc"  , "vec"   , "vector+"                           , "A vector/1D array/multidimensional array with multiple index positions in 0 or 1 dimension",
    "ccc"  , "vls"   , "vlist (non-data.frame list)"       , "A vector-list (not a data.frame list)",
    "ddd"  , "d0D"   , "defined as 0-dimensional"          , "A 0D object (NULL)",
    "ddd"  , "d1D"   , "defined as 1-dimensional"          , "A 1D object (vector, non-data.frame list, or 1-dimensional array)",
    "ddd"  , "d2D"   , "defined as 2-dimensional"          , "A 2D object (matrix or data.frame)",
    "ddd"  , "dHD"   , "defined as hyper-dimensional"      , "A 3D+ object (array with 3+ dimensions)",
    "eee"  , "e0D"   , "effectively 0-dimensional"         , "An effectively 0D object (vector/vlist/array of length 1 or data.frame with 1 row and 1 column",
    "eee"  , "e1D"   , "effectively 1-dimensional"         , "An effectively 1D object (vector/vlist with 2+ elements, row/column matrix/data.frame, or populated array with more than one index position in exactly 1 dimension)",
    "eee"  , "e2D"   , "effectively 2-dimensional"         , "An effectively 2D object (matrix/data.frame with 2+ rows and 2+ columns or populated array with multiple index positions in exactly 2 dimensions)",
    "eee"  , "eHD"   , "effectively hyper-dimensional"     , "An effectively 3D+ object (populated array with multiple index positions in 3+ dimensions)",
    "eee"  , "eUD"   , "effectively non-dimensional"       , "An effectively non-dimensional object (of length 0)",
    "iii"  , "cmp"   , "atomic and complete"               , "An complete atomic object (containing no NA values)",
    "iii"  , "ddd"   , "atomic, complete, with duplicates" , "A complete dup atomic object (containing no NA values, containing duplicate values)",
    "iii"  , "mss"   , "atomic and missing"                , "A missing atomic object (containing only non-NA values)",
    "iii"  , "nas"   , "atomic NA scalar"                  , "An atomic NA scalar object",
    "iii"  , "oks"   , "atomic non-NA scalar"              , "An atomic non-NA scalar object",
    "iii"  , "prt"   , "atomic and partial"                , "A partial atomic object (containing both NA and non-NA values)",
    "iii"  , "unq"   , "atomic, complete, and unique"      , "A unique, complete atomic object (containing no NA or duplicate values)",
    "mmm"  , "ch1"   , "onechar"                           , "Any non-NA values are character scalars containing exactly 1 character",
    "mmm"  , "ch3"   , "threechar"                         , "Any non-NA values are character scalars containing exactly 3 characters",
    "mmm"  , "chr"   , "character"                         , "A character object",
    "mmm"  , "clr"   , "character color value"             , "A character object containing valid color values",
    "mmm"  , "evn"   , "even whole-number"                 , "An even whole-number object",
    "mmm"  , "fac"   , "factor"                            , "An ordered-factor or unordered-factor object",
    "mmm"  , "frc"   , "fractional numeric"                , "A fractional numeric object (having 1+ non-whole-number values)",
    "mmm"  , "ind"   , "indexing"                          , "A indexing object (logical or positive whole number)",
    "mmm"  , "lgl"   , "logical"                           , "A logical object",
    "mmm"  , "neg"   , "negative numeric"                  , "A negative numeric object",
    "mmm"  , "ngw"   , "negative whole number"             , "A negative whole-number object",
    "mmm"  , "nng"   , "nonnegative numeric"               , "A non-negative numeric object",
    "mmm"  , "nnw"   , "nonnegative whole-number"          , "A non-negative whole-number object",
    "mmm"  , "nps"   , "nonpositive numeric"               , "A non-positive numeric object",
    "mmm"  , "npw"   , "nonpositive whole-number"          , "A non-positive whole-number object",
    "mmm"  , "nst"   , "non-sortable"                      , "A non-sortable object (atomic but not character, logical, numeric, or ordered factor)",
    "mmm"  , "num"   , "numeric"                           , "A numeric object",
    "mmm"  , "odd"   , "odd whole-number"                  , "An odd whole-number object",
    "mmm"  , "ord"   , "ordered-factor"                    , "An ordered-factor object",
    "mmm"  , "pct"   , "percentage numeric"                , "A numeric, percentage object (values in the interval [0, 100])",
    "mmm"  , "pos"   , "positive numeric"                  , "A positive numeric object",
    "mmm"  , "ppn"   , "proportion  numeric"               , "A numeric proportion object (values in the interval [0, 1])",
    "mmm"  , "psw"   , "positive whole-number"             , "A positive whole-number object",
    "mmm"  , "str"   , "string"                            , "A string object (character without any blanks)",
    "mmm"  , "srt"   , "sortable"                          , "A sortable object (character, logical, numeric, or ordered factor)",
    "mmm"  , "uno"   , "unordered-factor"                  , "A unordered-factor object",
    "mmm"  , "whl"   , "whole-number"                      , "A whole-number object",
    "sss"  , "col"   , "column"                            , "A column object (2+ x 1 data.frame/matrix)",
    "sss"  , "emp"   , "empty"                             , "An empty object (of length 0, but not NULL)",
    "sss"  , "lin"   , "linear"                            , "A linear object (vector/vlist/1D array of length 2+, row/column matrix/data.frame, or populated array with multiple indexing positions in exactly 1 dimension)",
    "sss"  , "pnt"   , "point"                             , "A point object (length-1 vector/array/vlist or 1 x 1 data.frame/matrix)",
    "sss"  , "rct"   , "rectangular"                       , "A rectangular object (2+ x 2+ data.frame/matrix)",
    "sss"  , "row"   , "row"                               , "A row object (1 x 2+ data.frame/matrix)",
    "sss"  , "sld"   , "solid"                             , "A solid object (array with multiple index positions in 3+ dimensions)",
    "sss"  , "sqr"   , "square"                            , "A square atomic matrix"
  )
}

#' @rdname ppp
#' @export
prop_verbose <- function(prop, print = TRUE) {
  errs <- base::c(uj::f0(uj::is_prop(prop), NULL, "[prop] must be a character scalar containing a single property spec."),
                  uj::f0(uj::isTF(print)  , NULL, "[print] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (!base::is.null(prop)) {prop <- uj::props_from_spec(prop)}
  if (base::length(prop) > 1) {stop(uj::format_errs(pkg = "uj", "[prop] contains more than 1 property value."))}
  prop <- uj:::.drop_iprefix(prop)
  out <- uj::prop_defs()
  if (uj::idef(prop)) {
    out <- base::paste0("\n family: '", uj::av(out$family[out$value == prop]), "'",
                        "\n value:  '", prop, "'",
                        "\n short:   ", uj::av(out$short[out$value == prop]),
                        "\n long:    ", uj::av(out$long[out$value == prop]), ".\n\n")
  }
  if (!print) {return(out)}
  if (base::is.data.frame(out)) {base::print(out, n = base::nrow(out))} else {base::cat(out)}
}

#' @rdname ppp
#' @export
combo_concise <- function(combo) {
  if (!uj::is_prop_combo(combo)) {stop(uj::format_errs(pkg = "uj", "[combo] does not contain a valid property combo spec."))}
  combo <- uj:::.drop_iprefix(uj::props_from_combo(combo))
  defs <- uj::prop_defs()
  fam <- uj::av(defs$family)
  val <- uj::av(defs$value )
  abb <- uj::av(defs$short )
  bbb <- abb[val %in% combo & fam == "bbb"]
  ccc <- abb[val %in% combo & fam == "ccc"]
  ddd <- abb[val %in% combo & fam == "ddd"]
  eee <- abb[val %in% combo & fam == "eee"]
  iii <- abb[val %in% combo & fam == "iii"]
  mmm <- abb[val %in% combo & fam == "mmm"]
  sss <- abb[val %in% combo & fam == "sss"]
  object <- base::length(ccc) == 0
  out <- base::paste0(base::c(bbb, ccc, ddd, eee, iii, mmm, sss), collapse = ", ")
  out <- uj::av(base::strsplit(out, ", ", TRUE))
  out <- out[!base::duplicated(out)]
  out <- base::paste0(out, collapse = ", ")
  if (object) {out <- base::paste0(out, " object")}
  if (base::substr(out, 1, 1) %in% c("a", "e", "i", "o", "u")) {prefix <- "an "}
  else {prefix <- "a "}
  out <- base::paste0(prefix, out)
  return(out)
}

#' @rdname ppp
#' @export
spec_concise <- function(spec) {
  if (!uj::is_prop_spec(spec)) {stop(uj::format_errs(pkg = "uj", "[spec] is not a valid property spec."))}
  combos <- uj:::.drop_iprefix(uj::combos_from_spec(spec))
  for (i in 1:base::length(combos)) {combos[i] <- uj::combo_concise(combos[i])}
  base::paste0(combos, collapse = " OR ")
}
