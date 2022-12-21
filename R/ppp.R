# PPP ####

.mmms <- c("atm", "ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")
.cccs <- c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls")
.ssss <- c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")
.bbbs <- c("atm", "def", "fun", "nil", "nll", "pop", "rcr")
.iiis <- c("cmp", "dup", "mss", "nas", "oks", "prt", "unq")
.eees <- c("e0D", "e1D", "e2D", "eHD", "eUD")
.ddds <- c("d0D", "d1D", "d2D", "dHD")
.ppps <- unique(sort(c(.bbbs, .cccs, .ddds, .eees, .iiis, .mmms, .ssss)))

# BBB ####

.iatm <- function(x) {is.atomic(x) & !is.null(x)}
.idef <- function(x) {!is.null(x)}
.ifun <- function(x) {f0(is.function(x), T, f0(length(x) != 1 | !is.character(x), F, f0(is.na(x), F, !isERR(match.fun(x)))))}
.inil <- function(x) {length(x) == 0}
.inll <- function(x) {is.null(x)}
.ipop <- function(x) {length(x) > 0}
.ircr <- function(x) {is.recursive(x)}

# CCC ####

.iarr <- function(x) {is.array(x)}
.idtf <- function(x) {is.data.frame(x)}
.igen <- function(x) {is.array(x) | is.vector(x)}
.imat <- function(x) {is.matrix(x)}
.imvc <- function(x) {f0(is.array(x) & length(which(dim(x) > 1)) == 1, T, f0(length(x) < 2, F, f0(is.vector(x), T, is.atomic(x) & !is.vector(x) & !is.array(x))))}
.iscl <- function(x) {NROW(x) * NCOL(x) == 1}
.ivec <- function(x) {f0(length(x) == 0, F, f0(is.vector(x), T, f0(is.array(x) & length(which(dim(x) > 1)) < 2, T, is.atomic(x) & !is.vector(x) & !is.array(x))))}
.ivls <- function(x) {is.list(x) & !is.data.frame(x)}

# DDD ####

.id0D <- function(x) {is.null(x)}
.id1D <- function(x) {is.vector(x)}
.id2D <- function(x) {is.matrix(x) | is.data.frame(x)}
.idHD <- function(x) {length(dim(x)) > 2}

# EEE ####

.ie0D <- function(x) {NROW(x) * NCOL(x) == 1 & length(x) == 1}
.ie1D <- function(x) {1 %in% c(NROW(x), NCOL(x)) & NROW(x) * NCOL(x) > 1}
.ieUD <- function(x) {length(x) == 0}
.ie2D <- function(x) {f0(!is.array(x) & !is.data.frame(x), F, length(which(dim(x) > 1)) == 2)}
.ieHD <- function(x) {f0(!is.array(x), F, length(which(dim(x) > 1)) > 2)}

# III ####

.icmp <- function(x) {f0(!is.atomic(x) | length(x) == 0, F, !any(is.na(x))                 )}
.imss <- function(x) {f0(!is.atomic(x) | length(x) == 0, F,                 !any(!is.na(x)))}
.iprt <- function(x) {f0(!is.atomic(x) | length(x) == 0, F,  any(is.na(x)) & any(!is.na(x)))}
.inas <- function(x) {f0(!is.atomic(x) | length(x) != 1, F,  is.na(x))}
.ioks <- function(x) {f0(!is.atomic(x) | length(x) != 1, F, !is.na(x))}
.idup <- function(x) {f0(!icmp(x), F, length(x) != length(unique(x)))}
.iunq <- function(x) {f0(!icmp(x), F, length(x) == length(unique(x)))}

# MMM ####

.wild_mmm <- function(x) {f0(length(x) == 0, T, all(is.na(x)))} # wildcard (is of every xmode)
.not_mmm <- function(x) {is.null(x) | !is.atomic(x)}            # is not of a non-empty atomic mode
.ich1 <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 1))))}
.ich3 <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 3))))}
.ichr <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.character(x)))}
.iclr <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.character(x), F, !any(c("error", "simpleError") %in% class(failsafe(col2rgb(x[!is.na(x)])))))))}
.ievn <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x / 2) == x / 2)})))}
.ifac <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.factor(x)))}
.ifrc <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; any(round(x) != round(x))})))}
.iind <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.logical(x) | all()))}
.ilgl <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.logical(x)))}
.ineg <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] < 0))))}
.ingw <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x < 0 & round(x) == x)})))}
.inng <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] < 0))))}
.innw <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & round(x) == x)})))}
.inps <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] > 0))))}
.inpw <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x <= 0 & round(x) == x)})))}
.inst <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, !(is.character(x) | is.numeric(x) | is.ordered(x))))}
.inum <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.numeric(x)))}
.iodd <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)] + 1; all(round(x / 2) == x / 2)})))}
.iord <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.ordered(x)))}
.ipct <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 100)})))}
.ipos <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] > 0))))}
.ippn <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 1)})))}
.ipsw <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x > 0 & round(x) == x)})))}
.isrt <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.character(x) | is.numeric(x) | is.ordered(x)))}
.istr <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.character(x), F, !any(x[!is.na(x)] == ""))))}
.iuno <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, is.factor(x) & !is.ordered(x)))}
.iwhl <- function(x) {f0(.not_mmm(x), F, f0(.wild_mmm(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x) == x)})))}

# sss ###

.icol <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow(x) > 1 & ncol(x) == 1)}
.iemp <- function(x) {length(x) == 0 & !is.null(x)}
.ilin <- function(x) {.ie1D(x)}
.ipnt <- function(x) {.ie0D(x)}
.irct <- function(x) {f0(!.ie2D(x), F, is.matrix(x) | is.data.frame(x))}
.irow <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow(x) == 1 & ncol(x) > 1)}
.isld <- function(x) {.ieUD(x)}
.isqr <- function(x) {is.matrix(x) & NROW(x) > 1 & NCOL(x) > 1 & NROW(x) == NCOL(x)}

# PPP ####

.spec_vals <- function(x) {if (!is.character(x)) {return(NULL)} else {x <- unname(unlist(strsplit(x, "|", fixed = T))); x[x != ""]}}
.drop_iprefix <- function(x) {i <- nchar(x) == 4 & substr(x, 1, 1) == "i"; x[i] <- substr(x[i], 2, 4); x}

#' @encoding UTF-8
#' @family properties
#' @title All purpose property checking
#' @description *Property families defined by this package*
#' \tabular{rl}{
#'       \code{\link{bbb}}   \tab basic properties
#'   \cr \code{\link{ccc}}   \tab xclass (extended class)
#'   \cr \code{\link{ddd}}   \tab defined.d (defined dimensionality)
#'   \cr \code{\link{eee}}   \tab effective.d (effective dimensionality)
#'   \cr \code{\link{iii}}   \tab integrity (completeness, uniqueness)
#'   \cr \code{\link{mmm}}   \tab xmode (extended modes)
#'   \cr \code{\link{sss}}   \tab shape (geometric shape)
#' }
#' *Types of property specs*
#' \tabular{rl}{
#'       *single*   \tab \link[=ich3]{3-char} scalars (i.e., `all(nchar(.) == 3)` with all values in `all_props()`. For example, `'ord', 'dtf', 'd1D',` and `'rct'` are *single* property specs.
#'   \cr            \tab  
#'   \cr  *combo*   \tab Character scalars containing multiple underscore-delimited *single* properties, indicating that those *single* properties must co-occur. For example, the *combo* property spec `'ord_dtf_d1D_rct'` (or an equivalent underscore-delimited permutation) indicates that all four *single* properties must co-occur to satisfy the spec.
#'   \cr            \tab  
#'   \cr *option*   \tab Character scalars containing multiple pipe-delimited *combo* and/or *single* property specs. For example, the *option* property spec `'ord|dtf_d1D|dtf_rct'` would be satisfied by an object with *single* property `'ord'`, *combo* property `'dtf_d1D'`, **or** *combo* property `'dtf_rct'`.
#'   \cr            \tab  
#'   \cr   *flex*   \tab A *single*, *combo*, or *option* property spec as defined above.
#' }
#' *Functions for property spec decomposition*
#' \tabular{rl}{
#'       `props_from_combo`   \tab What are the constituent *single* properties of a *combo* property spec?
#'   \cr                      \tab  
#'   \cr `combos_from_spec`   \tab What are the constituent *combo* properties in a *flex* property spec?
#'   \cr                      \tab  
#'   \cr  `props_from_spec`   \tab What are the *unique* constituent *single* properties in a *flex* property spec?
#' }
#' *Functions to check whether a value is a valid property spec*
#' \tabular{rl}{
#'      `is_prop_combo`   \tab Is `combo` a valid *combo* property spec?
#'   \cr                  \tab  
#'   \cr `is_prop_spec`   \tab Is `spec` a valid *flex* property spec?
#'   \cr                  \tab  
#'   \cr  `is_prop_fun`   \tab Is `fun` the name of a dedicated property checking function?
#'   \cr                  \tab  
#'   \cr      `is_prop`   \tab Is `prop` a valid *single* property?
#' }
#' *Functions to define properties and property specs*
#' \tabular{rl}{
#'      `combo_concise`   \tab How is a *combo* property spec (concisely) defined?
#'   \cr                  \tab  
#'   \cr `spec_concise`   \tab How is an *options* property spec (concisely) defined?
#'   \cr                  \tab  
#'   \cr `prop_verbose`   \tab How is a *single* property (verbosely) defined?
#'   \cr                  \tab  
#'   \cr    `prop_defs`   \tab What are the definitions of all possible *single* properties (returned as a data.frame)?
#' }
#' *Functions to list names of dedicated property-checking functions*
#' \tabular{rl}{
#'     `ppp_or_funs`   \tab What dedicated functions check `x` for either special values or a match to a *flex* property spec?
#'   \cr               \tab  
#'   \cr `prop_funs`   \tab What dedicated functions check `x` for a match to a specific *single* or *combo* property?
#'   \cr               \tab  
#'   \cr `all_props`   \tab What is the complete set of all possible *single* properties?
#' }
#' *Functions to check object against arbitrary property specs*
#' \tabular{rl}{
#'       `nas_or`   \tab Is `x` either `NA` or a match to the *flex* property spec in `spec`?
#'   \cr            \tab  
#'   \cr `nll_or`   \tab Is `x` either `NULL` or a match to the *flex* property spec in `spec`
#'   \cr            \tab  
#'   \cr   `ippp`   \tab Is `x` a match to the *flex* property spec in `spec`?
#' }
#' *Function to list all of an object's* single *properties across property families*
#' \tabular{rl}{  `ppp`   \tab What are all of `x`'s *single* properties compiled from all property families?}
#' For convenience, property functions from other function families are described in *details*.
#' @details *Functions to list all* single *properties in a property family*
#' \tabular{rl}{
#'       `bbb_props`   \tab \link[=bbb]{basic}
#'   \cr `ccc_props`   \tab \link[=ccc]{xclass}
#'   \cr `ddd_props`   \tab \link[=ddd]{defined.d}
#'   \cr `eee_props`   \tab \link[=eee]{effective.d}
#'   \cr `iii_props`   \tab \link[=iii]{integrity}
#'   \cr `mmm_props`   \tab \link[=mmm]{xmode}
#'   \cr `sss_props`   \tab \link[=sss]{shape}
#' }
#' *Functions to list names of dedicated property-checking functions*
#' \tabular{rl}{
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
#' *Dedicated functions to check an object for a specific* combo *or* single *property*
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
#' *Functions to check an object against an arbitrary* combo *property specs*
#' \cr\cr For these functions, an uppercase letter repeated three times is a placeholder for the value of an arbitrary single property from the associated property family.
#' \tabular{rl}{
#'       `cmp_mmm_ccc`   \tab integrity = `'cmp'` + arbitrary xmode + arbitrary xclass
#'   \cr `unq_mmm_ccc`   \tab integrity = `'unq'` + arbitrary xmode + arbitrary xclass
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `bbb_ccc`   \tab arbitrary basic property + arbitrary xclass
#'   \cr   `bbb_mmm`   \tab arbitrary basic property + arbitrary xmode
#'   \cr   `mmm_ccc`   \tab arbitrary xmode + arbitrary xclass
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `cmp_ccc`   \tab integrity = `'cmp'` + arbitrary xclass
#'   \cr   `cmp_mmm`   \tab integrity = `'cmp'` + arbitrary xmode
#' } \tabular{rl}{
#'                     \tab  
#'   \cr   `unq_ccc`   \tab integrity = `'unq'` + arbitrary xclass
#'   \cr   `unq_mmm`   \tab integrity = `'unq'` + arbitrary xmode
#' }
#' *Functions to check objects against* flex *property specs in a single family*
#' \tabular{rl}{
#'       `ibbb`   \tab \link[=bbb]{basic}
#'   \cr `iccc`   \tab \link[=ccc]{xclass}
#'   \cr `iddd`   \tab \link[=ddd]{defined.d}
#'   \cr `ieee`   \tab \link[=eee]{effective.d}
#'   \cr `iiii`   \tab \link[=iii]{integrity}
#'   \cr `immm`   \tab \link[=mmm]{xmode}
#'   \cr `isss`   \tab \link[=sss]{shape}
#' }
#' *Functions to retrieve all of an object's* single *properties of a specific family*
#' \tabular{rl}{
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
#'   \cr    `combos_from_spec`
#'   \cr    `props_from_combo`
#'   \cr    `props_from_spec`
#'   \cr    `ppp_or_funs`
#'   \cr    `all_props`
#'   \cr    `prop_funs`
#'   \cr    `ppp`
#'   \cr\cr *A character scalar*
#'   \cr    `combo_concise`
#'   \cr    `prop_verbose`
#'   \cr    `spec_concise`
#'   \cr\cr *A logical scalar*
#'   \cr    `is_valid_combo`
#'   \cr    `is_valid_prop`
#'   \cr    `is_valid_spec`
#'   \cr    `is_prop_fun`
#'   \cr    `nas_or`
#'   \cr    `nll_or`
#'   \cr    `ippp`
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
ppp <- function(x) {sort(c(bbb(x), ccc(x), ddd(x), eee(x), iii(x), mmm(x), sss(x)))}

# exported ####

#' @rdname ppp
#' @export
ppp_or_funs <- function() {c("nas_or", "nll_or")}

#' @rdname ppp
#' @export
all_props <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop(.errs("[as.dtf] must be TRUE or FALSE."))}
  bval <- bbb_props(); bfam <- rep("bbb", length(bval)); blab <- paste0("b_", bval)
  cval <- ccc_props(); cfam <- rep("ccc", length(cval)); clab <- paste0("c_", cval)
  dval <- ddd_props(); dfam <- rep("ddd", length(dval)); dlab <- paste0("d_", dval)
  eval <- eee_props(); efam <- rep("eee", length(eval)); elab <- paste0("e_", eval)
  ival <- iii_props(); ifam <- rep("iii", length(ival)); ilab <- paste0("i_", ival)
  mval <- mmm_props(); mfam <- rep("mmm", length(mval)); mlab <- paste0("m_", mval)
  sval <- sss_props(); sfam <- rep("sss", length(sval)); slab <- paste0("s_", sval)
  val <-       c(bval, cval, dval, eval, ival, mval, sval)
  fam <-       c(bfam, cfam, dfam, efam, ifam, mfam, sfam)
  ord <- order(c(blab, clab, dlab, elab, ilab, mlab, slab))
  if (!as.dtf) {return(val[ord])}
  tibble::tibble(family = fam[ord], ppp = val[ord])
}

#' @rdname ppp
#' @export
prop_funs <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop(.errs("[as.dtf] must be TRUE or FALSE."))}
    b_fun <-         bbb_funs();   b_fam <- rep(        "bbb", length(  b_fun));   b_lab <- paste0("1_",   b_fam, "_",   b_fun)
    c_fun <-         ccc_funs();   c_fam <- rep(        "ccc", length(  c_fun));   c_lab <- paste0("1_",   c_fam, "_",   c_fun)
    d_fun <-         ddd_funs();   d_fam <- rep(        "ddd", length(  d_fun));   d_lab <- paste0("1_",   d_fam, "_",   d_fun)
    e_fun <-         eee_funs();   e_fam <- rep(        "eee", length(  e_fun));   e_lab <- paste0("1_",   e_fam, "_",   e_fun)
    i_fun <-         iii_funs();   i_fam <- rep(        "iii", length(  i_fun));   i_lab <- paste0("1_",   i_fam, "_",   i_fun)
    m_fun <-         mmm_funs();   m_fam <- rep(        "mmm", length(  m_fun));   m_lab <- paste0("1_",   m_fam, "_",   m_fun)
    s_fun <-         sss_funs();   s_fam <- rep(        "sss", length(  s_fun));   s_lab <- paste0("1_",   s_fam, "_",   s_fun)
   or_fun <-      ppp_or_funs();  or_fam <- rep(     "ppp_or", length( or_fun));  or_lab <- paste0("2_",  or_fam, "_",  or_fun)
   bc_fun <-     bbb_ccc_funs();  bc_fam <- rep(    "bbb_ccc", length( bc_fun));  bc_lab <- paste0("3_",  bc_fam, "_",  bc_fun)
   bm_fun <-     bbb_mmm_funs();  bm_fam <- rep(    "bbb_mmm", length( bm_fun));  bm_lab <- paste0("3_",  bm_fam, "_",  bm_fun)
   mc_fun <-     mmm_ccc_funs();  mc_fam <- rep(    "mmm_ccc", length( mc_fun));  mc_lab <- paste0("3_",  mc_fam, "_",  mc_fun)
   cc_fun <-     cmp_ccc_funs();  cc_fam <- rep(    "cmp_ccc", length( cc_fun));  cc_lab <- paste0("4_",  cc_fam, "_",  cc_fun)
   cm_fun <-     cmp_mmm_funs();  cm_fam <- rep(    "cmp_mmm", length( cm_fun));  cm_lab <- paste0("4_",  cm_fam, "_",  cm_fun)
   uc_fun <-     unq_mmm_funs();  uc_fam <- rep(    "unq_ccc", length( uc_fun));  uc_lab <- paste0("4_",  uc_fam, "_",  uc_fun)
   um_fun <-     unq_mmm_funs();  um_fam <- rep(    "unq_mmm", length( um_fun));  um_lab <- paste0("4_",  um_fam, "_",  um_fun)
  cmc_fun <- cmp_mmm_ccc_funs(); cmc_fam <- rep("cmp_mmm_ccc", length(cmc_fun)); cmc_lab <- paste0("5_", cmc_fam, "_", cmc_fun)
  umc_fun <- unq_mmm_ccc_funs(); umc_fam <- rep("unq_mmm_ccc", length(umc_fun)); umc_lab <- paste0("5_", umc_fam, "_", umc_fun)
  fun <- c(b_fun, c_fun, d_fun, e_fun, i_fun, m_fun, s_fun, or_fun, bc_fun, bm_fun, mc_fun, cc_fun, cm_fun, uc_fun, um_fun, cmc_fun, umc_fun)
  fam <- c(b_fam, c_fam, d_fam, e_fam, i_fam, m_fam, s_fam, or_fam, bc_fam, bm_fam, mc_fam, cc_fam, cm_fam, uc_fam, um_fam, cmc_fam, umc_fam)
  ord <- c(b_lab, c_lab, d_lab, e_lab, i_lab, m_lab, s_lab, or_lab, bc_lab, bm_lab, mc_lab, cc_lab, cm_lab, uc_lab, um_lab, cmc_lab, umc_lab)
  ord <- order(ord)
  fun <- fun[ord]
  fam <- fam[ord]
  if (!as.dtf) {unique(fun[ord])}
  else {unique(tibble::tibble(family = fam[ord], fun = fun[ord]))}
}

#' @rdname ppp
#' @export
is_prop <- function(prop) {
  if (!cmp_ch3_scl(prop)) {return(FALSE)}
  prop %in% all_props()
}

#' @rdname ppp
#' @export
is_prop_fun <- function(fun) {
  if (!cmp_chr_scl(fun)) {stop(.errs("[fun] must be a complete character scalar (?cmp_chr_scl)."))}
  fun %in% prop_funs()
}

#' @rdname ppp
#' @export
is_prop_spec <- function(spec) {
  if (!cmp_chr_scl(spec)) {return(FALSE)}
  spec <- av(strsplit(spec, "_", TRUE))
  spec <- av(strsplit(spec, "|", TRUE))
  spec <- trimws(spec)
  spec <- spec[spec != ""]
  spec <- .drop_iprefix(spec)
  if (length(spec) == 0) {return(FALSE)}
  all(spec %in% all_props())
}

#' @rdname ppp
#' @export
is_prop_combo <- function(combo) {
  if (!cmp_chr_scl(combo)) {return(FALSE)}
  combo <- av(strsplit(combo, "_", TRUE))
  combo <- trimws(combo)
  combo <- combo[combo != ""]
  combo <- .drop_iprefix(combo)
  if (length(combo) == 0) {return(FALSE)}
  all(combo %in% all_props())
}

#' @rdname ppp
#' @export
props_from_spec <- function(spec, valid = all_props()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% all_props()), F)
  errs <- c(f0(cmp_chr_scl(spec), NULL, "[spec] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid         , NULL, "[valid] must be a complete character vector (?cmp_chr_vec) containing only values from ppp_vals()."))
  if (!is.null(errs)) {stop(.errs(errs))}
  combos <- av(strsplit(spec, "|", fixed = T))
  singles <- trimws(av(strsplit(combos, "_", fixed = T)))
  singles <- singles[singles != ""]
  singles <- .drop_iprefix(singles)
  if (length(singles) == 0)     {stop(.errs("The property spec [spec] is empty after splitting on pipes and underscores."))}
  if (!all(singles %in% valid)) {stop(.errs("The property spec [spec] contains a property not in all_props()."))}
  sort(unique(singles))
}

#' @rdname ppp
#' @export
combos_from_spec <- function(spec, valid = all_props()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% all_props()), F)
  errs <- c(f0(cmp_chr_scl(spec), NULL, "[spec] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid         , NULL, "[valid] must be a complete character vec (?cpm_chr_vec) containing only values from all_props()."))
  if (!is.null(errs)) {stop(.errs(errs))}
  ppp.singles <- props_from_spec(spec, valid)
  if (!all(ppp.singles %in% valid)) {stop(.errs(p0("[spec] contains a property not in c(", g(p0("'", valid, "'"), g = ", "), ").")))}
  out <- trimws(av(strsplit(spec, "|", fixed = T)))
  out <- out[out != ""]
  out <- .drop_iprefix(out)
  sort(unique(out))
}

#' @rdname ppp
#' @export
props_from_combo <- function(combo, valid = all_props()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% all_props()), F)
  errs <- c(f0(cmp_chr_scl(combo), NULL, "[combo] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid          , NULL, "[valid] must be a complete character vec (?cpm_chr_vec) containing only values from all_props()."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (length(combo) != length(av(strsplit(combo, "|", fixed = T)))) {stop(.errs("[combo] contains multiple alternative property specs."))}
  out <- trimws(av(strsplit(combo, "_", fixed = T)))
  out <- out[out != ""]
  out <- .drop_iprefix(out)
  if (length(out) == 0    ) {stop(.errs("[combo] contains no property specs."))}
  if (!all(out %in% valid)) {stop(.errs(p0("[combo] contains a property not in c(", g(p0("'", valid, "'"), g = ", "), ").")))}
  sort(unique(out))
}

#' @rdname ppp
#' @export
ippp <- function(x, spec, ...) {
  if (!is_prop_spec(spec)) {stop(.errs("[spec] specifies a property not in all_props()."))}
  if (!meets(x, ...)) {return(F)}
  all.props <- all_props()
  combos <- combos_from_spec(spec)
  for (combo in combos) {
    is.one <- combo %in% all.props
    is.fun <- is_prop_fun(combo)
    if (!is.one & !is.fun) {
      singles <- .drop_iprefix(props_from_combo(combo))
      meets <- TRUE
      for (prop in singles) {if (meets) {meets <- meets & eval(parse(text = paste0("i", prop, "(x)")))}}
    }
    else if (is.one) {meets <- eval(parse(text = paste0("i", combo, "(x)")))}
    else {meets <- eval(parse(text = paste0(combo, "(x)")))}
    if (meets) {return(TRUE)}
  }
  FALSE
}

#' @rdname ppp
#' @export
nll_or <- function(x, spec, ...) {f0(is.null(x), T, ippp(x, spec, ...))}

#' @rdname ppp
#' @export
nas_or <- function(x, spec, ...) {f0(isNAS(x), T, ippp(x, spec, ...))}

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
  errs <- c(f0(is_prop(prop), NULL, "[prop] must be a character scalar containing a single property spec."),
            f0(isTF(print)   , NULL, "[print] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!is.null(prop)) {prop <- props_from_spec(prop)}
  if (length(prop) > 1) {stop(.errs("[prop] contains more than 1 property value."))}
  prop <- .drop_iprefix(prop)
  out <- prop_defs()
  if (idef(prop)) {
    out <- paste0("\n family: '", av(out$family[out$value == prop]), "'",
                  "\n value:  '", prop, "'",
                  "\n short:   ", av(out$short[out$value == prop]),
                  "\n long:    ", av(out$long[out$value == prop]), ".\n\n")
  }
  if (!print) {return(out)}
  if (is.data.frame(out)) {print(out, n = nrow(out))} else {cat(out)}
}

#' @rdname ppp
#' @export
combo_concise <- function(combo) {
  if (!is_prop_combo(combo)) {stop(.errs("[combo] does not contain a valid property combo spec."))}
  combo <- .drop_iprefix(props_from_combo(combo))
  defs <- prop_defs()
  fam <- av(defs$family)
  val <- av(defs$value )
  abb <- av(defs$short )
  bbb <- abb[val %in% combo & fam == "bbb"]
  ccc <- abb[val %in% combo & fam == "ccc"]
  ddd <- abb[val %in% combo & fam == "ddd"]
  eee <- abb[val %in% combo & fam == "eee"]
  iii <- abb[val %in% combo & fam == "iii"]
  mmm <- abb[val %in% combo & fam == "mmm"]
  sss <- abb[val %in% combo & fam == "sss"]
  object <- length(ccc) == 0
  out <- paste0(c(bbb, ccc, ddd, eee, iii, mmm, sss), collapse = ", ")
  out <- av(strsplit(out, ", ", TRUE))
  out <- out[!duplicated(out)]
  out <- paste0(out, collapse = ", ")
  if (object) {out <- paste0(out, " object")}
  if (substr(out, 1, 1) %in% c("a", "e", "i", "o", "u")) {prefix <- "an "}
  else {prefix <- "a "}
  out <- paste0(prefix, out)
  return(out)
}

#' @rdname ppp
#' @export
spec_concise <- function(spec) {
  if (!is_prop_spec(spec)) {stop(.errs("[spec] is not a valid property spec."))}
  combos <- .drop_iprefix(combos_from_spec(spec))
  for (i in 1:length(combos)) {combos[i] <- combo_concise(combos[i])}
  return(paste0(combos, collapse = " OR "))
}
