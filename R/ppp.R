# internal ####

## PPP ####

.mmm <- base::c("atm", "ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")
.ccc <- base::c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls")
.sss <- base::c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")
.bbb <- base::c("atm", "def", "fun", "nil", "nll", "pop", "rcr")
.iii <- base::c("cmp", "dup", "mss", "nas", "oks", "prt", "unq")
.eee <- base::c("e0D", "e1D", "e2D", "eHD", "eUD")
.ddd <- base::c("d0D", "d1D", "d2D", "dHD")
.ppp <- base::unique(base::sort(base::c(uj:::.bbb, uj:::.ccc, uj:::.ddd, uj:::.eee, uj:::.iii, uj:::.mmm, uj:::.sss)))

.MMM <- base::toupper(uj:::.mmm)
.ccc <- base::toupper(uj:::.ccc)
.SSS <- base::toupper(uj:::.sss)
.BBB <- base::toupper(uj:::.bbb)
.III <- base::toupper(uj:::.iii)
.EEE <- base::toupper(uj:::.eee)
.DDD <- base::toupper(uj:::.ddd)
.PPP <- base::unique(base::sort(base::c(uj:::.BBB, uj:::.ccc, uj:::.DDD, uj:::.EEE, uj:::.III, uj:::.MMM, uj:::.SSS)))

## BBB ####

.ATM <- function(x) {uj::isATM(x) & uj::DEF(x)}
.DEF <- function(x) {uj::DEF(x)}
.FUN <- function(x) {uj::f0(uj::isFUN(x), T, uj::f0(uj::notN1(x) | uj::notCHR(x), F, uj::f0(uj::na(x), F, uj::notERR(base::match.fun(x)))))}
.NIL <- function(x) {uj::N0(x)}
.NLL <- function(x) {uj::null(x)}
.POP <- function(x) {uj::N1P(x)}
.RCR <- function(x) {base::is.recursive(x)}

## ccc ####

.ARR <- function(x) {uj::isARR(x)}
.DTF <- function(x) {uj::isDTF(x)}
.GEN <- function(x) {uj::isARR(x) | uj::isVEC(x)}
.AMT <- function(x) {uj::isMAT(x)}
.MVC <- function(x) {uj::f0(uj::isARR(x) & uj::NDIM1(x), T, uj::f0(uj::notN2P(x), F, uj::f0(uj::isVEC(x), T, uj::isATM(x) & uj::notVEC(x) & uj::notARR(x))))}
.SCL <- function(x) {uj::NRC1(x)}
.VEC <- function(x) {uj::f0(uj::N0(x), F, uj::f0(uj::isVEC(x), T, uj::f0(uj::isARR(x) & uj::notNDIM2P(x), T, uj::isATM(x) & !uj::isVEC(x) & uj::notARR(x))))}
.VLS <- function(x) {uj::isLST(x) & uj::notDTF(x)}

## DDD ####

.D0D <- function(x) {uj::null(x)}
.D1D <- function(x) {uj::isVEC(x)}
.D2D <- function(x) {uj::isMAT(x) | uj::isDTF(x)}
.DHD <- function(x) {uj::N3P(base::dim(x))}

## EEE ####

.E0D <- function(x) {uj::NRC1(x) & uj::N1(x)}
.E1D <- function(x) {uj::isIN1(1, uj::NR(x), uj::NC(x)) & uj::NRC2P(x)}
.EUD <- function(x) {uj::N0(x)}
.E2D <- function(x) {uj::f0(uj::notARR(x) & uj::notDTF(x), F, uj::N2(base::which(base::dim(x) > 1)))}
.EHD <- function(x) {uj::f0(uj::notARR(x), F, uj::N3P(base::which(base::dim(x) > 1)))}

## III ####

.CMP <- function(x) {uj::f0(uj::notATM(x) | uj::N0(x), F, uj::noneNAS(x))}
.MSS <- function(x) {uj::f0(uj::notATM(x) | uj::N0(x), F, uj::noneNAS(x))}
.PRT <- function(x) {uj::f0(uj::notATM(x) | uj::N0(x), F, uj::anyNAS(x) & uj::anyOKS(x))}
.NAS <- function(x) {uj::f0(uj::notATM(x) | uj::notN1(x), F, uj::is.na(x))}
.OKS <- function(x) {uj::f0(uj::notATM(x) | uj::notN1(x), F, !uj::is.na(x))}
.DUP <- function(x) {uj::f0(uj:::.CMP(x), uj::notUNQ(x), F)}
.UNQ <- function(x) {uj::f0(uj:::.CMP(x), uj::UNQ(x), F)}

## MMM ####

.wildMMM <- function(x) {uj::f0(uj::N0(0), T, uj::allNA(x))}
.notMMM <- function(x) {uj::null(x) | uj::notATM(x)}
.CH1 <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notCHR(x), F, base::all(uj::LEN(x[uj::ok(x)]) == 1))))}
.CH3 <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notCHR(x), F, base::all(uj::LEN(x[uj::ok(x)]) == 3))))}
.CHR <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isCHR(x)))}
.CLR <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notCHR(x), F, uj::noneIN(base::c("error", "simpleError"), base::class(uj::failsafe(grDevices::col2rgb(x[!uj::na(x)])))))))}
.EVN <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(uj::rounded(x[uj::ok(x)] / 2)))))}
.FAC <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isFAC(x)))}
.FRC <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::any(!uj::rounded(x[uj::ok(x)])))))}
.LGL <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isLGL(x)))}
.NEG <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] < 0))))}
.NGW <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] < 0 & uj::rounded(x[uj::ok(x)])))))}
.NNG <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, !base::any(x[uj::ok(x)] < 0))))}
.NNW <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] >= 0 & uj::rounded(x[uj::ok(x)])))))}
.NPS <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, !base::any(x[uj::ok(x)] > 0))))}
.NPW <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] <= 0 & uj::rounded(x[uj::ok(x)])))))}
.NST <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::notCHR(x) & uj::notNUM(x) | uj::notORD(x)))}
.NUM <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isNUM(x)))}
.ODD <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(uj::rounded((x[uj::ok(x)] + 1) / 2)))))}
.ORD <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isORD(x)))}
.PCT <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] >= 0 & x[uj::ok(x)] <= 100))))}
.POS <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] > 0))))}
.PPN <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] >= 0 & x[uj::ok(x)] <= 1))))}
.PSW <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(x[uj::ok(x)] > 0 & uj::rounded(x)))))}
.SRT <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isCHR(x) | uj::isNUM(x) | uj::isORD(x)))}
.STR <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notCHR(x), F, uj::noneBL(x[uj::ok(x)]))))}
.UNO <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isFAC(x) & uj::notORD(x)))}
.WHL <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::f0(uj::notNUM(x), F, base::all(uj::rounded(x[uj::ok(x)])))))}
.IND <- function(x) {uj::f0(uj:::.notMMM(x), F, uj::f0(uj:::.wildMMM(x), T, uj::isLGL(x) | uj:::.PSW(x)))}

## sss ###

.COL <- function(x) {uj::f0(uj::notMAT(x) & uj::notDTF(x), F, uj::NC1(x) & uj::NR2P(x))}
.EMP <- function(x) {uj::N0(x) & uj::DEF(x)}
.LIN <- function(x) {uj:::.E1D(x)}
.PNT <- function(x) {uj:::.E0D(x)}
.RCT <- function(x) {uj::f0(!uj:::.E2D(x), F, uj::isMAT(x) | uj::isDTF(x))}
.ROW <- function(x) {uj::f0(uj::notMAT(x) & uj::notDTF(x), F, uj::NR1(x) & uj::NC2P(x))}
.SLD <- function(x) {uj:::.EUD(x)}
.SQR <- function(x) {uj::isMAT(x) & uj::NR2P(x) & uj::NC2P(x) & uj::NR(x) == uj::NC(x)}

## PPP ####

.spec2combos <- function(x) {
  if (uj::isCHR(x)) {
    x <- uj::av(base::strsplit(x, "|", fixed = T))
    x <- x[x != ""]
    if (uj::N1P(x)) {base::tolower(base::unique(x))} else {NULL}
  } else {NULL}
}

.spec2props <- function(x) {
  if (uj::isCHR(x)) {
    x <- uj::av(base::strsplit(x, "|", fixed = T))
    x <- uj::av(base::strsplit(x, "_", fixed = T))
    x <- x[x != ""]
    if (uj::N1P(x)) {
      if (uj::cmp_ch3(x)) {base::tolower(base::unique(x))} else {NULL}
    } else {NULL}
  } else {NULL}
}

.combo2props <- function(x) {
  if (uj::isCHR(x)) {
    if (uj::N1(.spec_to_combos)) {
      x <- uj::av(base::strsplit(x, "_", fixed = T))
      x <- x[x != ""]
      if (uj::N1P(x)) {
        if (uj::cmp_ch3(x)) {base::tolower(base::unique(x))} else {NULL}
      } else {NULL}
    } else {NULL}
  } else {NULL}
}

# exported ####

#' @encoding UTF-8
#' @family properties
#' @title All purpose property checking
#' @description This family of functions brings together a variety of families defined in this and gives users the ability to manage object properties in a granular and concise manner.
#' \cr\cr **Property families defined by this package**
#' \tabular{ll}{  \code{\link{bbb}}     \tab basic properties                       \cr
#'                \code{\link{ccc}}     \tab xclass (extended class)                \cr
#'                \code{\link{ddd}}     \tab defined.D (defined dimensionality)     \cr
#'                \code{\link{eee}}     \tab effective.D (effective dimensionality) \cr
#'                \code{\link{iii}}     \tab integrity (completeness, uniqueness)   \cr
#'                \code{\link{mmm}}     \tab xmode (extended modes)                 \cr
#'                \code{\link{sss}}     \tab shape (geometric shape)                  }
#' \cr\cr This family uses concise and flexible *property specs* as defined in the following table.
#' \tabular{ll}{  *single*              \tab \link[=CH3]{3-char} scalars (i.e., `all(nchar(.) == 3)` with all values in `all_props()`. For example,
#'                                           `'ord', 'dtf', 'd1D',` and `'rct'` are *single* property specs.                                           \cr   \tab   \cr
#'                *option*              \tab Character scalars containing multiple pipe-delimited *combo* and/or *single* property specs. For example,
#'                                           the *option* property spec `'ord|dtfD1D|dtfRCT'` would be satisfied by an object with *single* property
#'                                           'ord'`, *combo* property `'dtfD1D'`, **or** *combo* property `'dtfRCT'`.                                  \cr   \tab  }
#' \tabular{ll}{  *combo*               \tab Character scalars containing multiple underscore-delimited *single* properties, indicating that those
#'                                           *single* properties must co-occur. For example, the *combo* property spec `'ordDTFd1dRCT'` (or an
#'                                           equivalent underscore-delimited permutation) indicates that all four *single* properties must co-occur
#'                                           to satisfy the spec.                                                                                      \cr   \tab  }
#' \tabular{ll}{  *flex*                \tab A *single*, *combo*, or *option* property spec as defined above.                                          }
#' @details **Functions for property spec decomposition**
#' \tabular{ll}{  `props_from_combo`    \tab What are the constituent *single* properties of a *combo* property spec?         \cr   \tab   \cr
#'                `combo_from_spec`     \tab What are the constituent *combo* properties in a *flex* property spec?           \cr   \tab  }
#' \tabular{ll}{  `props_from_spec`     \tab What are the *unique* constituent *single* properties in a *flex* property spec? }
#' \cr\cr **Functions to check whether a value is a valid property spec**
#' \tabular{ll}{  `is_prop_combo`       \tab Is `combo` a valid *combo* property spec?                    \cr   \tab     }
#' \tabular{ll}{  `is_prop_spec`        \tab Is `spec` a valid *flex* property spec?                      \cr   \tab     }
#' \tabular{ll}{  `is_prop_fun`         \tab Is `fun` the name of a dedicated property checking function? \cr   \tab     }
#' \tabular{ll}{  `is_prop`             \tab Is `prop` a valid *single* property?                         }
#' \cr\cr **Functions to define properties and property specs**
#' \tabular{ll}{  `combo_concise`       \tab How is a *combo* property spec (concisely) defined?                                      \cr   \tab     }
#' \tabular{ll}{  `spec_concise`        \tab How is an *options* property spec (concisely) defined?                                   \cr   \tab   \cr
#'                `prop_verbose`        \tab How is a *single* property (verbosely) defined?                                          \cr   \tab     }
#' \tabular{ll}{  `prop_defs`           \tab What are the definitions of all possible *single* properties (returned as a data.frame)? }
#' \cr\cr **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `ppp_or_funs`         \tab What dedicated functions check `x` for either special values or a match to a *flex* property spec? \cr   \tab     }
#' \tabular{ll}{  `prop_funs`           \tab What dedicated functions check `x` for a match to a specific *single* or *combo* property?         \cr   \tab   \cr
#'                `all_props`           \tab What is the complete set of all possible *single* properties?                                      }
#' \cr\cr **Functions to check object against arbitrary property specs**
#' \tabular{ll}{  `nas_or`              \tab Is `x` either `NA` or a match to the *flex* property spec in `spec`?  \cr   \tab   \cr
#'                `nll_or`              \tab Is `x` either `NULL` or a match to the *flex* property spec in `spec` \cr   \tab  }
#' \tabular{ll}{  `ppp`                 \tab Is `x` a match to the *flex* property spec in `spec`?                 }
#' \cr\cr **Function to list all of an object's** single **properties across property families**
#' \tabular{ll}{  `ppp`                 \tab What are all of `x`'s *single* properties compiled from all property families?}
#' \cr\cr For convenience, property functions from other function families are also described below.
#' \cr\cr **Functions to list all** single **properties in a property family**
#' \tabular{ll}{  `bbb_props`           \tab \link[=bbb]{basic}       \cr
#'                `ccc_props`           \tab \link[=ccc]{xclass}      \cr
#'                `ddd_props`           \tab \link[=ddd]{defined.D}   \cr
#'                `eee_props`           \tab \link[=eee]{effective.D} \cr
#'                `iii_props`           \tab \link[=iii]{integrity}   \cr
#'                `mmm_props`           \tab \link[=mmm]{xmode}       \cr
#'                `sss_props`           \tab \link[=sss]{shape}         }
#' \cr\cr **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `cmp_mmm_ccc_funs`    \tab integrity = `'cmp'` + xmode + xclass \cr
#'                `unq_mmm_ccc_funs`    \tab integrity = `'unq'` + xmode + xclass \cr   \tab  }
#' \tabular{ll}{  `cmp_ccc_funs`        \tab integrity = `'cmp'` + xclass         \cr
#'                `cmp_mmm_funs`        \tab integrity = `'cmp'` + xmode          \cr   \tab   \cr
#'                `unq_ccc_funs`        \tab integrity = `'unq'` + xclass         \cr
#'                `unq_mmm_funs`        \tab integrity = `'unq'` + xmode          \cr   \tab   \cr
#'                `bbb_ccc_funs`        \tab basic + xclass                       \cr
#'                `bbb_mmm_funs`        \tab basic + xmode                        \cr   \tab   \cr
#'                `mmm_ccc_funs`        \tab xmode + xclass                       \cr   \tab  }
#' \tabular{ll}{  `bbb_funs`            \tab \link[=bbb]{basic}       \cr
#'                `ccc_funs`            \tab \link[=ccc]{xclass}      \cr
#'                `ddd_funs`            \tab \link[=ddd]{defined.D}   \cr
#'                `eee_funs`            \tab \link[=eee]{effective.D} \cr
#'                `iii_funs`            \tab \link[=iii]{integrity}   \cr
#'                `mmm_funs`            \tab \link[=mmm]{xmode}       \cr
#'                `sss_funs`            \tab \link[=sss]{shape}       }
#' \cr\cr **Dedicated functions to check an object for a specific** combo **or** single **property**
#' \cr\cr For these functions, `{bbb}`, `{ccc}`, `{ddd}`, `{eee}`, `{iii}`, `{mmm}`, and `{sss}` are placeholders for any given basic, xclass, defined.D, effective.D, integrity, xmode, and shape properties, respectively.
#' \tabular{ll}{  `{bbb}`               \tab \link[=bbb]{basic} = `'{bbb}'`.       \cr
#'                `{ccc}`               \tab \link[=ccc]{xclass} = `'{ccc}'`.      \cr
#'                `{ddd}`               \tab \link[=ddd]{defined.D} = `'{ddd}'`.   \cr
#'                `{eee}`               \tab \link[=eee]{effective.D} = `'{eee}'`. \cr
#'                `{iii}`               \tab \link[=iii]{integrity} = `'{iii}'`.   \cr
#'                `{mmm}`               \tab \link[=mmm]{xmode} = `'{mmm}'`.       \cr
#'                `{sss}`               \tab \link[=sss]{shape} = `'{sss}'`.       \cr   \tab  }
#' \tabular{ll}{  `{bbb}_{ccc}`         \tab basic = `'{bbb}'` + xclass = `'{ccc}'`                     \cr
#'                `{bbb}_{mmm}`         \tab basic = `'{bbb}'` + xmode = `'{mmm}'`                      \cr   \tab   \cr
#'                `{mmm}_{ccc}`         \tab xmode = `'{mmm}'` + xclass = `'{ccc}'`                     \cr   \tab   \cr
#'                `{sss}_{ccc}`         \tab shape = `'{sss}'` + xclass = `'{ccc}'`                     \cr   \tab   \cr
#'                `cmp_{ccc}`           \tab integrity = `'cmp'` + xclass = `'{ccc}'`                   \cr
#'                `cmp_{mmm}`           \tab integrity = `'cmp'` + xmode = `'{mmm}'`                    \cr   \tab   \cr
#'                `unq_{ccc}`           \tab integrity = `'unq'` + xclass = `'{ccc}'`                   \cr
#'                `unq_{mmm}`           \tab integrity = `'unq'` + xmode = `'{mmm}'`                    \cr   \tab  }
#' \tabular{ll}{  `cmp_{mmm}_{ccc}`     \tab integrity = `'cmp'` + xmode=`'{mmm}'` + xclass = `'{ccc}'` \cr
#'                `unq_{mmm}_{ccc}`     \tab integrity = `'unq'` + xmode=`'{mmm}'` + xclass = `'{ccc}'` }
#' \cr\cr **Functions to check an object against an arbitrary* combo *property specs**
#' \cr\cr For these functions, an uppercase letter repeated three times is a placeholder for the value of an arbitrary single property from the associated property family.
#' \tabular{ll}{  `bbb_ccc`             \tab basic property in arg `bbb` + xclass property in arg `ccc` \cr   \tab   \cr
#'                `bbb_mmm`             \tab basic property in arg `bbb` + xmode property in arg `mmm`  \cr
#'                `mmm_ccc`             \tab xmode property in arg `mmm` + xclass property in arg `ccc` \cr   \tab   \cr
#'                `sss_ccc`             \tab shape property in arg `sss` + xclass property in arg `ccc` \cr   \tab   \cr
#'                `cmp_ccc`             \tab integrity = `'cmp'` + xclass property in arg `ccc`         \cr
#'                `cmp_mmm`             \tab integrity = `'cmp'` + xmode property in arg `mmm`          \cr   \tab   \cr
#'                `unq_ccc`             \tab integrity = `'unq'` + xclass property in arg `ccc`         \cr
#'                `unq_mmm`             \tab integrity = `'unq'` + xmode property in arg `mmm`          \cr   \tab  }
#' \tabular{ll}{  `cmp_mmm_ccc`         \tab integrity = `'cmp'` + xmode property in arg `mmm` + xclass property in arg `ccc` \cr   \tab   \cr
#'                `unq_mmm_ccc`         \tab integrity = `'unq'` + xmode property in arg `mmm` + xclass property in arg `ccc` }
#' \cr\cr **Functions to check objects against* flex *property specs in a single family**
#' \tabular{ll}{  `BBB`                 \tab \link[=bbb]{basic}       \cr
#'                `CCC`                 \tab \link[=ccc]{xclass}      \cr
#'                `DDD`                 \tab \link[=ddd]{defined.D}   \cr
#'                `EEE`                 \tab \link[=eee]{effective.D} \cr
#'                `III`                 \tab \link[=iii]{integrity}   \cr
#'                `MMM`                 \tab \link[=mmm]{xmode}       \cr
#'                `SSS`                 \tab \link[=sss]{shape}       }
#' \cr\cr **Functions to retrieve all of an object's* single *properties of a specific family**
#' \tabular{ll}{  `bbb`                 \tab \link[=bbb]{basic}       \cr
#'                `ccc`                 \tab \link[=ccc]{xclass}      \cr
#'                `ddd`                 \tab \link[=ddd]{defined.D}   \cr
#'                `eee`                 \tab \link[=eee]{effective.D} \cr
#'                `iii`                 \tab \link[=iii]{integrity}   \cr
#'                `mmm`                 \tab \link[=mmm]{xmode}       \cr
#'                `sss`                 \tab \link[=sss]{shape}       }
#' @param as.dtf `TRUE` or `FALSE` indicating whether to return the result as a data.frame with column `1` containing property values and column `2` containing the property families.
#' @param x An R object.
#' @param spec A \link[=cmp_chr_scl]{complete character scalar} containing one or more values from `ppp_vals()` separated by pipes and/or underscores. Combinations of properties can be specified by separating them with underscores. Separating properties or combinations of properties with pipes will result in a value of `TRUE` if any of them applies to `x`.
#' @param valid A \link[=cmp_chr_vec]{complete character vec} containing all properties considered valid.
#' @param print `TRUE` or `FALSE` indicating whether to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character scalar** \cr `combo_concise`    \cr `prop_verbose`     \cr `spec_concise`
#' \cr\cr  **A character vector** \cr `combos_from_spec` \cr `props_from_combo` \cr `props_from_specs` \cr `ppp_or_funs` \cr `all_props` \cr `prop_funs` \cr `ppp`
#' \cr\cr  **A logical scalar**   \cr `is_prop_combo`    \cr `is_prop`          \cr `is_prop_spec`     \cr `is_prop_fun` \cr `nas_or`    \cr `nll_or`    \cr `PPP`
#' @examples
#' nasOR("5", "ch1")
#' nasOR(NA, "ch1")
#'
#' nllOR(NULL, "ch1")
#' nllOR("1", "ch1")
#' nllOR(7, "ch1")
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
#' PPP("7", "nll|cmp_psw_vls|ch1_scl")
#' PPP(NULL, "nll|cmp_psw_vls|ch1_scl")
#' PPP("35", "nll|cmp_psw_vls|ch1_scl")
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
#' propDEFS()
#' @export
ppp <- function(x) {base::sort(base::c(uj::bbb(x), uj::ccc(x), uj::ddd(x), uj::eee(x), uj::iii(x), uj::mmm(x), uj::sss(x)))}

#' @rdname ppp
#' @export
ppp_or_funs <- function() {base::c("nasOR", "nllOR")}

#' @rdname ppp
#' @export
all_props <- function(as.dtf = F) {
  uj::err_if_not(uj::isTF1(as.dtf), "[as.dtf] must be TRUE or FALSE.", PKG = "uj")
  bval <- uj::bbb_props(); bfam <- base::rep("bbb", uj::N(bval)); blab <- uj::p0("b_", bval)
  cval <- uj::ccc_props(); cfam <- base::rep("ccc", uj::N(cval)); clab <- uj::p0("c_", cval)
  dval <- uj::ddd_props(); dfam <- base::rep("ddd", uj::N(dval)); dlab <- uj::p0("d_", dval)
  eval <- uj::eee_props(); efam <- base::rep("eee", uj::N(eval)); elab <- uj::p0("e_", eval)
  ival <- uj::iii_props(); ifam <- base::rep("iii", uj::N(ival)); ilab <- uj::p0("i_", ival)
  mval <- uj::mmm_props(); mfam <- base::rep("mmm", uj::N(mval)); mlab <- uj::p0("m_", mval)
  sval <- uj::sss_props(); sfam <- base::rep("sss", uj::N(sval)); slab <- uj::p0("s_", sval)
  val <- base::c(bval, cval, dval, eval, ival, mval, sval)
  fam <- base::c(bfam, cfam, dfam, efam, ifam, mfam, sfam)
  ord <- base::order(base::c(blab, clab, dlab, elab, ilab, mlab, slab))
  if (!as.dtf) {val[ord]} else {uj::tb(family = fam[ord], ppp = val[ord])}
}

#' @rdname ppp
#' @export
prop_funs <- function(as.dtf = F) {
  uj::err_if_not(uj::isTF1(as.dtf), "[as.dtf] must be TRUE or FALSE.", PKG = "uj")
    b_fun <-         uj::bbb_funs();   b_fam <- base::rep(        "bbb", uj::N(  b_fun));   b_lab <- uj::p0("1_",   b_fam, "_",   b_fun)
    c_fun <-         uj::ccc_funs();   c_fam <- base::rep(        "ccc", uj::N(  c_fun));   c_lab <- uj::p0("1_",   c_fam, "_",   c_fun)
    d_fun <-         uj::ddd_funs();   d_fam <- base::rep(        "ddd", uj::N(  d_fun));   d_lab <- uj::p0("1_",   d_fam, "_",   d_fun)
    e_fun <-         uj::eee_funs();   e_fam <- base::rep(        "eee", uj::N(  e_fun));   e_lab <- uj::p0("1_",   e_fam, "_",   e_fun)
    i_fun <-         uj::iii_funs();   i_fam <- base::rep(        "iii", uj::N(  i_fun));   i_lab <- uj::p0("1_",   i_fam, "_",   i_fun)
    m_fun <-         uj::mmm_funs();   m_fam <- base::rep(        "mmm", uj::N(  m_fun));   m_lab <- uj::p0("1_",   m_fam, "_",   m_fun)
    s_fun <-         uj::sss_funs();   s_fam <- base::rep(        "sss", uj::N(  s_fun));   s_lab <- uj::p0("1_",   s_fam, "_",   s_fun)
   or_fun <-      uj::ppp_or_funs();  or_fam <- base::rep(     "ppp_or", uj::N( or_fun));  or_lab <- uj::p0("2_",  or_fam, "_",  or_fun)
   bc_fun <-     uj::bbb_ccc_funs();  bc_fam <- base::rep(    "bbb_ccc", uj::N( bc_fun));  bc_lab <- uj::p0("3_",  bc_fam, "_",  bc_fun)
   bm_fun <-     uj::bbb_mmm_funs();  bm_fam <- base::rep(    "bbb_mmm", uj::N( bm_fun));  bm_lab <- uj::p0("3_",  bm_fam, "_",  bm_fun)
   mc_fun <-     uj::mmm_ccc_funs();  mc_fam <- base::rep(    "mmm_ccc", uj::N( mc_fun));  mc_lab <- uj::p0("3_",  mc_fam, "_",  mc_fun)
   sc_fun <-     uj::sss_ccc_funs();  sc_fam <- base::rep(    "sss_ccc", uj::N( sc_fun));  sc_lab <- uj::p0("3_",  sc_fam, "_",  sc_fun)
   cc_fun <-     uj::cmp_ccc_funs();  cc_fam <- base::rep(    "cmp_ccc", uj::N( cc_fun));  cc_lab <- uj::p0("4_",  cc_fam, "_",  cc_fun)
   cm_fun <-     uj::cmp_mmm_funs();  cm_fam <- base::rep(    "cmp_mmm", uj::N( cm_fun));  cm_lab <- uj::p0("4_",  cm_fam, "_",  cm_fun)
   uc_fun <-     uj::unq_mmm_funs();  uc_fam <- base::rep(    "unq_ccc", uj::N( uc_fun));  uc_lab <- uj::p0("4_",  uc_fam, "_",  uc_fun)
   um_fun <-     uj::unq_mmm_funs();  um_fam <- base::rep(    "unq_mmm", uj::N( um_fun));  um_lab <- uj::p0("4_",  um_fam, "_",  um_fun)
  cmc_fun <- uj::cmp_mmm_ccc_funs(); cmc_fam <- base::rep("cmp_mmm_ccc", uj::N(cmc_fun)); cmc_lab <- uj::p0("5_", cmc_fam, "_", cmc_fun)
  umc_fun <- uj::unq_mmm_ccc_funs(); umc_fam <- base::rep("unq_mmm_ccc", uj::N(umc_fun)); umc_lab <- uj::p0("5_", umc_fam, "_", umc_fun)
  fun <- base::c(b_fun, c_fun, d_fun, e_fun, i_fun, m_fun, s_fun, or_fun, bc_fun, bm_fun, mc_fun, sc_fun, cc_fun, cm_fun, uc_fun, um_fun, cmc_fun, umc_fun)
  fam <- base::c(b_fam, c_fam, d_fam, e_fam, i_fam, m_fam, s_fam, or_fam, bc_fam, bm_fam, mc_fam, sc_fam, cc_fam, cm_fam, uc_fam, um_fam, cmc_fam, umc_fam)
  ord <- base::c(b_lab, c_lab, d_lab, e_lab, i_lab, m_lab, s_lab, or_lab, bc_lab, bm_lab, mc_lab, sc_lab, cc_lab, cm_lab, uc_lab, um_lab, cmc_lab, umc_lab)
  ord <- base::order(ord)
  fun <- fun[ord]
  fam <- fam[ord]
  uj::f0(!as.dtf, uj::UV(fun[ord]), base::unique(uj::tb(family = fam[ord], fun = fun[ord])))
}

#' @rdname ppp
#' @export
is_prop <- function(prop) {uj::f0(!uj::cmp_ch3_scl(prop), F, uj::isIN1(base::tolower(prop), uj::all_props()))}

#' @rdname ppp
#' @export
is_prop_fun <- function(fun) {
  uj::err_if_not(uj::cmp_chr_scl(fun), "[fun] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  uj::isIN(fun, uj::prop_funs())
}

#' @rdname ppp
#' @export
is_prop_spec <- function(spec) {uj::f0(uj::cmp_chr_scl(spec), uj::allIN(uj:::spec2props(spec), uj::all_props()), F)}

#' @rdname ppp
#' @export
is_prop_combo <- function(combo) {uj::f0(!uj::cmp_chr_scl(combo), F, uj::allIN(uj:::.spec2props(combo), uj::all_props()))}

#' @rdname ppp
#' @export
props_from_spec <- function(spec, valid = all_props()) {
  ok.valid <- uj::f0(uj::cmp_chr_vec(valid), uj::allIN(valid, uj::all_props()), F)
  uj::errs_if_nots(uj::cmp_chr_scl(spec), "[spec] must be a complete character scalar (?cmp_chr_scl).",
                   ok.valid             , "[valid] must be a complete character vector (?cmp_chr_vec) containing only values from all_props().", PKG = "uj")
  singles <- uj:::.spec2props(spec)
  uj::err_if(uj::N0(singles), "[spec] is empty.", PKG = "uj")
  uj::err_if_not(uj::allIN(singles, valid), "The property spec [spec] contains a property not in [valid].", PKG = "uj")
  uj::SUV(singles)
}

#' @rdname ppp
#' @export
combos_from_spec <- function(spec, valid = uj::all_props()) {
  singles <- uj::props_from_spec(spec, valid = valid)
  uj::SUV(uj:::.spec2combos(spec))
}

#' @rdname ppp
#' @export
props_from_combo <- function(combo, valid = uj::all_props()) {uj::f0(uj::N1(uj:::.spec2props(combo)), uj::SUV(uj:::.combo2props(combo)), uj::stopper("[combo] contains more than one combination property.", PKG = "uj"))}

#' @rdname ppp
#' @export
PPP <- function(x, spec, ...) {
  uj::err_if_not(uj::is_prop_spec(spec), "[spec] specifies a property not in all_props().", PKG = "uj")
  if (uj::meets(x, ...)) {
    all.props <- uj::all_props()
    combos <- uj::combos_from_spec(spec)
    for (combo in combos) {
      is.one <- uj::isIN1(combo, all.props)
      is.fun <- uj::is_prop_fun(combo)
      if (!is.one & !is.fun) {
        singles <- uj::props_from_combo(combo)
        meets <- T
        for (prop in singles) {if (meets) {meets <- meets & base::eval(base::parse(text = uj::p0("i", base::toupper(prop), "(x)")))}}
      } else if (is.one) {meets <- base::eval(base::parse(text = uj::p0("i", combo, "(x)")))}
      else {meets <- base::eval(base::parse(text = uj::p0(combo, "(x)")))}
      if (meets) {return(T)}
    }}
  F
}

#' @rdname ppp
#' @export
nll_or <- function(x, spec, ...) {uj::f0(uj::null(x), T, uj::PPP(x, spec, ...))}

#' @rdname ppp
#' @export
nas_or <- function(x, spec, ...) {uj::f0(uj::isNAS(x), T, uj::PPP(x, spec, ...))}

#' @rdname ppp
#' @export
prop_defs <- function() {
  uj::trb(
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
  uj::errs_if_nots(uj::is_prop(prop), "[prop] must be a character scalar containing a single property spec.",
                   uj::isTF1(print), "[print] must be TRUE or FALSE."                                       , PKG = "uj" )
  if (uj::DEF(prop)) {prop <- uj::props_from_spec(prop)}
  uj::err_if_not(uj::N1(prop), "[prop] contains more than 1 property value.", PKG = "uj")
  y <- uj::prop_defs()
  if (uj::DEF(prop)) {y <- uj::p0("\n family: '", uj::av(y$family[y$value == prop]), "'"  ,
                                  "\n value:  '", prop, "'"                               ,
                                  "\n short:   ", uj::av(y$short[y$value == prop])        ,
                                  "\n long:    ", uj::av(y$long[y$value == prop]), ".\n\n")}
  if (!print) {y} else if (uj::DTF(y)) {base::print(y, n = uj::NR(y))} else {base::cat(y)}
}

#' @rdname ppp
#' @export
combo_concise <- function(combo) {
  uj::err_if_not(uj::is_prop_combo(combo), "[combo] does not contain a valid property combo spec.", PKG = "uj")
  combo <- uj::props_from_combo(combo)
  defs <- uj::prop_defs()
  fam <- uj::av(defs$family)
  val <- uj::av(defs$value)
  abb <- uj::av(defs$short)
  bbb <- abb[uj::isIN1(val, combo) & fam == "bbb"]
  ccc <- abb[uj::isIN1(val, combo) & fam == "ccc"]
  ddd <- abb[uj::isIN1(val, combo) & fam == "ddd"]
  eee <- abb[uj::isIN1(val, combo) & fam == "eee"]
  iii <- abb[uj::isIN1(val, combo) & fam == "iii"]
  mmm <- abb[uj::isIN1(val, combo) & fam == "mmm"]
  sss <- abb[uj::isIN1(val, combo) & fam == "sss"]
  object <- uj::N0(ccc)
  y <- uj::g(", ", base::c(bbb, ccc, ddd, eee, iii, mmm, sss))
  y <- uj::av(base::strsplit(y, ", ", T))
  y <- y[!base::duplicated(y)]
  y <- uj::g(", ", y)
  if (object) {y <- uj::p0(y, " object")}
  prefix <- uj::f0(uj::isIN1(base::substr(y, 1, 1), "a", "e", "i", "o", "u"), "an ", "a ")
  uj::p0(prefix, y)
}

#' @rdname ppp
#' @export
spec_concise <- function(spec) {
  uj::err_if_not(uj::is_prop_spec(spec), "[spec] is not a valid property spec.", PKG = "uj")
  combos <- uj::combos_from_spec(spec)
  for (i in 1:uj::N(combos)) {combos[i] <- uj::combo_concise(combos[i])}
  uj::p0(" OR ", combos)
}
