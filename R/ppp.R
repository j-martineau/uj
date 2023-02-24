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
.CCC <- base::toupper(uj:::.ccc)
.SSS <- base::toupper(uj:::.sss)
.BBB <- base::toupper(uj:::.bbb)
.III <- base::toupper(uj:::.iii)
.EEE <- base::toupper(uj:::.eee)
.DDD <- base::toupper(uj:::.ddd)
.PPP <- base::unique(base::sort(base::c(uj:::.BBB, uj:::.CCC, uj:::.DDD, uj:::.EEE, uj:::.III, uj:::.MMM, uj:::.SSS)))

## BBB ####

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

.NIL <- function(x) {base::length(x) == 0}

.NLL <- function(x) {uj::null(x)}

.POP <- function(x) {base::length(x) > 0}

.RCR <- function(x) {base::is.recursive(x)}

## CCC ####

.ARR <- function(x) {base::is.array(x)}

.DTF <- function(x) {base::is.data.frame(x)}

.GEN <- function(x) {base::is.array(x) | base::is.vector(x)}

.MAT <- function(x) {base::is.matrix(x)}

.MVC <- function(x) {
  if (base::length(x) < 2) {F}
  else if (base::is.vector(x)) {T}
  else if (!base::is.array(x)) {F}
  else {base::length(base::which(base::dim(x) > 1)) == 1}
}

.SCL <- function(x) {if (base::length(x) != 1) {F} else {base::is.vector(x) | base::is.array(x)}}

.VEC <- function(x) {
  if (base::length(x) == 0) {F}
  else if (base::is.vector(x)) {T}
  else if (!base::is.array(x)) {F}
  else {base::length(base::which(base::dim(x) > 1)) < 2}
}

.VLS <- function(x) {if (base::is.data.frame(x)) {F} else {base::is.list(x)}}

## DDD ####

.D0D <- function(x) {uj::null(x)}

.D1D <- function(x) {base::is.vector(x)}

.D2D <- function(x) {base::is.matrix(x) | base::is.data.frame(x)}

.DHD <- function(x) {if (is.array(x)) {base::length(base::dim(x)) > 2} else {F}}

## EEE ####

.E0D <- function(x) {
  if (base::is.atomic(x)) {base::length(x) == 1}
  else if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 1}
  else if (base::is.list(x)) {base::length(x) == 1}
  else {F}
}

.E1D <- function(x) {
  if (base::is.atomic(x)) {
    if (base::vector(x)) {base::length(x) > 1}
    else if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) == 1}
    else {F}
  } else if (base::is.data.frame(x)) {
    if (base::NROW(x) == 1) {base::NCOL(x) > 1}
    else if (base::NCOL(x) == 1) {base::NROW(x) > 1}
    else {F}
  } else if (base::is.list(x)) {base::length(x) > 1}
  else {F}
}

.EUD <- function(x) {base::length(x) == 0}

.E2D <- function(x) {base::length(base::which(base::dim(x) > 1)) == 2}

.EHD <- function(x) {base::length(base::which(base::dim(x) > 1)) > 2}

## III ####

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

.NAS <- function(x) {
  if (!base::is.atomic(x)) {F}
  else if (base::length(x) == 1) {F}
  else {base::is.na(x)}
}

.OKS <- function(x) {
  if (!base::is.atomic(x)) {F}
  else if (base::length(x) == 1) {F}
  else {!base::is.na(x)}
}

.DUP <- function(x) {
  if (!base::is.atomic(x)) {
    if (base::length(x) == 0) {F}
    else if (base::any(base::is.na(x))) {F}
    else {base::length(x) != base::length(base::unique(x))}
  } else {F}
}

.UNQ <- function(x) {
  if (!base::is.atomic(x)) {
    if (base::length(x) == 0) {F}
    else if (base::any(base::is.na(x))) {F}
    else {base::length(x) == base::length(base::unique(x))}
  } else {F}
}

## MMM ####

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
        if (base::length(x) > 0) {!assertthat::is.error(uj::failsafe(grDevices::col2rgb(x)))}
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
      if (!base::all(base::is.na(x))) {!base::is.character(x) & !base::is.numeric(x) & !base::is.ordered(x)}
      else {T}
    } else {T}
  } else {F}
}

.SRT <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {base::is.character(x) | base::is.numeric(x) | base::is.ordered(x)}
      else {T}
    } else {T}
  } else {F}
}

.LGL <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.logical(x)) {T}
      else {base::all(base::is.na(x))}
    } else {T}
  } else {F}
}

.NUM <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.numeric(x)) {T}
      else {base::all(base::is.na(x))}
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
          if (base::all(x == base::round(x))) {base::all(x < 0)}
          else {F}
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
          if (base::all(x == base::round(x))) {base::all(x >= 0)}
          else {F}
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
          if (base::all(x == base::round(x))) {base::all(x <= 0)}
          else {F}
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
          if (base::all(x == base::round(x))) {base::all(x > 0)}
          else {F}
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

.PCT <- function(x) {
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

## sss ###

.COL <- function(x) {if (base::is.data.frame(x) | base::is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) == 1} else {F}}

.ROW <- function(x) {if (base::is.data.frame(x) | base::is.matrix(x)) {base::NROW(x) == 1 & base::NCOL(x) > 1} else {F}}

.PNT <- function(x) {if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 1} else {base::length(x) == 1}}

.EMP <- function(x) {
  if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 0}
  else if (base::length(x) == 0) {!base::is.null(x)}
  else {F}
}

.LIN <- function(x) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x)) {
      if (base::NROW(x) == 1 & base::NCOL(x) > 1) {T}
      else {base::NROW(x) > 1 & base::NCOL(x) == 1}
    } else if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) == 2}
    else if (base::is.list(x)) {T}
    else if (base::is.vector(x)) {T}
  } else {F}
}

.RCT <- function(x) {
  if (base::length(x) > 1) {if (base::is.data.frame(x) | is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) > 1} else {F}}
  else {F}
}

.SLD <- function(x) {if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) > 2} else {F}}

.SQR <- function(x) {if (base::is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) > 1 & base::NROW(x) == base::NCOL(x)} else {F}}

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
    if (uj::N1(.spec2combos)) {
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
#' \tabular{ll}{  \code{\link{bbb}}   \tab basic properties                       \cr
#'                \code{\link{ccc}}   \tab xclass (extended class)                \cr
#'                \code{\link{ddd}}   \tab defined.D (defined dimensionality)     \cr
#'                \code{\link{eee}}   \tab effective.D (effective dimensionality) \cr
#'                \code{\link{iii}}   \tab integrity (completeness, uniqueness)   \cr
#'                \code{\link{mmm}}   \tab xmode (extended modes)                 \cr
#'                \code{\link{sss}}   \tab shape (geometric shape)                  }
#' This family uses concise and flexible *property specs* as defined in the following table.
#' \tabular{ll}{  *single*   \tab \link[=CH3]{3-char} scalars (i.e., `all(nchar(.) == 3)` with all values in `all_props()`. For example, `'ord', 'dtf', 'd2d',` and `'rct'` are *single* property specs.                                                                                                                                                                 \cr   \tab   \cr
#'                *combo*    \tab Character scalars containing multiple underscore-delimited *single* properties, indicating that those *single* properties must co-occur. For example, the *combo* property spec `'ord_dtf_d2d_rct'` (or an equivalent underscore-delimited permutation) indicates that all four *single* properties must co-occur to satisfy the spec. \cr   \tab   \cr
#'                *option*   \tab Character scalars containing multiple pipe-delimited *combo* and/or *single* property specs. For example, the *option* property spec `'ord|dtf_d2d|dtf_rct'` would be satisfied by an object with *single* property `'ord'`, *combo* property `'dtf_d2d'`, **or** *combo* property `'dtf_rct'`.                                        \cr   \tab   \cr
#'                *flex*     \tab A *single*, *combo*, or *option* property spec as defined above.                                                                                                                                                                                                                                                                                      }
#' @details **Functions for property spec decomposition**
#' \tabular{ll}{  `props_from_combo`   \tab What are the constituent *single* properties of a *combo* property spec? \cr   \tab   \cr
#'                `combo_from_spec`    \tab What are the constituent *combo* properties in a *flex* property spec?   \cr   \tab   \cr
#'                `props_from_spec`    \tab What are the *unique* constituent *single* properties in a *flex* property spec?        }
#' **Functions to check whether a value is a valid property spec**
#' \tabular{ll}{  `is_prop_combo`   \tab Is `combo` a valid *combo* property spec?                    \cr   \tab   \cr
#'                `is_prop_spec`    \tab Is `spec` a valid *flex* property spec?                      \cr   \tab   \cr
#'                `is_prop_fun`     \tab Is `fun` the name of a dedicated property checking function? \cr   \tab   \cr
#'                `is_prop`         \tab Is `prop` a valid *single* property?                                        }
#' **Functions to define properties and property specs**
#' \tabular{ll}{  `combo_concise`   \tab How is a *combo* property spec (concisely) defined?                       \cr   \tab   \cr
#'                `spec_concise`    \tab How is an *options* property spec (concisely) defined?                    \cr   \tab   \cr
#'                `prop_verbose`    \tab How is a *single* property (verbosely) defined?                           \cr   \tab   \cr
#'                `prop_defs`       \tab What are the definitions of all possible *single* properties (returned as a data.frame)? }
#' **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `ppp_or_funs`   \tab What dedicated functions check `x` for either special values or a match to a *flex* property spec? \cr   \tab   \cr
#'                `prop_funs`     \tab What dedicated functions check `x` for a match to a specific *single* or *combo* property?         \cr   \tab   \cr
#'                `all_props`     \tab What is the complete set of all possible *single* properties?                                                     }
#' **Functions to check object against arbitrary property specs**
#' \tabular{ll}{  `nas_or`   \tab Is `x` either `NA` or a match to the *flex* property spec in `spec`?  \cr   \tab   \cr
#'                `nll_or`   \tab Is `x` either `NULL` or a match to the *flex* property spec in `spec` \cr   \tab   \cr
#'                `ppp`      \tab Is `x` a match to the *flex* property spec in `spec`?                                }
#' **Function to list all of an object's** single **properties across property families**
#' \tabular{ll}{  `ppp`   \tab What are all of `x`'s *single* properties compiled from all property families? }
#' For convenience, property functions from other function families are also described below.
#' \cr\cr **Functions to list all** single **properties in a property family**
#' \tabular{ll}{  `bbb_props`   \tab \link[=bbb]{basic}       \cr
#'                `ccc_props`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd_props`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee_props`   \tab \link[=eee]{effective.D} \cr
#'                `iii_props`   \tab \link[=iii]{integrity}   \cr
#'                `mmm_props`   \tab \link[=mmm]{xmode}       \cr
#'                `sss_props`   \tab \link[=sss]{shape}         }
#' **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `cmp_mmm_ccc_funs`   \tab integrity = `'cmp'` + xmode + xclass \cr
#'                `unq_mmm_ccc_funs`   \tab integrity = `'unq'` + xmode + xclass \cr   \tab  }
#' \tabular{ll}{  `cmp_ccc_funs`   \tab integrity = `'cmp'` + xclass \cr
#'                `cmp_mmm_funs`   \tab integrity = `'cmp'` + xmode  \cr
#'                `unq_ccc_funs`   \tab integrity = `'unq'` + xclass \cr
#'                `unq_mmm_funs`   \tab integrity = `'unq'` + xmode  \cr
#'                `bbb_ccc_funs`   \tab basic + xclass               \cr
#'                `bbb_mmm_funs`   \tab basic + xmode                \cr
#'                `mmm_ccc_funs`   \tab xmode + xclass               \cr   \tab  }
#' \tabular{ll}{  `bbb_funs`   \tab \link[=bbb]{basic}       \cr
#'                `ccc_funs`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd_funs`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee_funs`   \tab \link[=eee]{effective.D} \cr
#'                `iii_funs`   \tab \link[=iii]{integrity}   \cr
#'                `mmm_funs`   \tab \link[=mmm]{xmode}       \cr
#'                `sss_funs`   \tab \link[=sss]{shape}         }
#' **Dedicated functions to check an object for a specific** combo **or** single **property**
#' \cr\cr For these functions, `{bbb}`, `{ccc}`, `{ddd}`, `{eee}`, `{iii}`, `{mmm}`, and `{sss}` are placeholders for any given basic, xclass, defined.D, effective.D, integrity, xmode, and shape properties, respectively.
#' \tabular{ll}{  `{bbb}`   \tab \link[=bbb]{basic} = `'{bbb}'`       \cr
#'                `{ccc}`   \tab \link[=ccc]{xclass} = `'{ccc}'`      \cr
#'                `{ddd}`   \tab \link[=ddd]{defined.D} = `'{ddd}'`   \cr
#'                `{eee}`   \tab \link[=eee]{effective.D} = `'{eee}'` \cr
#'                `{iii}`   \tab \link[=iii]{integrity} = `'{iii}'`   \cr
#'                `{mmm}`   \tab \link[=mmm]{xmode} = `'{mmm}'`       \cr
#'                `{sss}`   \tab \link[=sss]{shape} = `'{sss}'`       \cr   \tab  }
#' \tabular{ll}{  `cmp_{ccc}`   \tab integrity = `'cmp'` + xclass = `'{ccc}'` \cr
#'                `cmp_{mmm}`   \tab integrity = `'cmp'` + xmode = `'{mmm}'`  \cr
#'                `unq_{ccc}`   \tab integrity = `'unq'` + xclass = `'{ccc}'` \cr
#'                `unq_{mmm}`   \tab integrity = `'unq'` + xmode = `'{mmm}'`  \cr   \tab  }
#' \tabular{ll}{  `{bbb}_{ccc}`   \tab basic = `'{bbb}'` + xclass = `'{ccc}'` \cr
#'                `{bbb}_{mmm}`   \tab basic = `'{bbb}'` + xmode = `'{mmm}'`  \cr
#'                `{mmm}_{ccc}`   \tab xmode = `'{mmm}'` + xclass = `'{ccc}'` \cr
#'                `{sss}_{ccc}`   \tab shape = `'{sss}'` + xclass = `'{ccc}'` \cr   \tab  }
#' \tabular{ll}{  `cmp_{mmm}_{ccc}`   \tab integrity = `'cmp'` + xmode = `'{mmm}'` + xclass = `'{ccc}'` \cr
#'                `unq_{mmm}_{ccc}`   \tab integrity = `'unq'` + xmode = `'{mmm}'` + xclass = `'{ccc}'`   }
#' **Functions to check an object against an arbitrary* combo *property spec**
#' \cr\cr For these functions, an uppercase letter repeated three times is a placeholder for the value of an arbitrary single property from the associated property family.
#' \tabular{ll}{  `bbb_ccc`   \tab basic property in arg `bbb` + xclass property in arg `ccc` \cr
#'                `mmm_ccc`   \tab xmode property in arg `mmm` + xclass property in arg `ccc` \cr
#'                `cmp_ccc`   \tab integrity = `'cmp'` + xclass property in arg `ccc`         \cr
#'                `sss_ccc`   \tab shape property in arg `sss` + xclass property in arg `ccc` \cr
#'                `unq_ccc`   \tab integrity = `'unq'` + xclass property in arg `ccc`         \cr
#'                `bbb_mmm`   \tab basic property in arg `bbb` + xmode property in arg `mmm`  \cr
#'                `cmp_mmm`   \tab integrity = `'cmp'` + xmode property in arg `mmm`          \cr
#'                `unq_mmm`   \tab integrity = `'unq'` + xmode property in arg `mmm`          \cr   \tab  }
#' \tabular{ll}{  `cmp_mmm_ccc`   \tab integrity = `'cmp'` + xmode property in arg `mmm` + xclass property in arg `ccc` \cr   \tab   \cr
#'                `unq_mmm_ccc`   \tab integrity = `'unq'` + xmode property in arg `mmm` + xclass property in arg `ccc` }
#' **Functions to check objects against* flex *property specs in a single family**
#' \tabular{ll}{  `BBB`   \tab \link[=bbb]{basic}       \cr
#'                `CCC`   \tab \link[=ccc]{xclass}      \cr
#'                `DDD`   \tab \link[=ddd]{defined.D}   \cr
#'                `EEE`   \tab \link[=eee]{effective.D} \cr
#'                `III`   \tab \link[=iii]{integrity}   \cr
#'                `MMM`   \tab \link[=mmm]{xmode}       \cr
#'                `SSS`   \tab \link[=sss]{shape}         }
#' **Functions to retrieve all of an object's* single *properties of a specific family**
#' \tabular{ll}{  `bbb`   \tab \link[=bbb]{basic}       \cr
#'                `ccc`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee`   \tab \link[=eee]{effective.D} \cr
#'                `iii`   \tab \link[=iii]{integrity}   \cr
#'                `mmm`   \tab \link[=mmm]{xmode}       \cr
#'                `sss`   \tab \link[=sss]{shape}         }
#' @param as.dtf `TRUE` or `FALSE` indicating whether to return the result as a data.frame with column `1` containing property values and column `2` containing the property families.
#' @param x An R object.
#' @param spec A \link[=cmp_chr_scl]{complete character scalar} containing one or more values from `ppp_vals()` separated by pipes and/or underscores. Combinations of properties can be specified by separating them with underscores. Separating properties or combinations of properties with pipes will result in a value of `TRUE` if any of them applies to `x`.
#' @param valid A \link[=cmp_chr_vec]{complete character vec} containing all properties considered valid.
#' @param print `TRUE` or `FALSE` indicating whether to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character scalar** \cr\cr `combo_concise`    \cr `prop_verbose`     \cr `spec_concise`
#' \cr\cr  **A character vector** \cr\cr `combos_from_spec` \cr `props_from_combo` \cr `props_from_specs` \cr `ppp_or_funs` \cr `all_props` \cr `prop_funs` \cr `ppp`
#' \cr\cr  **A logical scalar**   \cr\cr `is_prop_combo`    \cr `is_prop`          \cr `is_prop_spec`     \cr `is_prop_fun` \cr `nas_or`    \cr `nll_or`    \cr `PPP`
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
#' prop_defs()
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
props_from_combo <- function(combo, valid = uj::all_props()) {uj::f0(uj::N1(uj:::.spec2props(combo)), uj::SUV(uj:::.combo2props(combo)), uj::stopperr("[combo] contains more than one combination property.", PKG = "uj"))}

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
    "iii"  , "dup"   , "atomic, complete, with duplicates" , "A complete dup atomic object (containing no NA values, containing duplicate values)",
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
