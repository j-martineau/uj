#' @family properties
#' @title Fast property checking for common checks
#' @details Fast property checking for \code{\link{ppp}} functions without the dot prefix.
#' @return `TRUE` or `FALSE`.
ppp_fast <- function() {utils::help("ppp_fast", "uj")}

# xxx ####

#' @rdname ppp_fast
#' @export
.ATM <- function(X) {if (base::is.atomic(X)) {!base::is.null(X)} else {F}}

#' @rdname ppp_fast
#' @export
.DEF <- function(X) {!base::is.null(X)}

#' @rdname ppp_fast
#' @export
.FUN <- function(X) {
  if (!base::is.function(X)) {
    if (base::length(X) == 1 & base::is.character(X)) {
      X <- tryCatch(base::match.fun(X), error = function(X) e, finally = NULL)
      !rlang::is_error(X)
    } else {F}
  } else {T}
}

# basic

#' @rdname ppp_fast
#' @export
.NIL <- function(X) {base::length(X) == 0}

#' @rdname ppp_fast
#' @export
.NLL <- function(X) {uj::null(X)}

#' @rdname ppp_fast
#' @export
.POP <- function(X) {base::length(X) > 0}

#' @rdname ppp_fast
#' @export
.RCR <- function(X) {base::is.recursive(X)}

# xclass

#' @rdname ppp_fast
#' @export
.ARR <- function(X) {base::is.array(X)}

#' @rdname ppp_fast
#' @export
.DTF <- function(X) {base::is.data.frame(X)}

#' @rdname ppp_fast
#' @export
.GEN <- function(X) {base::is.array(X) | base::is.vector(X)}

#' @rdname ppp_fast
#' @export
.MAT <- function(X) {base::is.matrix(X)}

#' @rdname ppp_fast
#' @export
.MVC <- function(X) {if (base::length(X) < 2) {F} else if (base::is.vector(X)) {T} else if (!base::is.array(X)) {F} else {base::length(base::which(base::dim(X) > 1)) == 1}}

#' @rdname ppp_fast
#' @export
.SCL <- function(X) {if (base::length(X) != 1) {F} else {base::is.vector(X) | base::is.array(X)}}

#' @rdname ppp_fast
#' @export
.VLS <- function(X) {if (base::is.data.frame(X)) {F} else {base::is.list(X)}}

#' @rdname ppp_fast
#' @export
.VEC <- function(X) {if (base::length(X) == 0) {F} else if (base::is.vector(X)) {T} else if (!base::is.array(X)) {F} else {base::length(base::which(base::dim(X) > 1)) < 2}}

# defined D

#' @rdname ppp_fast
#' @export
.D0D <- function(X) {uj::null(X)}

#' @rdname ppp_fast
#' @export
.D1D <- function(X) {base::is.vector(X)}

#' @rdname ppp_fast
#' @export
.D2D <- function(X) {base::is.matrix(X) | base::is.data.frame(X)}

#' @rdname ppp_fast
#' @export
.DHD <- function(X) {if (is.array(X)) {base::length(base::dim(X)) > 2} else {F}}

# effective D

#' @rdname ppp_fast
#' @export
.EUD <- function(X) {base::length(X) == 0}

#' @rdname ppp_fast
#' @export
.E2D <- function(X) {base::length(base::which(base::dim(X) > 1)) == 2}

#' @rdname ppp_fast
#' @export
.EHD <- function(X) {base::length(base::which(base::dim(X) > 1)) > 2}

#' @rdname ppp_fast
#' @export
.E0D <- function(X) {if (base::is.atomic(X)) {base::length(X) == 1} else if (base::is.data.frame(X)) {base::NROW(X) * base::NCOL(X) == 1} else if (base::is.list(X)) {base::length(X) == 1} else {F}}

#' @rdname ppp_fast
#' @export
.E1D <- function(X) {
  if (base::is.atomic(X)) {
    if (base::is.vector(X)) {base::length(X) > 1}
    else if (base::is.array(X)) {base::length(base::which(base::dim(X) > 1)) == 1}
    else {F}
  } else if (base::is.data.frame(X)) {
    if (base::NROW(X) == 1) {base::NCOL(X) > 1}
    else if (base::NCOL(X) == 1) {base::NROW(X) > 1}
    else {F}
  } else if (base::is.list(X)) {base::length(X) > 1}
  else {F}
}

# integrity

#' @rdname ppp_fast
#' @export
.NA0 <- function(X) {if (!base::is.atomic(X) | base::length(X) != 1) {F} else {base::is.na(X)}}

#' @rdname ppp_fast
#' @export
.OK0 <- function(X) {if (!base::is.atomic(X) | base::length(X) != 1) {F} else {!base::is.na(X)}}

#' @rdname ppp_fast
#' @export
.DUP <- function(X) {if (!base::is.atomic(X) | base::length(X) == 0) {F} else {base::length(X) != base::length(base::unique(X))}}

#' @rdname ppp_fast
#' @export
.UNQ <- function(X) {if (!base::is.atomic(X) | base::length(X) == 0) {F} else {base::length(X) == base::length(base::unique(X))}}

#' @rdname ppp_fast
#' @export
.CMP <- function(X) {if (!base::is.atomic(X) | base::length(X) == 0) {F} else {!base::any(base::is.na(X))}}

#' @rdname ppp_fast
#' @export
.MSS <- function(X) {if (!base::is.atomic(X) | base::length(X) == 0) {F} else {base::all(base::is.na(X))}}

#' @rdname ppp_fast
#' @export
.PRT <- function(X) {if (!base::is.atomic(X) | base::length(X) <= 1) {F} else {base::any(base::is.na(X)) & base::any(!base::is.na(X))}}

# xmode

#' @rdname ppp_fast
#' @export
.CH1 <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.character(X)) {
          X <- X[!base::is.na(X)]
          if (base::length(X) == 0) {T}
          else {base::all(base::nchar(X) == 1)}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CH3 <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.character(X)) {
          X <- X[!base::is.na(X)]
          if (base::length(X) == 0) {T}
          else {base::all(base::nchar(X) == 3)}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CHR <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (base::is.character(X)) {T}
      else {base::all(base::is.na(X))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.STR <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.character(X)) {
          X <- X[!base::is.na(X)]
          if (base::length(X) == 0) {T}
          else {base::all(base::nchar(X) > 0)}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CLR <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        X <- X[!base::is.na(X)]
        if (base::length(X) > 0) {!uj::is_err(grDevices::col2rgb(X))}
        else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.FAC <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (base::is.factor(X)) {T}
      else {base::all(base::is.na(X))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.ORD <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (base::is.ordered(X)) {T}
      else {base::all(base::is.na(X))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.UNO <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (base::is.factor(X) & !base::is.ordered(X)) {T}
      else {base::all(base::is.na(X))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.IND <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        X <- X[!base::is.na(X)]
        if (base::is.numeric(X)) {base::all(X == base::round(X))}
        else {base::is.logical(X)}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NST <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {!base::is.character(X) & !base::is.numeric(X) & !base::is.ordered(X)} else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.SRT <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {base::is.character(X) | base::is.numeric(X) | base::is.ordered(X)} else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.LGL <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (base::is.logical(X)) {T} else {base::all(base::is.na(X))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NUM <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (base::is.numeric(X)) {T} else {base::all(base::is.na(X))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.WHL <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::all(X == base::round(X))
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.ODD <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          if (base::all(X == base::round(X))) {
            X <- X / 2
            base::any(X != base::round(X))
          } else {F}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.EVN <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          if (base::all(X == base::round(X))) {
            X <- X / 2
            base::all(X == base::round(X))
          } else {F}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NGW <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          if (base::all(X == base::round(X))) {base::all(X < 0)} else {F}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

.NNW <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          if (base::all(X == base::round(X))) {base::all(X >= 0)} else {F}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NPW <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          if (base::all(X == base::round(X))) {base::all(X <= 0)} else {F}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PSW <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          if (base::all(X == base::round(X))) {base::all(X > 0)} else {F}
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.FRC <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::any(X != base::round(X))
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NEG <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::all(X < 0)
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.POS <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::all(X > 0)
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NNW <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::all(X >= 0)
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NPS <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::all(X <= 0)
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PCT <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::all(X >= 0 | X <= 100)
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PPN <- function(X) {
  if (base::is.atomic(X)) {
    if (base::length(X) > 0) {
      if (!base::all(base::is.na(X))) {
        if (base::is.numeric(X)) {
          X <- X[!base::is.na(X)]
          base::all(X >= 0 | X <= 1)
        } else {F}
      } else {T}
    } else {T}
  } else {F}
}

# shape

#' @rdname ppp_fast
#' @export
.COL <- function(X) {if (base::is.data.frame(X) | base::is.matrix(X)) {base::NROW(X) > 1 & base::NCOL(X) == 1} else {F}}

#' @rdname ppp_fast
#' @export
.ROW <- function(X) {if (base::is.data.frame(X) | base::is.matrix(X)) {base::NROW(X) == 1 & base::NCOL(X) > 1} else {F}}

#' @rdname ppp_fast
#' @export
.PNT <- function(X) {if (base::is.data.frame(X)) {base::NROW(X) * base::NCOL(X) == 1} else {base::length(X) == 1}}

#' @rdname ppp_fast
#' @export
.SLD <- function(X) {if (base::is.array(X)) {base::length(base::which(base::dim(X) > 1)) > 2} else {F}}

#' @rdname ppp_fast
#' @export
.SQR <- function(X) {if (base::is.matrix(X)) {base::NROW(X) > 1 & base::NCOL(X) > 1 & base::NROW(X) == base::NCOL(X)} else {F}}

#' @rdname ppp_fast
#' @export
.EMP <- function(X) {if (base::is.data.frame(X)) {base::NROW(X) * base::NCOL(X) == 0} else if (base::length(X) == 0) {!base::is.null(X)} else {F}}

#' @rdname ppp_fast
#' @export
.RCT <- function(X) {if (base::length(X) > 1) {if (base::is.data.frame(X) | is.matrix(X)) {base::NROW(X) > 1 & base::NCOL(X) > 1} else {F}} else {F}}

#' @rdname ppp_fast
#' @export
.LIN <- function(X) {
  if (base::length(X) > 1) {
    if (base::is.data.frame(X)) {if (base::NROW(X) == 1 & base::NCOL(X) > 1) {T} else {base::NROW(X) > 1 & base::NCOL(X) == 1}}
    else if (base::is.array(X)) {base::length(base::which(base::dim(X) > 1)) == 2}
    else if (base::is.list(X)) {T}
    else if (base::is.vector(X)) {T}
  } else {F}
}

## .xxx_yyy ####

#' @rdname ppp_fast
#' @export
.atm_dtf <- function(X) {
  NC <- base::NCOL(X)
  if (base::is.data.frame(X) & base::NROW(X) > 0 & NC > 0) {
    for (i in 1:NC) {if (!base::is.atomic(X[[i]])) {return(FALSE)}}
    TRUE
  } else {FALSE}
}

#' @rdname ppp_fast
#' @export
.atm_mat <- function(X) {base::is.atomic(X) & base::is.matrix(X)}

#' @rdname ppp_fast
#' @export
.atm_scl <- function(X) {base::is.atomic(X) & base::length(X) == 1}

#' @rdname ppp_fast
#' @export
.atm_vec <- function(X) {base::is.atomic(X) & uj:::.VEC(X)}

#' @rdname ppp_fast
#' @export
.atm_vls <- function(X) {if (base::is.list(X) & !base::is.data.frame(X) & base::length(X) > 0) {base::all(base::sapply(X, base::is.atomic))} else {F}}

#' @rdname ppp_fast
#' @export
.chr_arr <- function(X) {base::is.array(X) & base::is.character(X)}

#' @rdname ppp_fast
#' @export
.chr_dtf <- function(X) {
  NC <- base::NCOL(X)
  if (base::is.data.frame(X) & base::NROW(X) > 0 & NC > 0) {
    for (i in 1:NC) {if (!base::is.character(X[[i]])) {return(FALSE)}}
    TRUE
  } else {FALSE}
}

#' @rdname ppp_fast
#' @export
.chr_vec <- function(X) {uj:::.VEC(X) & base::is.character(X)}

#' @rdname ppp_fast
#' @export
.chr_vls <- function(X) {uj::f0(base::is.list(X) & !is.data.frame(X), base::all(base::sapply(X, base::is.charsacter)), F)}

#' @rdname ppp_fast
#' @export
.cmp_atm <- function(X) {if (base::is.atomic(X) & base::length(X) > 0) {!base::any(base::is.na(X))} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_chr <- function(X) {if (base::is.character(X) & base::length(X) > 0) {!base::any(base::is.na(X))} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_num <- function(X) {if (base::is.numeric(X) & base::length(X) > 0) {!base::any(base::is.na(X))} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_scl <- function(X) {if (base::is.atomic(X) & base::length(X) == 1) {!base::is.na(X)} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_vec <- function(X) {if (!base::is.atomic(X) | base::length(X) == 0) {F} else if (!uj:::.VEC(X)) {F} else {!base::any(base::is.na(X))}}

#' @rdname ppp_fast
#' @export
.lgl_scl <- function(X) {base::logical(X) & base::length(X) == 1}

#' @rdname ppp_fast
#' @export
.nnw_vec <- function(X) {
  if (uj:::.VEC(X)) {
    if (base::is.numeric(X)) {
      X <- X[!base::is.na(X)]
      if (base::length(X) > 0) {
        if (base::all(X >= 0)) {base::all(X == base::round(X))}
        else {F}
      } else {T}
    } else {F}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.pop_atm <- function(X) {base::is.atomic(X) & base::length(X) > 0}

#' @rdname ppp_fast
#' @export
.pop_chr <- function(X) {base::length(X) > 0 & base::is.character(X)}

#' @rdname ppp_fast
#' @export
.pop_dtf <- function(X) {base::is.data.frame(X) & base::NROW(X) > 0 & base::NCOL(X) > 0}

#' @rdname ppp_fast
#' @export
.pop_mat <- function(X) {base::is.matrix(X) & base::length(X) > 0}

#' @rdname ppp_fast
#' @export
.pop_srt <- function(X) {base::length(X) > 0 & (base::is.character(X) | base::is.numeric(X) | base::is.ordered(X))}

#' @rdname ppp_fast
#' @export
.pop_vec <- function(X) {uj:::.VEC(X) & base::length(X) > 0}

#' @rdname ppp_fast
#' @export
.pop_vls <- function(X) {uj:::.VLS(X) & base::length(X) > 0}

## .xxx_yyy_zzz ####

#' @rdname ppp_fast
#' @export
.cmp_atm_vec <- function(X) {if (!uj:::.VEC(X)) {F} else if (!base::is.atomic(X)) {F} else if (base::any(base::is.na(X))) {F}else {T}}

#' @rdname ppp_fast
#' @export
.cmp_ch1_scl <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.character(X)) {F} else if (base::is.na(X)) {F} else {base::nchar(X) == 1}}

#' @rdname ppp_fast
#' @export
.cmp_ch1_vec <- function(X) {if (!uj:::.VEC(X)) {F} else if (!base::is.character(X)) {F} else if (base::any(base::is.na(X))) {F} else {base::all(base::nchar(X) == 1)}}

#' @rdname ppp_fast
#' @export
.cmp_ch3_scl <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.character(X)) {F} else if (base::is.na(X)) {F} else {base::nchar(X) == 3}}

#' @rdname ppp_fast
#' @export
.cmp_ch3_vec <- function(X) {if (!uj:::.VEC(X)) {F} else if (!base::is.character(X)) {F} else if (base::is.na(X)) {F} else {base::all(base::nchar(X) == 3)}}

#' @rdname ppp_fast
#' @export
.cmp_chr_arr <- function(X, Valid = NULL) {
  if      (base::length(X) == 0     ) {F}
  else if (!uj:::.ARR(X)            ) {F}
  else if (!base::is.character(X)   ) {F}
  else if (base::any(base::is.na(X))) {F}
  else if (base::is.null(Valid)     ) {T}
  else {base::all(X %in% Valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_vec <- function(X, Valid = NULL) {if (!uj:::.VEC(X)) {F} else if (!base::is.character(X)) {F} else if (base::any(base::is.na(X))) {F} else if (!base::is.null(Valid)) {base::all(X %in% Valid)} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_chr_vls <- function(X, Valid = NULL) {if (!uj:::.chr_vls(X)) {F} else if (base::any(base::is.na(uj::av(X)))) {F} else if (base::is.null(Valid)) {T} else {base::all(uj::av(X) %in% Valid)}}

#' @rdname ppp_fast
#' @export
.cmp_chr_gen <- function(X, Valid = NULL) {if (uj::.cmp_chr_vec(X, Valid = Valid)) {T} else if (uj::.cmp_chr_arr(X, Valid = Valid)) {T} else {uj::.cmp_chr_vls(X, Valid = Valid)}}

#' @rdname ppp_fast
#' @export
.cmp_chr_scl <- function(X, Valid = NULL) {if (base::length(X) != 1) {F} else if (!base::is.character(X)) {F} else if (base::is.na(X)) {F} else if (!base::is.null(Valid)) {X %in% Valid} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_clr_vec <- function(X) {uj:::.cmp_vec(X) & uj:::.CLR(X)}

#' @rdname ppp_fast
#' @export
.cmp_lgl_scl <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.logical(X)) {F} else {!base::is.na(X)}}

#' @rdname ppp_fast
#' @export
.cmp_lgl_vec <- function(X) {if (!uj:::.VEC(X)) {F} else if (!base::is.logical(X)) {F} else {!base::any(base::is.na(X))}}

#' @rdname ppp_fast
#' @export
.cmp_nng_scl <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.numeric(X)) {F} else if (base::is.na(X)) {F} else {X < 0}}

#' @rdname ppp_fast
#' @export
.cmp_nnw_scl <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.numeric(X)) {F} else if (base::is.na(X)) {F} else if (X < 0) {F} else {X == round(X)}}

#' @rdname ppp_fast
#' @export
.cmp_nnw_vec <- function(X) {if (!uj:::.VEC(X)) {F} else if (!base::is.numeric(X)) {F} else if (base::any(base::is.na(X))) {F} else if (base::any(X < 0)) {F} else {base::all(X == round(X))}}

#' @rdname ppp_fast
#' @export
.cmp_num_scl <- function(X, Valid = NULL) {if (base::length(X) != 1) {F} else if (!base::is.numeric(X)) {F} else if (base::is.na(X)) {F} else if (!base::is.null(Valid)) {X %in% Valid} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_num_vec <- function(X, Valid = NULL) {if (!uj:::.VEC(X)) {F} else if (!base::is.numeric(X)) {F} else if (base::any(base::is.na(X))) {F} else if (!base::is.null(Valid)) {base::all(X %in% Valid)} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_pos_vec <- function(X) {if (base::length(X) == 0) {F} else if (!base::is.numeric(X)) {F} else if (base::any(base::is.na(X))) {F} else {base::all(X >= 0 & X <= 1)}}

#' @rdname ppp_fast
#' @export
.cmp_ppn_vec <- function(X) {if (base::length(X) == 0) {F} else if (!base::is.numeric(X)) {F} else if (base::any(base::is.na(X))) {F} else {base::all(X >= 0 & X <= 1)}}

#' @rdname ppp_fast
#' @export
.cmp_psw_scl <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.numeric(X)) {F} else if (base::is.na(X)) {F} else if (X <= 0) {F} else {X == round(X)}}

#' @rdname ppp_fast
#' @export
.cmp_psw_vec <- function(X) {if (!uj:::.VEC(X)) {F} else if (!base::is.numeric(X)) {F} else if (base::any(base::is.na(X))) {F} else if (base::any(X <= 0)) {F} else {base::all(X == round(X))}}

#' @rdname ppp_fast
#' @export
.cmp_srt_vec <- function(X) {if (!uj:::.VEC(X)) {F} else if (!base::is.numeric(X) & !base::is.character(X) & !base::is.ordered(X)) {F} else {base::any(base::is.na(X))}}

#' @rdname ppp_fast
#' @export
.cmp_str_scl <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.character(X)) {F} else if (base::is.na(X)) {F} else {!base::any(X == "")}}

#' @rdname ppp_fast
#' @export
.cmp_str_vec <- function(X) {if (base::length(X) == 0) {F} else if (!base::is.character(X)) {F} else if (base::any(base::is.na(X))) {F} else {!base::any(X == "")}}

#' @rdname ppp_fast
#' @export
.cmp_atm_mvc <- function(X) {if (!uj:::.MVC(X)) {F} else if (!base::is.atomic(X)) {F} else if (base::any(base::is.na(X))) {F} else {T}}

#' @rdname ppp_fast
#' @export
.unq_atm_mvc <- function(X) {uj::f0(uj:::.cmp_atm_mvc(X), base::length(X) == base::length(base::unique(X)), F)}

#' @rdname ppp_fast
#' @export
.unq_atm_vec <- function(X) {uj::f0(uj:::.cmp_atm_vec(X), base::length(X) == base::length(base::unique(X)), F)}

#' @rdname ppp_fast
#' @export
.unq_ch1_vec <- function(X) {uj::f0(uj:::.cmp_ch1_vec(X), base::length(X) == base::length(base::unique(X)), F)}

#' @rdname ppp_fast
#' @export
.unq_ch3_vec <- function(X) {uj::f0(uj:::.cmp_ch3_vec(X), base::length(X) == base::length(base::unique(X)), F)}

#' @rdname ppp_fast
#' @export
.unq_chr_vec <- function(X, Valid = NULL) {uj::f0(uj:::.cmp_chr_vec(X), base::length(X) == base::length(base::unique(X)), uj::f0(base::is.null(Valid), T, base::all(X %in% Valid)))}

#' @rdname ppp_fast
#' @export
.unq_nnw_vec <- function(X) {uj::f0(uj:::.cmp_nnw_vec(X), base::length(X) == base::length(base::unique(X)), F)}

#' @rdname ppp_fast
#' @export
.unq_num_vec <- function(X, Valid = NULL) {uj::f0(uj:::.cmp_num_vec(X), base::length(X) == base::length(base::unique(X)), uj::f0(base::is.null(Valid), T, base::all(X %in% Valid)))}

#' @rdname ppp_fast
#' @export
.unq_psw_vec <- function(X) {uj::f0(uj:::.cmp_psw_vec(X), base::length(X) == base::length(base::unique(X)), F)}

#' @rdname ppp_fast
#' @export
.unq_str_vec <- function(X) {uj::f0(uj:::.cmp_str_vec(X), base::length(X) == base::length(base::unique(X)), F)}
