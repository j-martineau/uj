#' @family properties
#' @title Fast property checking for common checks
#' @details Fast property checking for \code{\link{ppp}} functions without the dot prefix.
#' @return `TRUE` or `FALSE`.
ppp_fast <- function() {utils::help("ppp_fast", "uj")}

# xxx ####

#' @rdname ppp_fast
#' @export
.ATM <- function() {if (base::is.atomic(x)) {!base::is.null(x)} else {F}}

#' @rdname ppp_fast
#' @export
.DEF <- function(x) {!base::is.null(x)}

#' @rdname ppp_fast
#' @export
.FUN <- function(x) {
  if (!base::is.function(x)) {
    if (base::length(x) == 1 & base::is.character(x)) {
      x <- tryCatch(base::match.fun(x), error = function(x) e, finally = NULL)
      !rlang::is_error(x)
    } else {F}
  } else {T}
}

# basic

#' @rdname ppp_fast
#' @export
.NIL <- function(x) {base::length(x) == 0}

#' @rdname ppp_fast
#' @export
.NLL <- function(x) {uj::null(x)}

#' @rdname ppp_fast
#' @export
.POP <- function(x) {base::length(x) > 0}

#' @rdname ppp_fast
#' @export
.RCR <- function(x) {base::is.recursive(x)}

# xclass

#' @rdname ppp_fast
#' @export
.ARR <- function(x) {base::is.array(x)}

#' @rdname ppp_fast
#' @export
.DTF <- function(x) {base::is.data.frame(x)}

#' @rdname ppp_fast
#' @export
.GEN <- function(x) {base::is.array(x) | base::is.vector(x)}

#' @rdname ppp_fast
#' @export
.MAT <- function(x) {base::is.matrix(x)}

#' @rdname ppp_fast
#' @export
.MVC <- function(x) {if (base::length(x) < 2) {F} else if (base::is.vector(x)) {T} else if (!base::is.array(x)) {F} else {base::length(base::which(base::dim(x) > 1)) == 1}}

#' @rdname ppp_fast
#' @export
.SCL <- function(x) {if (base::length(x) != 1) {F} else {base::is.vector(x) | base::is.array(x)}}

#' @rdname ppp_fast
#' @export
.VLS <- function(x) {if (base::is.data.frame(x)) {F} else {base::is.list(x)}}

#' @rdname ppp_fast
#' @export
.VEC <- function(x) {if (base::length(x) == 0) {F} else if (base::is.vector(x)) {T} else if (!base::is.array(x)) {F} else {base::length(base::which(base::dim(x) > 1)) < 2}}

# defined D

#' @rdname ppp_fast
#' @export
.D0D <- function(x) {uj::null(x)}

#' @rdname ppp_fast
#' @export
.D1D <- function(x) {base::is.vector(x)}

#' @rdname ppp_fast
#' @export
.D2D <- function(x) {base::is.matrix(x) | base::is.data.frame(x)}

#' @rdname ppp_fast
#' @export
.DHD <- function(x) {if (is.array(x)) {base::length(base::dim(x)) > 2} else {F}}

# effective D

#' @rdname ppp_fast
#' @export
.EUD <- function(x) {base::length(x) == 0}

#' @rdname ppp_fast
#' @export
.E2D <- function(x) {base::length(base::which(base::dim(x) > 1)) == 2}

#' @rdname ppp_fast
#' @export
.EHD <- function(x) {base::length(base::which(base::dim(x) > 1)) > 2}

#' @rdname ppp_fast
#' @export
.E0D <- function(x) {if (base::is.atomic(x)) {base::length(x) == 1} else if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 1} else if (base::is.list(x)) {base::length(x) == 1} else {F}}

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
.NA0 <- function(x) {if (!base::is.atomic(x) | base::length(x) != 1) {F} else {base::is.na(x)}}

#' @rdname ppp_fast
#' @export
.OK0 <- function(x) {if (!base::is.atomic(x) | base::length(x) != 1) {F} else {!base::is.na(x)}}

#' @rdname ppp_fast
#' @export
.DUP <- function(x) {if (!base::is.atomic(x) | base::length(x) == 0) {F} else {base::length(x) != base::length(base::unique(x))}}

#' @rdname ppp_fast
#' @export
.UNQ <- function(x) {if (!base::is.atomic(x) | base::length(x) == 0) {F} else {base::length(x) == base::length(base::unique(x))}}

#' @rdname ppp_fast
#' @export
.CMP <- function(x) {if (!base::is.atomic(x) | base::length(x) == 0) {F} else {!base::any(base::is.na(x))}}

#' @rdname ppp_fast
#' @export
.MSS <- function(x) {if (!base::is.atomic(x) | base::length(x) == 0) {F} else {base::all(base::is.na(x))}}

#' @rdname ppp_fast
#' @export
.PRT <- function(x) {if (!base::is.atomic(x) | base::length(x) <= 1) {F} else {base::any(base::is.na(x)) & base::any(!base::is.na(x))}}

# xmode

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
.CHR <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.character(x)) {T}
      else {base::all(base::is.na(x))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
.FAC <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x)) {T}
      else {base::all(base::is.na(x))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.ORD <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.ordered(x)) {T}
      else {base::all(base::is.na(x))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.UNO <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x) & !base::is.ordered(x)) {T}
      else {base::all(base::is.na(x))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
.NST <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {!base::is.character(x) & !base::is.numeric(x) & !base::is.ordered(x)} else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.SRT <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {base::is.character(x) | base::is.numeric(x) | base::is.ordered(x)} else {T}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.LGL <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.logical(x)) {T} else {base::all(base::is.na(x))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NUM <- function(x) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.numeric(x)) {T} else {base::all(base::is.na(x))}
    } else {T}
  } else {F}
}

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
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

#' @rdname ppp_fast
#' @export
.COL <- function(x) {if (base::is.data.frame(x) | base::is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) == 1} else {F}}

#' @rdname ppp_fast
#' @export
.ROW <- function(x) {if (base::is.data.frame(x) | base::is.matrix(x)) {base::NROW(x) == 1 & base::NCOL(x) > 1} else {F}}

#' @rdname ppp_fast
#' @export
.PNT <- function(x) {if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 1} else {base::length(x) == 1}}

#' @rdname ppp_fast
#' @export
.SLD <- function(x) {if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) > 2} else {F}}

#' @rdname ppp_fast
#' @export
.SQR <- function(x) {if (base::is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) > 1 & base::NROW(x) == base::NCOL(x)} else {F}}

#' @rdname ppp_fast
#' @export
.EMP <- function(x) {if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 0} else if (base::length(x) == 0) {!base::is.null(x)} else {F}}

#' @rdname ppp_fast
#' @export
.RCT <- function(x) {if (base::length(x) > 1) {if (base::is.data.frame(x) | is.matrix(x)) {base::NROW(x) > 1 & base::NCOL(x) > 1} else {F}} else {F}}

#' @rdname ppp_fast
#' @export
.LIN <- function(x) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x)) {if (base::NROW(x) == 1 & base::NCOL(x) > 1) {T} else {base::NROW(x) > 1 & base::NCOL(x) == 1}}
    else if (base::is.array(x)) {base::length(base::which(base::dim(x) > 1)) == 2}
    else if (base::is.list(x)) {T}
    else if (base::is.vector(x)) {T}
  } else {F}
}

## .xxx_yyy ####

#' @rdname ppp_fast
#' @export
.atm_dtf <- function(x) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {if (!base::is.atomic(x[[i]])) {return(FALSE)}}
    TRUE
  } else {FALSE}
}

#' @rdname ppp_fast
#' @export
.atm_mat <- function(x) {base::is.atomic(x) & base::is.matrix(x)}

#' @rdname ppp_fast
#' @export
.atm_scl <- function(x) {base::is.atomic(x) & base::length(x) == 1}

#' @rdname ppp_fast
#' @export
.atm_vec <- function(x) {base::is.atomic(x) & uj:::.VEC(x)}

#' @rdname ppp_fast
#' @export
.atm_vls <- function(x) {if (base::is.list(x) & !base::is.data.frame(x) & base::length(x) > 0) {base::all(base::sapply(x, base::is.atomic))} else {F}}

#' @rdname ppp_fast
#' @export
.chr_arr <- function(x) {base::is.array(x) & base::is.character(x)}

#' @rdname ppp_fast
#' @export
.chr_dtf <- function(x) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {if (!base::is.character(x[[i]])) {return(FALSE)}}
    TRUE
  } else {FALSE}
}

#' @rdname ppp_fast
#' @export
.chr_vec <- function(x) {uj:::.VEC(x) & base::is.character(x)}

#' @rdname ppp_fast
#' @export
.chr_vls <- function(x) {uj::f0(base::is.list(x) & !is.data.frame(x), base::all(base::sapply(x, base::is.charsacter)), F)}

#' @rdname ppp_fast
#' @export
.cmp_atm <- function(x) {if (base::is.atomic(x) & base::length(x) > 0) {!base::any(base::is.na(x))} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_chr <- function(x) {if (base::is.character(x) & base::length(x) > 0) {!base::any(base::is.na(x))} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_num <- function(x) {if (base::is.numeric(x) & base::length(x) > 0) {!base::any(base::is.na(x))} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_scl <- function(x) {if (base::is.atomic(x) & base::length(x) == 1) {!base::is.na(x)} else {F}}

#' @rdname ppp_fast
#' @export
.cmp_vec <- function(x) {if (!base::is.atomic(x) | base::length(x) == 0) {F} else if (!uj:::.VEC(x)) {F} else {!base::any(base::is.na(x))}}

#' @rdname ppp_fast
#' @export
.lgl_scl <- function(x) {base::logical(x) & base::length(x) == 1}

#' @rdname ppp_fast
#' @export
.nnw_vec <- function(x) {
  if (uj:::.VEC(x)) {
    if (base::is.numeric(x)) {
      x <- x[!base::is.na(x)]
      if (base::length(x) > 0) {
        if (base::all(x >= 0)) {base::all(x == base::round(x))}
        else {F}
      } else {T}
    } else {F}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.pop_atm <- function(x) {base::is.atomic(x) & base::length(x) > 0}

#' @rdname ppp_fast
#' @export
.pop_chr <- function(x) {base::length(x) > 0 & base::is.character(x)}

#' @rdname ppp_fast
#' @export
.pop_dtf <- function(x) {base::is.data.frame(x) & base::NROW(x) > 0 & base::NCOL(x) > 0}

#' @rdname ppp_fast
#' @export
.pop_mat <- function(x) {base::is.matrix(x) & base::length(x) > 0}

#' @rdname ppp_fast
#' @export
.pop_srt <- function(x) {base::length(x) > 0 & (base::is.character(x) | base::is.numeric(x) | base::is.ordered(x))}

#' @rdname ppp_fast
#' @export
.pop_vec <- function(x) {uj:::.VEC(x) & base::length(x) > 0}

#' @rdname ppp_fast
#' @export
.pop_vls <- function(x) {uj:::.VLS(x) & base::length(x) > 0}

## .xxx_yyy_zzz ####

#' @rdname ppp_fast
#' @export
.cmp_atm_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.atomic(x)) {F} else if (base::any(base::is.na(x))) {F}else {T}}

#' @rdname ppp_fast
#' @export
.cmp_ch1_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {base::nchar(x) == 1}}

#' @rdname ppp_fast
#' @export
.cmp_ch1_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.character(x)) {F} else if (base::any(base::is.na(x))) {F} else {base::all(base::nchar(x) == 1)}}

#' @rdname ppp_fast
#' @export
.cmp_ch3_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {base::nchar(x) == 3}}

#' @rdname ppp_fast
#' @export
.cmp_ch3_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {base::all(base::nchar(x) == 3)}}

#' @rdname ppp_fast
#' @export
.cmp_chr_arr <- function(x, .VALID = NULL) {
  if      (base::length(x) == 0     ) {F}
  else if (!uj:::.ARR(x)            ) {F}
  else if (!base::is.character(x)   ) {F}
  else if (base::any(base::is.na(x))) {F}
  else if (base::is.null(.VALID)     ) {T}
  else {base::all(x %in% .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_vec <- function(x, .VALID = NULL) {if (!uj:::.VEC(x)) {F} else if (!base::is.character(x)) {F} else if (base::any(base::is.na(x))) {F} else if (!base::is.null(.VALID)) {base::all(x %in% .VALID)} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_chr_vls <- function(x, .VALID = NULL) {if (!uj:::.chr_vls(x)) {F} else if (base::any(base::is.na(uj::av(x)))) {F} else if (base::is.null(.VALID)) {T} else {base::all(uj::av(x) %in% .VALID)}}

#' @rdname ppp_fast
#' @export
.cmp_chr_gen <- function(x, .VALID = NULL) {if (uj::.cmp_chr_vec(x, .VALID = .VALID)) {T} else if (uj::.cmp_chr_arr(x, .VALID = .VALID)) {T} else {uj::.cmp_chr_vls(x, .VALID = .VALID)}}

#' @rdname ppp_fast
#' @export
.cmp_chr_scl <- function(x, .VALID = NULL) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else if (!base::is.null(.VALID)) {x %in% .VALID} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_clr_vec <- function(x) {uj:::.cmp_vec(x) & uj:::.CLR(x)}

#' @rdname ppp_fast
#' @export
.cmp_lgl_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.logical(x)) {F} else {!base::is.na(x)}}

#' @rdname ppp_fast
#' @export
.cmp_lgl_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.logical(x)) {F} else {!base::any(base::is.na(x))}}

#' @rdname ppp_fast
#' @export
.cmp_nng_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else {x < 0}}

#' @rdname ppp_fast
#' @export
.cmp_nnw_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else if (x < 0) {F} else {x == round(x)}}

#' @rdname ppp_fast
#' @export
.cmp_nnw_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else if (base::any(x < 0)) {F} else {base::all(x == round(x))}}

#' @rdname ppp_fast
#' @export
.cmp_num_scl <- function(x, .VALID = NULL) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else if (!base::is.null(.VALID)) {x %in% .VALID} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_num_vec <- function(x, .VALID = NULL) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else if (!base::is.null(.VALID)) {base::all(x %in% .VALID)} else {T}}

#' @rdname ppp_fast
#' @export
.cmp_pos_vec <- function(x) {if (base::length(x) == 0) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else {base::all(x >= 0 & x <= 1)}}

#' @rdname ppp_fast
#' @export
.cmp_ppn_vec <- function(x) {if (base::length(x) == 0) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else {base::all(x >= 0 & x <= 1)}}

#' @rdname ppp_fast
#' @export
.cmp_psw_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::is.na(x)) {F} else if (x <= 0) {F} else {x == round(x)}}

#' @rdname ppp_fast
#' @export
.cmp_psw_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else if (base::any(x <= 0)) {F} else {base::all(x == round(x))}}

#' @rdname ppp_fast
#' @export
.cmp_srt_vec <- function(x) {if (!uj:::.VEC(x)) {F} else if (!base::is.numeric(x) & !base::is.character(x) & !base::is.ordered(x)) {F} else {base::any(base::is.na(x))}}

#' @rdname ppp_fast
#' @export
.cmp_str_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (base::is.na(x)) {F} else {!base::any(x == "")}}

#' @rdname ppp_fast
#' @export
.cmp_str_vec <- function(x) {if (base::length(x) == 0) {F} else if (!base::is.character(x)) {F} else if (base::any(base::is.na(x))) {F} else {!base::any(x == "")}}

#' @rdname ppp_fast
#' @export
.cmp_whl_scl <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.numeric(x)) {F} else if (base::any(base::is.na(x))) {F} else {x == base::round(x)}}

#' @rdname ppp_fast
#' @export
.cmp_atm_mvc <- function(x) {if (!uj:::.MVC(x)) {F} else if (!base::is.atomic(x)) {F} else if (base::any(base::is.na(x))) {F} else {T}}

#' @rdname ppp_fast
#' @export
.unq_atm_mvc <- function(x) {uj::f0(uj:::.cmp_atm_mvc(x), base::length(x) == base::length(base::unique(x)), F)}

#' @rdname ppp_fast
#' @export
.unq_atm_vec <- function(x) {uj::f0(uj:::.cmp_atm_vec(x), base::length(x) == base::length(base::unique(x)), F)}

#' @rdname ppp_fast
#' @export
.unq_ch1_vec <- function(x) {uj::f0(uj:::.cmp_ch1_vec(x), base::length(x) == base::length(base::unique(x)), F)}

#' @rdname ppp_fast
#' @export
.unq_ch3_vec <- function(x) {uj::f0(uj:::.cmp_ch3_vec(x), base::length(x) == base::length(base::unique(x)), F)}

#' @rdname ppp_fast
#' @export
.unq_chr_vec <- function(x, .VALID = NULL) {uj::f0(uj:::.cmp_chr_vec(x), base::length(x) == base::length(base::unique(x)), uj::f0(base::is.null(.VALID), T, base::all(x %in% .VALID)))}

#' @rdname ppp_fast
#' @export
.unq_nnw_vec <- function(x) {uj::f0(uj:::.cmp_nnw_vec(x), base::length(x) == base::length(base::unique(x)), F)}

#' @rdname ppp_fast
#' @export
.unq_num_vec <- function(x, .VALID = NULL) {uj::f0(uj:::.cmp_num_vec(x), base::length(x) == base::length(base::unique(x)), uj::f0(base::is.null(.VALID), T, base::all(x %in% .VALID)))}

#' @rdname ppp_fast
#' @export
.unq_psw_vec <- function(x) {uj::f0(uj:::.cmp_psw_vec(x), base::length(x) == base::length(base::unique(x)), F)}

#' @rdname ppp_fast
#' @export
.unq_str_vec <- function(x) {uj::f0(uj:::.cmp_str_vec(x), base::length(x) == base::length(base::unique(x)), F)}

#' @rdname ppp_fast
#' @export
.atm_rct_dtf <- function(x) {
  if (!uj::.DTF(x)) {return(FALSE)}
  if (uj::nr(x) < 2 | uj::nc(x) < 2) {return(FALSE)}
  for (i in 1:uj::nc(x)) {if (!uj::.ATM(x[[i]])) {return(FALSE)}}
  TRUE
}

