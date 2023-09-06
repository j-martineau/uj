#' @family properties
#' @title Fast property checking for common checks
#' @description Fast property checking for \code{\link{ppp}} functions without the dot prefix.
#' @details When `.VALID` is supplied, both `x` and `.VALID` are \link[uj:av]{atomized} before checking whether all atomic elements of `x` are contained in the atomic elements of `.VALID`.
#' @param x Any object. See *details*.
#' @param .VALID An optional object containing valid atomic values to check against atomic values in `x`. See *details*.
#' @return `TRUE` or `FALSE`.
ppp_fast <- function() {utils::help("ppp_fast", "uj")}

# xxx ####

#' @rdname ppp_fast
#' @export
.ATM <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.atomic(x) & !base::is.null(x), .VALID)}

#' @rdname ppp_fast
#' @export
.DEF <- function(x, .VALID = NULL) {uj:::.check_VALID(x, !base::is.null(x), .VALID)}

#' @rdname ppp_fast
#' @export
.FUN <- function(x, .VALID = NULL) {
  if (!base::is.function(x)) {
    if (base::length(x) == 1 & base::is.character(x)) {
      new.x <- tryCatch(base::match.fun(x), error = function(x) e, finally = NULL)
      uj:::.check_VALID(x, !rlang::is_error(new.x), .VALID)
    } else {F}
  } else {uj:::.check_VALID(x, T, .VALID)}
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
.POP <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::length(x) > 0, .VALID)}

#' @rdname ppp_fast
#' @export
.RCR <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.recursive(x), .VALID)}

# xclass

#' @rdname ppp_fast
#' @export
.ARR <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.array(x), .VALID)}

#' @rdname ppp_fast
#' @export
.DTF <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.data.frame(x), .VALID)}

#' @rdname ppp_fast
#' @export
.GEN <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.array(x) | base::is.vector(x), .VALID)}

#' @rdname ppp_fast
#' @export
.MAT <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.matrix(x), .VALID)}

#' @rdname ppp_fast
#' @export
.MVC <- function(x, .VALID = NULL) {
  if (base::length(x) < 2) {F}
  else if (base::is.vector(x)) {uj:::.check_VALID(x, T, .VALID)}
  else if (!base::is.array(x)) {F}
  else {uj:::.check_VALID(x, base::length(base::which(base::dim(x) > 1)) == 1, .VALID)}
}

#' @rdname ppp_fast
#' @export
.SCL <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else {uj:::.check_VALID(x, base::is.vector(x) | base::is.array(x), .VALID)}
}

#' @rdname ppp_fast
#' @export
.VLS <- function(x, .VALID = NULL) {
  if (base::is.data.frame(x)) {F}
  else {uj:::.check_VALID(x, base::is.list(x), .VALID)}
}

#' @rdname ppp_fast
#' @export
.VEC <- function(x, .VALID = NULL) {
  if (base::length(x) == 0) {F}
  else if (base::is.vector(x)) {uj:::.check_VALID(x, T, .VALID)}
  else if (!base::is.array(x)) {F}
  else {uj:::.check_VALID(x, base::length(base::which(base::dim(x) > 1)) < 2, .VALID)}
}

# defined D

#' @rdname ppp_fast
#' @export
.D0D <- function(x) {uj::null(x)}

#' @rdname ppp_fast
#' @export
.D1D <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.vector(x), .VALID)}

#' @rdname ppp_fast
#' @export
.D2D <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.matrix(x) | base::is.data.frame(x), .VALID)}

#' @rdname ppp_fast
#' @export
.DHD <- function(x, .VALID = NULL) {
  if (is.array(x)) {uj:::.check_VALID(x, base::length(base::dim(x)) > 2, .VALID)}
  else {F}
}

# effective D

#' @rdname ppp_fast
#' @export
.EUD <- function(x) {base::length(x) == 0}

#' @rdname ppp_fast
#' @export
.E2D <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::length(base::which(base::dim(x) > 1)) == 2, .VALID)}

#' @rdname ppp_fast
#' @export
.EHD <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::length(base::which(base::dim(x) > 1)) > 2, .VALID)}

#' @rdname ppp_fast
#' @export
.E0D <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {uj:::.check_VALID(x, base::length(x) == 1, .VALID)}
  else if (base::is.data.frame(x)) {uj:::.check_VALID(x, base::NROW(x) * base::NCOL(x) == 1, .VALID)}
  else if (base::is.list(x)) {uj:::.check_VALID(x, base::length(x) == 1, .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.E1D <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::is.vector(x)) {uj:::.check_VALID(x, base::length(x) > 1, .VALID)}
    else if (base::is.array(x)) {uj:::.check_VALID(x, base::length(base::which(base::dim(x) > 1)) == 1, .VALID)}
    else {F}
  } else if (base::is.data.frame(x)) {
    if (base::NROW(x) == 1) {uj:::.check_VALID(x, base::NCOL(x) > 1, .VALID)}
    else if (base::NCOL(x) == 1) {uj:::.check_VALID(x, base::NROW(x) > 1, .VALID)}
    else {F}
  } else if (base::is.list(x)) {uj:::.check_VALID(x, base::length(x) > 1, .VALID)}
  else {F}
}

# integrity

#' @rdname ppp_fast
#' @export
.NA0 <- function(x) {
  if (!base::is.atomic(x) | base::length(x) != 1) {F}
  else {base::is.na(x)}
}

#' @rdname ppp_fast
#' @export
.OK0 <- function(x, .VALID = NULL) {
  if (!base::is.atomic(x) | base::length(x) != 1) {F}
  else {uj:::.check_VALID(x, !base::is.na(x), .VALID)}
}

#' @rdname ppp_fast
#' @export
.DUP <- function(x, .VALID = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {uj:::.check_VALID(x, base::length(x) != base::length(base::unique(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.UNQ <- function(x, .VALID = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.CMP <- function(x, .VALID = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {uj:::.check_VALID(x, !base::any(base::is.na(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.MSS <- function(x) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {base::all(base::is.na(x))}
}

#' @rdname ppp_fast
#' @export
.PRT <- function(x, .VALID = NULL) {
  if (!base::is.atomic(x) | base::length(x) <= 1) {F}
  else if (!any(base::is.na(x))) {F}
  else if (all(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, x[!base::is.na(x)], .VALID)}
}

# xmode

#' @rdname ppp_fast
#' @export
.CH1 <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {uj:::.check_VALID(x, T, .VALID)}
          else {uj:::.check_VALID(x, base::all(base::nchar(x) == 1), .VALID)}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CH3 <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {uj:::.check_VALID(x, T, .VALID)}
          else {uj:::.check_VALID(x, base::all(base::nchar(x) == 3), .VALID)}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CHR <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.character(x)) {uj:::.check_VALID(x, T, .VALID)}
      else {uj:::.check_VALID(x, base::all(base::is.na(x)), .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.STR <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {uj:::.check_VALID(x, T, .VALID)}
          else {uj:::.check_VALID(x, base::all(base::nchar(x) > 0), .VALID)}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CLR <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        x <- x[!base::is.na(x)]
        if (base::length(x) > 0) {uj:::.check_VALID(x, !uj::is_err(grDevices::col2rgb(x)), .VALID)}
        else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.FAC <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x)) {uj:::.check_VALID(x, T, .VALID)}
      else {uj:::.check_VALID(x, base::all(base::is.na(x)), .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.ORD <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.ordered(x)) {uj:::.check_VALID(x, T, .VALID)}
      else {uj:::.check_VALID(x, base::all(base::is.na(x)), .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.UNO <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x) & !base::is.ordered(x)) {uj:::.check_VALID(x, T, .VALID)}
      else {uj:::.check_VALID(x, base::all(base::is.na(x)), .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.IND <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        x <- x[!base::is.na(x)]
        if (base::is.numeric(x)) {uj:::.check_VALID(x, base::all(x == base::round(x)), .VALID)}
        else {uj:::.check_VALID(x, base::is.logical(x), .VALID)}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NST <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {uj:::.check_VALID(x, !base::is.character(x) & !base::is.numeric(x) & !base::is.ordered(x), .VALID)}
      else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.SRT <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {uj:::.check_VALID(x, base::is.character(x) | base::is.numeric(x) | base::is.ordered(x), .VALID)}
      else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.LGL <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.logical(x)) {uj:::.check_VALID(x, T, .VALID)}
      else {uj:::.check_VALID(x, base::all(base::is.na(x)), .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NUM <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.numeric(x)) {uj:::.check_VALID(x, T, .VALID)}
      else {uj:::.check_VALID(x, base::all(base::is.na(x)), .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.WHL <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::all(x == base::round(x), .VALID))
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.ODD <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {
            x <- x / 2
            uj:::.check_VALID(x, base::any(x != base::round(x)), .VALID)
          } else {F}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.EVN <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {
            x <- x / 2
            uj:::.check_VALID(x, base::all(x == base::round(x)), .VALID)
          } else {F}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NGW <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj:::.check_VALID(x, base::all(x < 0), .VALID)}
          else {F}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

.NNW <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj:::.check_VALID(x, base::all(x >= 0), .VALID)}
          else {F}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NPW <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj:::.check_VALID(x, base::all(x <= 0), .VALID)}
          else {F}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PSW <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj:::.check_VALID(x, base::all(x > 0), .VALID)}
          else {F}
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.FRC <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::any(x != base::round(x)), .VALID)
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NEG <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::all(x < 0), .VALID)
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.POS <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::all(x > 0), .VALID)
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NNW <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::all(x >= 0), .VALID)
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NPS <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::all(x <= 0), .VALID)
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PCT <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::all(x >= 0 | x <= 100), .VALID)
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PPN <- function(x, .VALID = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj:::.check_VALID(x, base::all(x >= 0 | x <= 1), .VALID)
        } else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

# shape

#' @rdname ppp_fast
#' @export
.COL <- function(x, .VALID = NULL) {
  if (base::is.data.frame(x) | base::is.matrix(x)) {uj:::.check_VALID(x, base::NROW(x) > 1 & base::NCOL(x) == 1, .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.ROW <- function(x, .VALID = NULL) {
  if (base::is.data.frame(x) | base::is.matrix(x)) {uj:::.check_VALID(x, base::NROW(x) == 1 & base::NCOL(x) > 1, .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.PNT <- function(x, .VALID = NULL) {
  if (base::is.data.frame(x)) {uj:::.check_VALID(x, base::NROW(x) * base::NCOL(x) == 1, .VALID)}
  else {uj:::.check_VALID(x, base::length(x) == 1, .VALID)}
}

#' @rdname ppp_fast
#' @export
.SLD <- function(x, .VALID = NULL) {
  if (base::is.array(x)) {uj:::.check_VALID(x, base::length(base::which(base::dim(x) > 1)) > 2, .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.SQR <- function(x, .VALID = NULL) {
  if (base::is.matrix(x)) {uj:::.check_VALID(x, base::NROW(x) > 1 & base::NCOL(x) > 1 & base::NROW(x) == base::NCOL(x), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.EMP <- function(x, .VALID = NULL) {
  if (base::is.data.frame(x)) {uj:::.check_VALID(x, base::NROW(x) * base::NCOL(x) == 0, .VALID)}
  else if (base::length(x) == 0) {uj:::.check_VALID(x, !base::is.null(x), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.RCT <- function(x, .VALID = NULL) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x) | is.matrix(x)) {uj:::.check_VALID(x, base::NROW(x) > 1 & base::NCOL(x) > 1, .VALID)}
    else {F}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.LIN <- function(x, .VALID = NULL) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x)) {
      if (base::NROW(x) == 1 & base::NCOL(x) > 1) {uj:::.check_VALID(x, T, .VALID)}
      else {uj:::.check_VALID(x, base::NROW(x) > 1 & base::NCOL(x) == 1, .VALID)}
    } else if (base::is.array(x)) {uj:::.check_VALID(x, base::length(base::which(base::dim(x) > 1)) == 2, .VALID)}
    else if (base::is.list(x)) {uj:::.check_VALID(x, T, .VALID)}
    else if (base::is.vector(x)) {uj:::.check_VALID(x, T, .VALID)}
  } else {F}
}

## .xxx_yyy ####

#' @rdname ppp_fast
#' @export
.atm_dtf <- function(x, .VALID = NULL) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {
      if (!base::is.atomic(x[[i]])) {return(F)}
    }
    uj:::.check_VALID(x, T, .VALID)
  } else {F}
}

#' @rdname ppp_fast
#' @export
.atm_mat <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.atomic(x) & base::is.matrix(x), .VALID)}

#' @rdname ppp_fast
#' @export
.atm_scl <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.atomic(x) & base::length(x) == 1, .VALID)}

#' @rdname ppp_fast
#' @export
.atm_vec <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.atomic(x) & uj:::.VEC(x), .VALID)}

#' @rdname ppp_fast
#' @export
.atm_vls <- function(x, .VALID = NULL) {
  if (base::is.list(x) & !base::is.data.frame(x) & base::length(x) > 0) {uj:::.check_VALID(x, base::all(base::sapply(x, base::is.atomic)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.chr_arr <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.array(x) & base::is.character(x), .VALID)}

#' @rdname ppp_fast
#' @export
.chr_dtf <- function(x, .VALID = NULL) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {
      if (!base::is.character(x[[i]])) {return(F)}
    }
    uj:::.check_VALID(x, T, .VALID)
  } else {F}
}

#' @rdname ppp_fast
#' @export
.chr_vec <- function(x, .VALID = NULL) {uj:::.check_VALID(x, uj:::.VEC(x) & base::is.character(x), .VALID)}

#' @rdname ppp_fast
#' @export
.chr_vls <- function(x, .VALID = NULL) {
  if (base::is.list(x) & !is.data.frame(x)) {uj:::.check_VALID(x, base::all(base::sapply(x, base::is.character)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_atm <- function(x, .VALID = NULL) {
  if (base::is.atomic(x) & base::length(x) > 0) {uj:::.check_VALID(x, !base::any(base::is.na(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_chr <- function(x, .VALID = NULL) {
  if (base::is.character(x) & base::length(x) > 0) {uj:::.check_VALID(x, !base::any(base::is.na(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_num <- function(x, .VALID = NULL) {
  if (base::is.numeric(x) & base::length(x) > 0) {uj:::.check_VALID(x, !base::any(base::is.na(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_scl <- function(x, .VALID = NULL) {
  if (base::is.atomic(x) & base::length(x) == 1) {uj:::.check_VALID(x, !base::is.na(x), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_vec <- function(x, .VALID = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else if (!uj:::.VEC(x)) {F}
  else {uj:::.check_VALID(x, !base::any(base::is.na(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.lgl_scl <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::logical(x) & base::length(x) == 1, .VALID)}

#' @rdname ppp_fast
#' @export
.nnw_vec <- function(x, .VALID = NULL) {
  if (uj:::.VEC(x)) {
    if (base::is.numeric(x)) {
      x <- x[!base::is.na(x)]
      if (base::length(x) > 0) {
        if (base::all(x >= 0)) {uj:::.check_VALID(x, base::all(x == base::round(x)), .VALID)}
        else {F}
      } else {uj:::.check_VALID(x, T, .VALID)}
    } else {F}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.pop_atm <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.atomic(x) & base::length(x) > 0, .VALID)}

#' @rdname ppp_fast
#' @export
.pop_chr <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::length(x) > 0 & base::is.character(x), .VALID)}

#' @rdname ppp_fast
#' @export
.pop_dtf <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.data.frame(x) & base::NROW(x) > 0 & base::NCOL(x) > 0, .VALID)}

#' @rdname ppp_fast
#' @export
.pop_mat <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::is.matrix(x) & base::length(x) > 0, .VALID)}

#' @rdname ppp_fast
#' @export
.pop_srt <- function(x, .VALID = NULL) {uj:::.check_VALID(x, base::length(x) > 0 & (base::is.character(x) | base::is.numeric(x) | base::is.ordered(x)), .VALID)}

#' @rdname ppp_fast
#' @export
.pop_vec <- function(x, .VALID = NULL) {uj:::.check_VALID(x, uj:::.VEC(x) & base::length(x) > 0, .VALID)}

#' @rdname ppp_fast
#' @export
.pop_vls <- function(x, .VALID = NULL) {uj:::.check_VALID(x, uj:::.VLS(x) & base::length(x) > 0, .VALID)}

## .xxx_yyy_zzz ####

#' @rdname ppp_fast
#' @export
.cmp_atm_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.atomic(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, T, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch1_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj:::.check_VALID(x, base::nchar(x) == 1, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch1_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, base::all(base::nchar(x) == 1), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch3_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj:::.check_VALID(x, base::nchar(x) == 3, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch3_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj:::.check_VALID(x, base::all(base::nchar(x) == 3), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_arr <- function(x, .VALID = NULL) {
  if      (base::length(x) == 0     ) {F}
  else if (!uj:::.ARR(x)            ) {F}
  else if (!base::is.character(x)   ) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, T, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, T, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_vls <- function(x, .VALID = NULL) {
  if (!uj:::.chr_vls(x)) {F}
  else if (base::any(base::is.na(uj::av(x)))) {F}
  else {uj:::.check_VALID(x, T, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_gen <- function(x, .VALID = NULL) {
  if (uj::.cmp_chr_vec(x, .VALID = .VALID)) {T}
  else if (uj::.cmp_chr_arr(x, .VALID = .VALID)) {T}
  else {uj::.cmp_chr_vls(x, .VALID = .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj:::.check_VALID(x, !base::is.null(.VALID), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_clr_vec <- function(x, .VALID = NULL) {uj:::.check_VALID(x, uj:::.cmp_vec(x) & uj:::.CLR(x), .VALID)}

#' @rdname ppp_fast
#' @export
.cmp_lgl_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.logical(x)) {F}
  else {uj:::.check_VALID(x, !base::is.na(x), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_lgl_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.logical(x)) {F}
  else {uj:::.check_VALID(x, !base::any(base::is.na(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_nng_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj:::.check_VALID(x, x < 0, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_nnw_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else if (x < 0) {F}
  else {uj:::.check_VALID(x, x == round(x), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_nnw_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else if (base::any(x < 0)) {F}
  else {uj:::.check_VALID(x, base::all(x == round(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_num_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj:::.check_VALID(x, T, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_num_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, T, .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_pos_vec <- function(x, .VALID = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, base::all(x >= 0 & x <= 1), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_ppn_vec <- function(x, .VALID = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, base::all(x >= 0 & x <= 1), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_psw_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else if (x <= 0) {F}
  else {uj:::.check_VALID(x, x == round(x), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_psw_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else if (base::any(x <= 0)) {F}
  else {uj:::.check_VALID(x, base::all(x == round(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_srt_vec <- function(x, .VALID = NULL) {
  if (!uj:::.VEC(x)) {F}
  else if (!base::is.numeric(x) & !base::is.character(x) & !base::is.ordered(x)) {F}
  else {uj:::.check_VALID(x, base::any(base::is.na(x)), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_str_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj:::.check_VALID(x, !base::any(x == ""), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_str_vec <- function(x, .VALID = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, !base::any(x == ""), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_whl_scl <- function(x, .VALID = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, x == base::round(x), .VALID)}
}

#' @rdname ppp_fast
#' @export
.cmp_atm_mvc <- function(x, .VALID = NULL) {
  if (!uj:::.MVC(x)) {F}
  else if (!base::is.atomic(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj:::.check_VALID(x, T, .VALID)}
}

#' @rdname ppp_fast
#' @export
.unq_atm_mvc <- function(x, .VALID = NULL) {
  if (uj:::.cmp_atm_mvc(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_atm_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_atm_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_ch1_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_ch1_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_ch3_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_ch3_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_chr_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_chr_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_nnw_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_nnw_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_num_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_num_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_psw_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_psw_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_str_vec <- function(x, .VALID = NULL) {
  if (uj:::.cmp_str_vec(x)) {uj:::.check_VALID(x, base::length(x) == base::length(base::unique(x)), .VALID)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.atm_rct_dtf <- function(x, .VALID = NULL) {
  if (!uj::.DTF(x)) {return(F)}
  if (uj::nr(x) < 2 | uj::nc(x) < 2) {return(F)}
  for (i in 1:uj::nc(x)) {
    if (!uj::.ATM(x[[i]])) {return(F)}
  }
  uj:::.check_VALID(x, T, .VALID)
}
