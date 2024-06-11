#' @title Fast Property Checking for Common Checks with Optional valid Values
#' @family properties
#' @description Fast property checking for \code{\link{ppp}} functions without the dot prefix (e.g., `.ATM(x)` is a faster version of `ATM(x)` because it uses only base functions to conduct property checking).
#' @details When `valid` is supplied, both `x` and `valid` are \link[=av]{atomized} before checking whether all atomic elements of `x` are contained in the atomic elements of `valid`.
#' @param x Any object. See *details*.
#' @param valid An optional object containing valid atomic values to check against atomic values in `x`. See *details*.
#' @return A logical scalar.
#' @export
ppp_fast_help <- function() {utils::help("ppp_fast_help", "uj")}

# .xxx ####

#' @describeIn ppp_fast_help Checks for atomic-ness subject to any value restrictions in `valid`.
#' @export
.ATM <- function(x, valid = NULL) {uj::check_valid(x, base::is.atomic(x) & !base::is.null(x), valid)}

#' @describeIn ppp_fast_help Checks for defined-ness (non-`NULL`-ness) subject to any value restrictions in `valid`.
#' @export
.DEF <- function(x, valid = NULL) {uj::check_valid(x, !base::is.null(x), valid)}

#' @describeIn ppp_fast_help Checks for function-ness (function object or function name) subject to any value restrictions in `valid`.
#' @export
.FUN <- function(x, valid = NULL) {
  if (!base::is.function(x)) {
    if (base::length(x) == 1 & base::is.character(x)) {
      newX <- tryCatch(base::match.fun(x), error = function(x) e, finally = NULL)
      uj::check_valid(x, !rlang::is_error(newX), valid)
    } else {F}
  } else {
    for (valid in valid) {if (base::is.function(valid)) {if (base::identical(x, valid)) {return(T)}}}
    F
  }
}

## .bbb ####

#' @describeIn ppp_fast_help Checks for nil-ness (length 0-ness).
#' @export
.NIL <- function(x) {base::length(x) == 0}

#' @describeIn ppp_fast_help Checks for NULL-ness.
#' @export
.NLL <- function(x) {base::is.null(x)}

#' @describeIn ppp_fast_help Checks for populated-ness subject to any value restrictions in `valid`.
#' @export
.POP <- function(x, valid = NULL) {uj::check_valid(x, base::length(x) > 0, valid)}

#' @describeIn ppp_fast_help Checks for recursiveness.
#' @export
.RCR <- function(x) {base::is.recursive(x)}

## .ccc ####

#' @describeIn ppp_fast_help Checks for array-ness subject to any value restrictions in `valid`.
#' @export
.ARR <- function(x, valid = NULL) {uj::check_valid(x, base::is.array(x), valid)}

#' @describeIn ppp_fast_help Checks for data.frame-ness subject to any value restrictions in `valid`.
#' @export
.DTF <- function(x) {base::is.data.frame(x)}

#' @describeIn ppp_fast_help Checks for generic-ness (array-ness or vector-ness).
#' @export
.GEN <- function(x) {base::is.array(x) | base::is.vector(x)}

#' @describeIn ppp_fast_help Checks for matrix-ness subject to any value restrictions in `valid`.
#' @export
.MAT <- function(x, valid = NULL) {uj::check_valid(x, base::is.matrix(x), valid)}

#' @describeIn ppp_fast_help Checks for multivec-ness subject to any value restrictions in `valid`.
#' @export
.MVC <- function(x, valid = NULL) {
  if (base::length(x) < 2) {F}
  else if (base::is.vector(x)) {uj::check_valid(x, T, valid)}
  else if (!base::is.array(x)) {F}
  else {uj::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 1, valid)}
}

#' @describeIn ppp_fast_help Checks for scalar-ness subject to any value restrictions in `valid`.
#' @export
.SCL <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else {uj::check_valid(x, base::is.vector(x) | base::is.array(x), valid)}
}

#' @describeIn ppp_fast_help Checks for vector-list-ness.
#' @export
.VLS <- function(x) {
  if (base::is.data.frame(x)) {F}
  else {base::is.list(x)}
}

#' @describeIn ppp_fast_help Checks for vec-ness (scalar or vector) subject to any value restrictions in `valid`.
#' @export
.VEC <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (base::is.vector(x)) {uj::check_valid(x, T, valid)}
  else if (!base::is.array(x)) {F}
  else {uj::check_valid(x, base::length(base::which(base::dim(x) > 1)) < 2, valid)}
}

## .ddd ####

#' @describeIn ppp_fast_help Checks for zero defined dimensions.
#' @export
.D0D <- function(x) {base::is.null(x)}

#' @describeIn ppp_fast_help Checks for a single defined dimension subject to any value restrictions in `valid`.
#' @export
.D1D <- function(x, valid = NULL) {uj::check_valid(x, base::is.vector(x), valid)}

#' @describeIn ppp_fast_help Checks for two defined dimensions subject to any value restrictions in `valid`.
#' @export
.D2D <- function(x, valid = NULL) {uj::check_valid(x, base::is.matrix(x) | base::is.data.frame(x), valid)}

#' @describeIn ppp_fast_help Checks for hyper-defined dimension-ness subject to any value restrictions in `valid`.
#' @export
.DHD <- function(x, valid = NULL) {
  if (is.array(x)) {uj::check_valid(x, base::length(base::dim(x)) > 2, valid)}
  else {F}
}

## .eee ####

#' @describeIn ppp_fast_help Checks for undefined effective dimensionality.
#' @export
.EUD <- function(x) {base::length(x) == 0}

#' @describeIn ppp_fast_help Checks for effective bi-dimensionality subject to any value restrictions in `valid`.
#' @export
.E2D <- function(x, valid = NULL) {uj::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 2, valid)}

#' @describeIn ppp_fast_help Checks for effective hyper-dimensionality subject to any value restrictions in `valid`.
#' @export
.EHD <- function(x, valid = NULL) {uj::check_valid(x, base::length(base::which(base::dim(x) > 1)) > 2, valid)}

#' @describeIn ppp_fast_help Checks for effective zero-dimensionality subject to any value restrictions in `valid`.
#' @export
.E0D <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {uj::check_valid(x, base::length(x) == 1, valid)}
  else if (base::is.data.frame(x)) {uj::check_valid(x, base::NROW(x) * base::NCOL(x) == 1, valid)}
  else if (base::is.list(x)) {uj::check_valid(x, base::length(x) == 1, valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for effective unidimensionality subject to any value restrictions in `valid`.
#' @export
.E1D <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::is.vector(x)) {uj::check_valid(x, base::length(x) > 1, valid)}
    else if (base::is.array(x)) {uj::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 1, valid)}
    else {F}
  } else if (base::is.data.frame(x)) {
    if (base::NROW(x) == 1) {uj::check_valid(x, base::NCOL(x) > 1, valid)}
    else if (base::NCOL(x) == 1) {uj::check_valid(x, base::NROW(x) > 1, valid)}
    else {F}
  } else if (base::is.list(x)) {uj::check_valid(x, base::length(x) > 1, valid)}
  else {F}
}

## .iii ####

#' @describeIn ppp_fast_help Checks for scalar `NA`-ness.
#' @export
.NA0 <- function(x) {
  if (!base::is.atomic(x) | base::length(x) != 1) {F}
  else {base::is.na(x)}
}

#' @describeIn ppp_fast_help Checks for atomic, non-`NA` scalar-ness subject to any value restrictions in `valid`.
#' @export
.OK0 <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) != 1) {F}
  else {uj::check_valid(x, !base::is.na(x), valid)}
}

#' @describeIn ppp_fast_help Checks for atomic duplicated-ness subject to any value restrictions in `valid`.
#' @export
.DUP <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {uj::check_valid(x, base::length(x) != base::length(base::unique(x)), valid)}
}

#' @describeIn ppp_fast_help Checks for atomic uniqueness subject to any value restrictions in `valid`.
#' @export
.UNQ <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
}

#' @describeIn ppp_fast_help Checks for atomic completeness (non-`NA`-ness) subject to any value restrictions in `valid`.
#' @export
.CMP <- function(x, valid = NULL) {
  if (base::is.null(x)) {F}
  else if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for atomic fully-missing-ness.
#' @export
.MSS <- function(x) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {base::all(base::is.na(x))}
}

#' @describeIn ppp_fast_help Checks for atomic partial completeness subject to any value restrictions in `valid`.
#' @export
.PRT <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) <= 1) {F}
  else if (!any(base::is.na(x))) {F}
  else if (all(base::is.na(x))) {F}
  else {uj::check_valid(x, x[!base::is.na(x)], valid)}
}

## .mmm ####

#' @describeIn ppp_fast_help Checks for atomic one-char-ness subject to any value restrictions in `valid`.
#' @export
.CH1 <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {uj::check_valid(x, T, valid)}
          else {uj::check_valid(x, base::all(base::nchar(x) == 1), valid)}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for atomic three-char-ness subject to any value restrictions in `valid`.
#' @export
.CH3 <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {uj::check_valid(x, T, valid)}
          else {uj::check_valid(x, base::all(base::nchar(x) == 3), valid)}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for atomic character-ness subject to any value restrictions in `valid`.
#' @export
.CHR <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.character(x)) {uj::check_valid(x, T, valid)}
      else {uj::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for atomic string-ness (non-blank and character) subject to any value restrictions in `valid`.
#' @export
.STR <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {uj::check_valid(x, T, valid)}
          else {uj::check_valid(x, base::all(base::nchar(x) > 0), valid)}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for atomic color-ness subject to any value restrictions in `valid`.
#' @export
.CLR <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        x <- x[!base::is.na(x)]
        if (base::length(x) > 0) {uj::check_valid(x, !uj::is_err(grDevices::col2rgb(x)), valid)}
        else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for factor-ness subject to any value restrictions in `valid`.
#' @export
.FAC <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x)) {uj::check_valid(x, T, valid)}
      else {uj::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for ordered factor-ness subject to any value restrictions in `valid`.
#' @export
.ORD <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.ordered(x)) {uj::check_valid(x, T, valid)}
      else {uj::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for unordered factor-ness subject to any value restrictions in `valid`.
#' @export
.UNO <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x) & !base::is.ordered(x)) {uj::check_valid(x, T, valid)}
      else {uj::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for indexer-ness (positive whole number or logical) subject to any value restrictions in `valid`.
#' @export
.IND <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        x <- x[!base::is.na(x)]
        if (base::is.numeric(x)) {uj::check_valid(x, base::all(x == base::round(x)), valid)}
        else {uj::check_valid(x, base::is.logical(x), valid)}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for non-sortableness subject to any value restrictions in `valid`.
#' @export
.NST <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {uj::check_valid(x, !base::is.character(x) & !base::is.numeric(x) & !base::is.ordered(x), valid)}
      else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for sortable-ness subject to any value restrictions in `valid`.
#' @export
.SRT <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {uj::check_valid(x, base::is.character(x) | base::is.numeric(x) | base::is.ordered(x), valid)}
      else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for logical-ness subject to any value restrictions in `valid`.
#' @export
.LGL <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.logical(x)) {uj::check_valid(x, T, valid)}
      else {uj::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for numeric-ness subject to any value restrictions in `valid`.
#' @export
.NUM <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.numeric(x)) {uj::check_valid(x, T, valid)}
      else {uj::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for whole-number-ness subject to any value restrictions in `valid`.
#' @export
.WHL <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x == base::round(x)), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for odd-valued-ness subject to any value restrictions in `valid`.
#' @export
.ODD <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {
            x <- x / 2
            uj::check_valid(x, base::any(x != base::round(x)), valid)
          } else {F}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for even-valued-ness subject to any value restrictions in `valid`.
#' @export
.EVN <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {
            x <- x / 2
            uj::check_valid(x, base::all(x == base::round(x)), valid)
          } else {F}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for negative whole-numbered-ness subject to any value restrictions in `valid`.
#' @export
.NGW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj::check_valid(x, base::all(x < 0), valid)}
          else {F}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for non-positive whole-numbered-ness subject to any value restrictions in `valid`.
#' @export
.NPW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj::check_valid(x, base::all(x <= 0), valid)}
          else {F}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for positive numeric-ness subject to any value restrictions in `valid`.
#' @export
.POS <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj::check_valid(x, base::all(x > 0), valid)}
          else {F}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for positive whole-numbered-ness subject to any value restrictions in `valid`.
#' @export
.PSW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj::check_valid(x, base::all(x > 0), valid)}
          else {F}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for numeric fractional-ness subject to any value restrictions in `valid`.
#' @export
.FRC <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::any(x != base::round(x)), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for negative numeric-ness subject to any value restrictions in `valid`.
#' @export
.NEG <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x < 0), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for positive numeric-ness subject to any value restrictions in `valid`.
#' @export
.POS <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x > 0), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for non-negative numeric-ness subject to any value restrictions in `valid`.
#' @export
.NNG <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x >= 0), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for non-negative whole-numbered-ness subject to any value restrictions in `valid`.
#' @export
.NNW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {uj::check_valid(x, base::all(x >= 0), valid)}
          else {F}
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for non-positive numeric-ness subject to any value restrictions in `valid`.
#' @export
.NPS <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x <= 0), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for percentage-valued-ness subject to any value restrictions in `valid`.
#' @export
.PCT <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x >= 0 | x <= 100), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for proportion-valued-ness subject to any value restrictions in `valid`.
#' @export
.PPN <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x >= 0 | x <= 1), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for correlation-valued-ness subjec to any value restrictions in `valid`.
#' @export
.COR <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          uj::check_valid(x, base::all(x >= -1 | x <= 1), valid)
        } else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {uj::check_valid(x, T, valid)}
  } else {F}
}

## .sss ####

#' @describeIn ppp_fast_help Checks for column-ness subject to any value restrictions in `valid`.
#' @export
.COL <- function(x, valid = NULL) {
  if (base::is.data.frame(x) | base::is.matrix(x)) {uj::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) == 1, valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for row-ness subject to any value restrictions in `valid`.
#' @export
.ROW <- function(x, valid = NULL) {
  if (base::is.data.frame(x) | base::is.matrix(x)) {uj::check_valid(x, base::NROW(x) == 1 & base::NCOL(x) > 1, valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for point-ness subject to any value restrictions in `valid`.
#' @export
.PNT <- function(x, valid = NULL) {
  if (base::is.data.frame(x)) {uj::check_valid(x, base::NROW(x) * base::NCOL(x) == 1, valid)}
  else {uj::check_valid(x, base::length(x) == 1, valid)}
}

#' @describeIn ppp_fast_help Checks for solid-ness subject to any value restrictions in `valid`.
#' @export
.SLD <- function(x, valid = NULL) {
  if (base::is.array(x)) {uj::check_valid(x, base::length(base::which(base::dim(x) > 1)) > 2, valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for square matrix-ness subject to any value restrictions in `valid`.
#' @export
.SQR <- function(x, valid = NULL) {
  if (base::is.matrix(x)) {uj::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) > 1 & base::NROW(x) == base::NCOL(x), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for emptiness (length `0` but not `NULL`).
#' @export
.EMP <- function(x) {
  if (base::is.data.frame(x)) {base::NROW(x) * base::NCOL(x) == 0}
  else if (base::length(x) == 0) {!base::is.null(x)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for rectangularness (`nrow > 1` and `ncol > 1`) subject to any value restrictions in `valid`.
#' @export
.RCT <- function(x, valid = NULL) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x) | is.matrix(x)) {uj::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) > 1, valid)}
    else {F}
  } else {F}
}

#' @describeIn ppp_fast_help Checks for linearness subject to any value restrictions in `valid`.
#' @export
.LIN <- function(x, valid = NULL) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x)) {
      if (base::NROW(x) == 1 & base::NCOL(x) > 1) {uj::check_valid(x, T, valid)}
      else {uj::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) == 1, valid)}
    } else if (base::is.array(x)) {uj::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 2, valid)}
    else if (base::is.list(x)) {uj::check_valid(x, T, valid)}
    else if (base::is.vector(x)) {uj::check_valid(x, T, valid)}
  } else {F}
}

# .xxx_yyy ####

## .mmm_ccc ####

#' @describeIn ppp_fast_help Checks for atomic data.frame-ness subject to any value restrictions in `valid`.
#' @export
.atm_dtf <- function(x, valid = NULL) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {
      if (!base::is.atomic(x[[i]])) {return(F)}
    }
    uj::check_valid(x, T, valid)
  } else {F}
}

#' @describeIn ppp_fast_help Checks for atomic matrix-ness subject to any value restrictions in `valid`.
#' @export
.atm_mat <- function(x, valid = NULL) {uj::check_valid(x, base::is.atomic(x) & base::is.matrix(x), valid)}

#' @describeIn ppp_fast_help Checks for atomic scalar-ness subject to any value restrictions in `valid`.
#' @export
.atm_scl <- function(x, valid = NULL) {uj::check_valid(x, base::is.atomic(x) & base::length(x) == 1, valid)}

#' @describeIn ppp_fast_help Checks for atomic vec-ness subject to any value restrictions in `valid`.
#' @export
.atm_vec <- function(x, valid = NULL) {uj::check_valid(x, base::is.atomic(x) & uj::.VEC(x), valid)}

#' @describeIn ppp_fast_help Checks for atomic vector-list-ness subject to any value restrictions in `valid`.
#' @export
.atm_vls <- function(x, valid = NULL) {
  if (base::is.list(x) & !base::is.data.frame(x) & base::length(x) > 0) {uj::check_valid(x, base::all(base::sapply(x, base::is.atomic)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for character array-ness subject to any value restrictions in `valid`.
#' @export
.chr_arr <- function(x, valid = NULL) {uj::check_valid(x, base::is.array(x) & base::is.character(x), valid)}

#' @describeIn ppp_fast_help Checks for character data.frame-ness subject to any value restrictions in `valid`.
#' @export
.chr_dtf <- function(x, valid = NULL) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {
      if (!base::is.character(x[[i]])) {return(F)}
    }
    uj::check_valid(x, T, valid)
  } else {F}
}

#' @describeIn ppp_fast_help Checks for character vec-ness subject to any value restrictions in `valid`.
#' @export
.chr_vec <- function(x, valid = NULL) {uj::check_valid(x, uj::.VEC(x) & base::is.character(x), valid)}

#' @describeIn ppp_fast_help Checks for character vector-list-ness subject to any value restrictions in `valid`.
#' @export
.chr_vls <- function(x, valid = NULL) {
  if (base::is.list(x) & !is.data.frame(x)) {uj::check_valid(x, base::all(base::sapply(x, base::is.character)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for atomic completeness subject to any value restrictions in `valid`.
#' @export
.cmp_atm <- function(x, valid = NULL) {
  if (base::is.atomic(x) & base::length(x) > 0) {uj::check_valid(x, !base::any(base::is.na(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for character completeness subject to any value restrictions in `valid`.
#' @export
.cmp_chr <- function(x, valid = NULL) {
  if (base::is.character(x) & base::length(x) > 0) {uj::check_valid(x, !base::any(base::is.na(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for correlation-valued completeness subject to any value restrictions in `valid`.
#' @export
.cmp_cor <- function(x, valid = NULL) {
  if (!base::is.numeric(x) | base::length(x) == 0) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, base::all(x >= -1 & x <= 1), valid)}
}

#' @describeIn ppp_fast_help Checks for numeric completeness subject to any value restrictions in `valid`.
#' @export
.cmp_num <- function(x, valid = NULL) {
  if (base::is.numeric(x) & base::length(x) > 0) {uj::check_valid(x, !base::any(base::is.na(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_scl <- function(x, valid = NULL) {
  if (base::is.atomic(x) & base::length(x) == 1) {uj::check_valid(x, !base::is.na(x), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_vec <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else if (!uj::.VEC(x)) {F}
  else {uj::check_valid(x, !base::any(base::is.na(x)), valid)}
}

#' @describeIn ppp_fast_help Checks for logical scalar-ness subject to any value restrictions in `valid`.
#' @export
.lgl_scl <- function(x, valid = NULL) {uj::check_valid(x, base::logical(x) & base::length(x) == 1, valid)}

#' @describeIn ppp_fast_help Checks for non-negative whole-numbered vec-ness subject to any value restrictions in `valid`.
#' @export
.nnw_vec <- function(x, valid = NULL) {
  if (uj::.VEC(x)) {
    if (base::is.numeric(x)) {
      x <- x[!base::is.na(x)]
      if (base::length(x) > 0) {
        if (base::all(x >= 0)) {uj::check_valid(x, base::all(x == base::round(x)), valid)}
        else {F}
      } else {uj::check_valid(x, T, valid)}
    } else {F}
  } else {F}
}

## .bbb_mmm ####

#' @describeIn ppp_fast_help Checks for populated atomic-ness subject to any value restrictions in `valid`.
#' @export
.pop_atm <- function(x, valid = NULL) {uj::check_valid(x, base::is.atomic(x) & base::length(x) > 0, valid)}

#' @describeIn ppp_fast_help Checks for populated character-ness subject to any value restrictions in `valid`.
#' @export
.pop_chr <- function(x, valid = NULL) {uj::check_valid(x, base::length(x) > 0 & base::is.character(x), valid)}

#' @describeIn ppp_fast_help Checks for populated data.frame-ness subject to any value restrictions in `valid`.
#' @export
.pop_dtf <- function(x, valid = NULL) {uj::check_valid(x, base::is.data.frame(x) & base::NROW(x) > 0 & base::NCOL(x) > 0, valid)}

#' @describeIn ppp_fast_help Checks for populated matrix-ness subject to any value restrictions in `valid`.
#' @export
.pop_mat <- function(x, valid = NULL) {uj::check_valid(x, base::is.matrix(x) & base::length(x) > 0, valid)}

#' @describeIn ppp_fast_help Checks for populated sortableness subject to any value restrictions in `valid`.
#' @export
.pop_srt <- function(x, valid = NULL) {uj::check_valid(x, base::length(x) > 0 & (base::is.character(x) | base::is.numeric(x) | base::is.ordered(x)), valid)}

#' @describeIn ppp_fast_help Checks for populated vec-ness subject to any value restrictions in `valid`.
#' @export
.pop_vec <- function(x, valid = NULL) {uj::check_valid(x, uj::.VEC(x) & base::length(x) > 0, valid)}

#' @describeIn ppp_fast_help Checks for populated vector-list-ness subject to any value restrictions in `valid`.
#' @export
.pop_vls <- function(x, valid = NULL) {uj::check_valid(x, uj::.VLS(x) & base::length(x) > 0, valid)}

# .xxx_yyy_zzz ####

## .iii_mmm_ccc ####

#' @describeIn ppp_fast_help Checks for atomic vector completeness subject to any value restrictions in `valid`.
#' @export
.cmp_atm_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.atomic(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for one-char scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_ch1_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj::check_valid(x, base::nchar(x) == 1, valid)}
}

#' @describeIn ppp_fast_help Checks for one-char vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_ch1_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, base::all(base::nchar(x) == 1), valid)}
}

#' @describeIn ppp_fast_help Checks for three-char scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_ch3_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj::check_valid(x, base::nchar(x) == 3, valid)}
}

#' @describeIn ppp_fast_help Checks for three-char vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_ch3_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj::check_valid(x, base::all(base::nchar(x) == 3), valid)}
}

#' @describeIn ppp_fast_help Checks for character array completeness subject to any value restrictions in `valid`.
#' @export
.cmp_chr_arr <- function(x, valid = NULL) {
  if      (base::length(x) == 0     ) {F}
  else if (!uj::.ARR(x)            ) {F}
  else if (!base::is.character(x)   ) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for character data-frame completeness subject to any value restrictions in `valid`.
#' @export
.cmp_chr_dtf <- function(x, valid = NULL) {
  if (!uj::.chr_dtf(x)) {F}
  else if (base::any(base::is.na(uj::av(x)))) {F}
  else {uj::check_valid(uj::av(x), T, valid)}
}

#' @describeIn ppp_fast_help Checks for character vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_chr_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for character vector-list completeness subject to any value restrictions in `valid`.
#' @export
.cmp_chr_vls <- function(x, valid = NULL) {
  if (!uj::.chr_vls(x)) {F}
  else if (base::any(base::is.na(uj::av(x)))) {F}
  else {uj::check_valid(uj::av(x), T, valid)}
}

#' @describeIn ppp_fast_help Checks for correlation-valued scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_cor_scl <- function(x, valid = NULL) {
  if (!uj::.SCL(x)) {F}
  else if (!uj::.COR(x)) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for correlation-valued vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_cor_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!uj::.COR(x)) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for correlation-valued multivec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_cor_mvc <- function(x, valid = NULL) {
  if (!uj::.MVC(x)) {F}
  else if (!uj::.COR(x)) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for correlation-valued square matrix completeness subject to any value restrictions in `valid`.
#' @export
.cmp_cor_sqr <- function(x, valid = NULL) {
  if (!uj::.SQR(x)) {F}
  else if (!uj::.COR(x)) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for character generic completeness subject to any value restrictions in `valid`.
#' @export
.cmp_chr_gen <- function(x, valid = NULL) {
  if (uj::.cmp_chr_vec(x, valid = valid)) {T}
  else if (uj::.cmp_chr_arr(x, valid = valid)) {T}
  else {uj::.cmp_chr_vls(x, valid = valid)}
}

#' @describeIn ppp_fast_help Checks for character scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_chr_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for color vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_clr_vec <- function(x, valid = NULL) {uj::check_valid(x, uj::.cmp_vec(x) & uj::.CLR(x), valid)}

#' @describeIn ppp_fast_help Checks for logical scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_lgl_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.logical(x)) {F}
  else {uj::check_valid(x, !base::is.na(x), valid)}
}

#' @describeIn ppp_fast_help Checks for logical vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_lgl_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.logical(x)) {F}
  else {uj::check_valid(x, !base::any(base::is.na(x)), valid)}
}

#' @describeIn ppp_fast_help Checks for non-negative numeric scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_nng_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj::check_valid(x, x < 0, valid)}
}

#' @describeIn ppp_fast_help Checks for non-negative whole-numbered scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_nnw_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else if (x < 0) {F}
  else {uj::check_valid(x, x == round(x), valid)}
}

#' @describeIn ppp_fast_help Checks for non-negative whole-numbered vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_nnw_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else if (base::any(x < 0)) {F}
  else {uj::check_valid(x, base::all(x == round(x)), valid)}
}

#' @describeIn ppp_fast_help Checks for numeric scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_num_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for numeric vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_num_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for positive numeric vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_pos_vec <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, base::all(x > 0), valid)}
}

#' @describeIn ppp_fast_help Checks for proportion-valued scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_ppn_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, base::all(x >= 0 & x <= 1), valid)}
}

#' @describeIn ppp_fast_help Checks for proportion-value vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_ppn_vec <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, base::all(x >= 0 & x <= 1), valid)}
}

#' @describeIn ppp_fast_help Checks for positive whole-numbered scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_psw_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else if (x <= 0) {F}
  else {uj::check_valid(x, x == round(x), valid)}
}

#' @describeIn ppp_fast_help Checks for positive whole-numbered vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_psw_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else if (base::any(x <= 0)) {F}
  else {uj::check_valid(x, base::all(x == round(x)), valid)}
}

#' @describeIn ppp_fast_help Checks for sortable vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_srt_vec <- function(x, valid = NULL) {
  if (!uj::.VEC(x)) {F}
  else if (!base::is.numeric(x) & !base::is.character(x) & !base::is.ordered(x)) {F}
  else {uj::check_valid(x, base::any(base::is.na(x)), valid)}
}

#' @describeIn ppp_fast_help Checks for string scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_str_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {uj::check_valid(x, !base::any(x == ""), valid)}
}

#' @describeIn ppp_fast_help Checks for string vec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_str_vec <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, !base::any(x == ""), valid)}
}

#' @describeIn ppp_fast_help Checks for whole-numbered scalar completeness subject to any value restrictions in `valid`.
#' @export
.cmp_whl_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, x == base::round(x), valid)}
}

#' @describeIn ppp_fast_help Checks for atomic multivec completeness subject to any value restrictions in `valid`.
#' @export
.cmp_atm_mvc <- function(x, valid = NULL) {
  if (!uj::.MVC(x)) {F}
  else if (!base::is.atomic(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {uj::check_valid(x, T, valid)}
}

#' @describeIn ppp_fast_help Checks for atomic multivec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_atm_mvc <- function(x, valid = NULL) {
  if (uj::.cmp_atm_mvc(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for atomic vec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_atm_vec <- function(x, valid = NULL) {
  if (uj::.cmp_atm_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for one-char vec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_ch1_vec <- function(x, valid = NULL) {
  if (uj::.cmp_ch1_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for three-char vec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_ch3_vec <- function(x, valid = NULL) {
  if (uj::.cmp_ch3_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for character vec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_chr_vec <- function(x, valid = NULL) {
  if (uj::.cmp_chr_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for non-negative whole-numbered non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_nnw_vec <- function(x, valid = NULL) {
  if (uj::.cmp_nnw_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for numeric vec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_num_vec <- function(x, valid = NULL) {
  if (uj::.cmp_num_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for positive whole-numbered vec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_psw_vec <- function(x, valid = NULL) {
  if (uj::.cmp_psw_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for string vec non-`NA` uniqueness subject to any value restrictions in `valid`.
#' @export
.unq_str_vec <- function(x, valid = NULL) {
  if (uj::.cmp_str_vec(x)) {uj::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @describeIn ppp_fast_help Checks for atomic rectangular data-frame-ness subject to any value restrictions in `valid`.
#' @export
.atm_rct_dtf <- function(x, valid = NULL) {
  if (!uj::.DTF(x)) {return(F)}
  if (base::nrow(x) < 2 | base::ncol(x) < 2) {return(F)}
  for (i in 1:base::ncol(x)) {if (!uj::.ATM(x[[i]])) {return(F)}}
  uj::check_valid(x, T, valid)
}
