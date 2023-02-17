#' @encoding UTF-8
#' @family extensions
#' @family values
#' @title Atomize
#' @description Get (and maybe process) a vector of all atoms contained in the full set of `...` args.
#' @details Functions in this family are:
#' \tabular{ll}{  `atomize, atoms, av, a`   \tab Gets a vector of all atomic values in all `...` arguments. \cr   \tab     }
#' \tabular{ll}{  `unique_av, uav, uv`       \tab Calls `unique(av(...))`                                    \cr   \tab     }
#' \tabular{ll}{  `which_av, wav, wv`        \tab Calls `which(av(...))`                                     \cr   \tab   \cr
#'                `paste_av, pav, pv`        \tab Calls `paste0(p, av(...), s)`                              \cr   \tab     }
#' \tabular{ll}{  `sort_av, sav, sv`         \tab Calls `sort(av(...))`.                                     \cr   \tab   \cr
#'                `glue_av, gav, gv`         \tab Calls `paste0(av(...) collapse = g)`                       \cr   \tab     }
#' \tabular{ll}{  `sort_uv, suv`             \tab Calls `sort(unique(av(...)))`                              \cr   \tab     }
#' @param ... Arguments to be atomized. Expected to atomize to a logical vector for `which_av` and its aliases `wav` and `wv`.
#' @param g Character scalar glue.
#' @param p Character scalar prefix.
#' @param s Character scalar suffix.
#' @return **A character scalar** \cr `glue_av, gav, gv`
#' \cr\cr  **A character vector** \cr `paste_av, pav, pv`
#' \cr\cr  **An integer vector**  \cr `which_av, wav, wv`
#' \cr\cr  **An atomic vector**   \cr  All others
#' @examples
#' x <- list("a", "b", "c")
#' y <- data.frame(1:3)
#'
#' x
#' y
#'
#' a(x)
#' av(y)
#' atoms(x, y)
#' atomize(x, "d", y, 4)
#' glue_av(letters, LETTERS, g = "-")
#' paste_av(letters, p = "\u2022 ", s = ".")
#' sort_av(letters, LETTERS, 0:9)
#' sort_uv(letters, LETTERS, 0:9)
#' unique_av(letters, "d", 0:9, 5)
#' @export
atomize <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  x
}

#' @rdname atomize
#' @export
atoms <- atomize

#' @rdname atomize
#' @export
av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  x
}

#' @rdname atomize
#' @export
a <- atomize

#' @rdname atomize
#' @export
sort_av <- function(...) {base::sort(uj::av(...))}

#' @rdname atomize
#' @export
sv <- sort_av

#' @rdname atomize
#' @export
sav <- sort_av

#' @rdname atomize
#' @export
glue_av <- function(..., g = "") {
  uj::err_if_not(uj::cmp_chr_scl(g), "[g] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  uj::g(g, uj::av(...))
}

#' @rdname atomize
#' @export
gv <- glue_av

#' @rdname atomize
#' @export
gav <- glue_av

#' @rdname atomize
#' @export
paste_av <- function(..., p = "", s = "") {
  uj::errs_if_nots(uj::cmp_chr_scl(p), "[p] must be a complete character scalar (?cmp_chr_scl).",
                   uj::cmp_chr_scl(s), "[s] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  uj::p0(uj::p(p, uj::av(...)), s)
}

#' @rdname atomize
#' @export
pav <- paste_av

#' @rdname atomize
#' @export
pv <- paste_av

#' @rdname atomize
#' @export
unique_av <- function(...) {base::unique(uj::av(...))}

#' @rdname atomize
#' @export
uav <- unique_av

#' @rdname atomize
#' @export
uv <- unique_av

#' @rdname atomize
#' @export
sort_uv <- function(...) {base::sort(uj::UV(...))}

#' @rdname atomize
#' @export
suv <- sort_uv

#' @rdname atomize
#' @export
which_av <- function(...) {
  x <- uj::av(...)
  uj::err_if_not(uj::LGL(x), "[...] does not atomize (?uj::atomize) to a logical object.", PKG = "uj")
  uj::W(x)
}

#' @rdname atomize
#' @export
wav <- which_av

#' @rdname atomize
#' @export
wv <- which_av
