#' @encoding UTF-8
#' @family extensions
#' @family values
#' @title Atomize (reduce arguments to an atomic vector)
#' @description \tabular{rl}{
#'     `atomize, atoms, av, a`   \tab Get a vector of all atomic values in all `...` arguments.
#'   \cr  `unique_av, uav, uv`   \tab Call `unique(av(...))`
#'   \cr   `which_av, wav, wv`   \tab Call `which(av(...))`
#'   \cr   `paste_av, pav, pv`   \tab Call `paste0(p, av(...), s)`
#'   \cr    `sort_av, sav, sv`   \tab Call `sort(av(...))`.
#'   \cr    `glue_av, gav, gv`   \tab Call `paste0(av(...) collapse = g)`
#'   \cr        `sort_uv, suv`   \tab Call `sort(unique(av(...)))`
#' }
#' @param ... Arguments to be atomized. Expected to atomize to a logical vector for `which_av` and its aliases `wav` and `wv`.
#' @param g Character scalar glue.
#' @param p Character scalar prefix.
#' @param s Character scalar suffix.
#' @return *An character scalar* \cr   `glue_av, gav, gv`
#' \cr\cr *A character vector* \cr   `paste_av, pav, pv`
#' \cr\cr *An integer vector* \cr   `which_av, wav, wv`
#' \cr\cr *An atomic vector* \cr   All others
#' @export
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
#'
#' gav(x, "d", y, 4)
#' glue_av(letters, LETTERS, g = "-")
#'
#' pav(x, "d", y, 4)
#' paste_av(letters, p = "\u2022 ", s = ".")
#'
#' sav(x, "d", y, 4)
#' sort_av(letters, LETTERS, 0:9)
atomize <- function(...) {x <- base::as.vector(base::unlist(base::list(...), T, F)); base::attributes(x) <- NULL; x}

#' @rdname atomize
#' @export
atoms <- atomize

#' @rdname atomize
#' @export
av <- atomize

#' @rdname atomize
#' @export
a <- atomize

#' @rdname atomize
#' @export
sort_av <- function(...) {base::sort(uj::atomize(...))}

#' @rdname atomize
#' @export
sv <- sort_av

#' @rdname atomize
#' @export
sav <- sort_av

#' @rdname atomize
#' @export
glue_av <- function(..., g = "") {
  if (!uj::cmp_chr_scl(g)) {stop(uj::format_err("gav", "[g] must be a complete character scalar (?cmp_chr_scl).", pkg = "uj"))}
  base::paste0(uj::atomize(...), collapse = "")
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
  errs <- base::c(uj::f0(uj::cmp_chr_scl(p), NULL, "[p] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::cmp_chr_scl(s), NULL, "[g] must be a complete character scalar (?cmp_chr_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs("pav", errs, pkg = "uj"))}
  base::paste0(p, uj::atomize(...), s)
}

#' @rdname atomize
#' @export
pv <- paste_av

#' @rdname atomize
#' @export
pav <- paste_av

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
sort_uv <- function(...) {base::sort(base::unique(uj::av(...)))}

#' @rdname atomize
#' @export
suv <- sort_uv

#' @rdname atomize
#' @export
which_av <- function(...) {
  x <- uj::av(...)
  uj::f0(uj::ilgl(x), base::which(x), stop(uj::format_errs(pkg = "uj", "[...] does not resolve to a logical object.")))
}

#' @rdname atomize
#' @export
wv <- which_av

#' @rdname atomize
#' @export
wav <- which_av
