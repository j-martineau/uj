#' @encoding UTF-8
#' @name paths
#' @family extensions
#' @family files
#' @title Evaluate and manipulate paths to files or folders
#' @description Take an file reference and get its directories, path, and name; its parent's directories, path, and name. Convert a `...` args to a path, check the validity of a path, and create new directories within a given directory.
#' @details
#' \tabular{ll}{  `parent_dirs`   \tab Calls `parent_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names
#'                                     of any intermediate folders, and the name of the parent folder of the object specified by `...`.                                         \cr   \tab   \cr
#'                `parent_path`   \tab Calls `object_path` and extract from the resulting character scalar just the path to the parent folder of the object specified by `...`
#'                                     (i.e., discarding the name of the object itself).                                                                                        \cr   \tab   \cr
#'                `parent_name`   \tab Calls `parent_path` and extract from the resulting character scalar just the name of last folder in the path.                            \cr   \tab   \cr
#'                `object_path`   \tab Collapses `...` into a character scalar and expand the path to the object indicated by the result to account for relative paths.         \cr   \tab   \cr
#'                `object_dirs`   \tab Calls `object_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names
#'                                     of any intermediate folders, the name of the parent folder of the object, and the name of the object.                                    \cr   \tab   \cr
#'                `object_name`   \tab Calls `object_path` and extract from the resulting character scalar just the name of the object, discarding the path to its parent.      \cr   \tab   \cr
#'                `new_dirs`      \tab Creates sub-directories within an existing directory, optionally asking the user to choose that existing directory.                      \cr   \tab   \cr
#'                `is_path`       \tab Checks whether `...` resolves to a valid path for an object (file or folder) when `...` arguments are collapsed into a character scalar. \cr   \tab   \cr
#'                `as_path`       \tab Collapses `...` into a path using the current platform file path separator `.Platform$file.sep`.                                                        }
#' @details In most cases, if (optionally) specified, functions throw an error if `...` does not resolve to a valid object path.
#' @param ... Atomic arguments pasted to create
#' @param dirs A \link[=cmp_chr_vec]{complete character vec} of new sub-directory names.
#' @param path A \link[=cmp_chr_scl]{complete character scalar} path to either a directory or a file.
#' @param .ERR `TRUE` or `FALSE` indicating whether an error should be thrown if `...` is not a valid object path when collapsed to a character scalar.
#' @return **A character scalar** \cr\cr `object_dirs, parent_path` \cr `object_name, parent_name` \cr `as_path`
#' \cr\cr  **A character vector** \cr\cr `object_dirs, parent_dirs`
#' \cr\cr  **A logical scalar**   \cr\cr `is_path`
#' @examples
#' path. <- path.expand("~")
#' parts. <- ss(.Platform$file.sep, path.)
#' parts. <- parts.[parts. != ""]
#' parts.[1] <- paste0(.Platform$file.sep, parts.[1])
#' parts. <- paste0("'", parts., "'")
#' code. <- paste0(parts., collapse = ", ")
#' is_path. <- paste0("is_path(", code., ")")
#' as_path. <- paste0("as_path(", code., ")")
#'
#' path.
#' parts.
#' is_path.
#' as_path.
#'
#' parent_dirs(path.)
#' object_dirs(path.)
#' parent_path(path.)
#' object_path(path.)
#' parent_name(path.)
#' object_name(path.)
#' run(is_path.)
#' run(as_path.)
#' @export
is_path <- function(..., .ERR = F) {
  X <- base::list(...)
  OkN <- base::length(X) > 0
  OkNs <- uj::f0(!OkN, T, base::all(base::lengths(X) > 0))
  Errors <- NULL
  if (!OkN) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!OkNs) {Errors <- base::c(Errors, "An argument in [...] is empty.")}
  if (!uj::f0(OkN | OkNs, T, base::all(base::sapply(X, uj:::.cmp_chr)))) {Errors <- base::c(Errors, "Arguments in [...] must be complete character scalars/vecs (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!uj:::.cmp_lgl_scl(.ERR)) {Errors <- base::c(Errors, "[.ERR] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  path <- base::file.path(...)
  if (base::file.exists(path)) {return(T)}
  OK <- !uj::is_err(tryCatch(base::file.create(path, showWarnings = F), error = function(e) e, finally = NULL))
  if (!OK & .ERR) {uj::stopperr("[...] doesn't resolve to a valid file/folder path.", .PKG = "uj")}
  if (OK) {base::file.remove(path)}
  OK
}

#' @rdname paths
#' @export
as_path <- function(..., .ERR = T) {
  Sep <- base::.Platform$file.sep
  path <- tryCatch(base::file.path(...), error = function(e) e, finally = NULL)
  if (uj::is_err(path)) {uj::stopperr("[...] does not resolve to a valid file/folder path.", .PKG = "uj")}
  path <- base::strsplit(path, Sep, fixed = T)[[1]]
  Args <- base::paste0("path[", 1:base::length(path), "]")
  Args <- base::paste0(Args, collapse = ", ")
  Code <- base::paste0("base::file.path(", Args, ")")
  path <- uj::run(Code)
  if (base::substr(path, 1, 1) != Sep) {path <- base::paste0(Sep, path)}
  path
}

#' @rdname paths
#' @export
object_dirs <- function(path, .ERR = T) {
  path <- uj::as_path(path, .ERR = .ERR)
  path <- base::strsplit(path, base::.Platform$file.sep, fixed = T)[[1]]
  path <- path[path != ""]
  uj::f0(base::length(path) < 2, base::character(0), path[1:(base::length(path) - 1)])
}

#' @rdname paths
#' @export
parent_dirs <- function(path, .ERR = T) {
  path <- uj::object_dirs(path, .ERR)
  uj::f0(base::length(path) < 2, base::character(0), path[1:(base::length(path) - 1)])
}

#' @rdname paths
#' @export
object_path  <- function(path, .ERR = T) {uj::as_path(path, .ERR = .ERR)}

#' @rdname paths
#' @export
parent_path  <- function(path, .ERR = T) {base::paste0(uj::object_dirs(path, .ERR), collapse = base::.Platform$file.sep)}

#' @rdname paths
#' @export
object_name <- function(path, .ERR = T) {base::basename(uj::as_path(path, .ERR = .ERR))}

#' @rdname paths
#' @export
parent_name <- function(path, .ERR = T) {base::basename(uj::parent_path(path, .ERR = .ERR))}

#' @rdname paths
#' @export
new_dirs <- function(dirs, path = NULL) {
  if (base::is.null(path)) {path <- uj::choose_dir("folder to create new sub-folders in")}
  for (d in dirs) {if (!base::dir.exists(base::file.path(path, d))) {base::dir.create(base::file.path(path, d))}}
}
