#' @name paths
#' @family files
#' @family extensions
#' @title Evaluate and manipulate paths to files or folders
#' @description \itemize{
#'   \item **`is_path`**: checks whether `...` resolves to a valid path for an object (file or folder) when `...` arguments are collapsed into a character scalar.
#'   \item **`as_path`**: collapses `...` into a path using the current platform file path separator `.Platform$file.sep`.
#'   \item **`new_dirs`**: creates sub-directories within an existing directory, optionally asking the user to choose that existing directory.
#'   \item **`object_path`**: collapses `...` into a character scalar and expand the path to the object indicated by the result to account for relative paths.
#'   \item **`parent_path`**: calls `object_path` and extract from the resulting character scalar just the path to the parent folder of the object specified by `...` (i.e., discarding the name of the object itself).
#'   \item **`object_name`**: calls `object_path` and extract from the resulting character scalar just the name of the object, discarding the path to its parent.
#'   \item **`parent_name`**: calls `parent_path` and extract from the resulting character scalar just the name of last folder in the path.
#'   \item **`object_dirs`**: calls `object_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names of any intermediate folders, the name of the parent folder of the object, and the name of the object.
#'   \item **`parent_dirs`**: calls `parent_path` and split the resulting character scalar into a character vector containing the name of the root folder, the names of any intermediate folders, and the name of the parent folder of the object specified by `...`.
#' }
#' @details In most cases, if (optionally) specified, functions throw an error if `...` does not resolve to a valid object path.
#' @param ... Atomic arguments pasted to create
#' @param err A non-`NA` logical scalar indicating whether an error should be thrown if `...` is not a valid object path when collapsed to a character scalar.
#' @param dirs A \link[=cmp_chr_vec]{complete character vec} of new sub-directory names.
#' @param path A \link[=cmp_chr_scl]{complete character scalar} path to either a directory or a file.
#' @return \itemize{
#'   \item **`is_path`**: a logical scalar.
#'   \item **`object_dirs, folder_dirs`**: a character vector.
#'   \item **`object_path, folder_path, object_name, folder_name`**: a character scalar.
#' }
#' @export
is_path <- function(..., err = F) {
  x <- list(...)
  ok.n <- length(x) == 0
  ok.ns <- f0(!ok.n, T, all(lengths(x) > 0))
  errs <- c(f0(ok.n                                        , NULL, "\n \u2022 [...] is empty."),
            f0(ok.ns                                       , NULL, "\n \u2022 An argument in [...] is empty."),
            f0(f0(ok.n | ok.ns, T, all(sapply(x, cmp_chr))), NULL, "\n \u2022 Arguments in [...] must be complete character scalars/vecs (?cmp_chr_scl, ?cmp_chr_vec)."),
            f0(isTF(err)                                   , NULL, "\n \u2022 [err] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  path <- file.path(...)
  exists <- file.exists(path)                                                    # whether the file exists
  if (exists) {return(T)}                                                        # if the resulting file already exists, return T
  out <- isERR(file.create(path, showWarnings = F))                              # can such a file be created?
  if (out) {file.remove(path)}                                                   # remove any newly created file
  if (!out & err) {stop("\n \u2022 [...] doesn't resolve to a valid file/folder path after collapsing.")}
  out
}

#' @rdname paths
#' @export
as_path <- function(...) {if (is_path(..., err = T)) {file.path(...)}}

#' @rdname paths
#' @export
object_path  <- function(path, err = T) {f0(is_path(path, err = err), path.expand(path), "")}

#' @rdname paths
#' @export
parent_path  <- function(path, err = T) {dirname(object_path(path, err = err))}

#' @rdname paths
#' @export
object_name  <- function(path, err = T) {basename(object_path(path, err = err))}

#' @rdname paths
#' @export
parent_name  <- function(path, err = T) {object_name(parent_path(path, err = err))}

#' @rdname paths
#' @export
object_dirs <- function(path, err = T) {strsplit(object_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]}

#' @rdname paths
#' @export
parent_dirs <- function(path, err = T) {strsplit(parent_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]}

#' @rdname paths
#' @export
newdirs <- function(dirs, path = NULL) {
  cancel <- function() {stop("canceled by user.")}                               # FUNCTION to throw an error
  if (inll(path)) {                                                              # IF path is not supplied
    msgbox("In the next dialog, choose a folder to create new sub-folders in.")  # : notify user of what to do next (title doesn't always appear in next box)
    path <- dirbox(x = "Choose a folder to create new sub-folders in:")$res      # : ask user to select a directory
  }                                                                              # END
  if (inll(path)) {cancel()} else if (!file.exists(path)) {cancel()}             # IF path is still NULL > error ELSE if it doesn't exist > error
  for (dir in dirs) {                                                            # FOR each new directory to create in the selected directory
    newdir <- file.path(path, dir)                                               # : build the path
    if (!dir.exists(newdir)) {dir.create(newdir)}                                # : IF it doesn't already exist > create it
  }                                                                              # END
}
