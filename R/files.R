#' @name files.
#' @family files
#' @family extensions
#' @title Evaluate and manipulate paths to files or folders on disk.
#' @details In most cases, if (optionally) specified, functions throw an error
#'   if \code{...} does not resolve to a valid object path
#' @param ... Atomic arguments pasted to create
#' @param err \link[cmp_atm_scl]{Complete logical scalar} indicating whether an
#'   error should be thrown if \code{...} is not a valid object path when
#'   collapsed to a character scalar.
#' @param dirs \link[cmp_chr_vec]{Complete character vec} of new sub-directory
#'   names.
#' @param path \link[cmp_chr_scl]{Complete character scalar} path to either a
#'   directory or a file.
#' @return \strong{\code{is_path}}: A logical scalar. \strong{\code{object_path,
#'   folder_path, object_name, folder_name}}: A character scalar.
#'   \strong{\code{object_dirs, folder_dirs}}: A character vector.
#' @export
files. <- function() {help("files.", package = "uj")}

#' @describeIn files. Check whether \code{...} resolves to a valid path for an
#'   object (file or folder) when arguments in \code{...} are collapsed into a
#'   character scalar.
is_path <- function(..., err = F) {
  x  <- list(...)
  vn <- length(x) > 0
  vns <- f0(!vn, T, all(lengths(x) > 0))
  vx <- f0(!vn | !vns, T, all(sapply(x, cmp_chr)))
  ve <- isTF(err)
  errs <- NULL
  if (!vn) {errs <- c(errs, "\n • [...] is empty.")}
  if (!vns) {errs <- c(errs, "\n • An argument in [...] is empty.")}
  if (!vx) {errs <- c(errs, "\n • Arguments in [...] must be complete character scalars/vecs (?cmp_chr_scl, ?cmp_chr_vec).")}
  if (!ve) {errs <- c(errs, "\n • [err] must be TRUE or FALSE.")}
  if (idef(errs)) {stop(errs)}
  path <- file.path(...)
  exists <- file.exists(path)                                                    # whether the file exists
  if (exists) {return(T)}                                                        # if the resulting file already exists, return T
  out <- isERR(file.create(path, showWarnings = F))                              # can such a file be created?
  if (out) {file.remove(path)}                                                   # remove any newly created file
  if (!out & err) {stop("\n • [...] doesn't resolve to a valid file/folder path after collapsing.")}
  out
}

#' @describeIn files. Collapse \code{...} into a path using the current
#'   platform file path separator (\code{.Platform$file.sep}).
#' @export
as_path <- function(...) {if (is_path(..., err = T)) {file.path(...)}}

#' @describeIn files. Collapse \code{...} into a character scalar and expands
#'   the path to the object indicated by the result to account for relative
#'   paths.
#' @export
object_path  <- function(path, err = T) {f0(is_path(path, err = err), path.expand(path), "")}

#' @describeIn files. Call \code{object_path} and extracts from the resulting
#'   character scalar just the path to the parent folder of the object specified
#'   by \code{...} (i.e., discarding the name of the object itself).
#' @export
parent_path  <- function(path, err = T) {dirname(object_path(path, err = err))}

#' @describeIn files. Calls \code{object_path} and extracts from the resulting
#'   character scalar just the name of the object, discarding the path to its
#'   parent.
#' @export
object_name  <- function(path, err = T) {basename(object_path(path, err = err))}

#' @describeIn files. Call \code{parent_path} and extracts from the resulting
#'   character scalar just the name of last folder in the path
#' @export
parent_name  <- function(path, err = T) {object_name(parent_path(path, err = err))}

#' @describeIn files. Call \code{object_path} and splits the resulting
#'   character scalar into a character vector containing the name of the root
#'   folder, the names of any intermediate folders, the name of the parent
#'   folder of the object, and the name of the object.
#' @export
object_dirs <- function(path, err = T) {strsplit(object_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]}

#' @describeIn files. Call \code{folder_path} and splits the resulting
#'   character scalar into a character vector containing the name of the root
#'   folder, the names of any intermediate folders, and the name of the parent
#'   folder of the object specified by \code{...}.
#' @export
parent_dirs <- function(path, err = T) {strsplit(parent_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]}

#' @describeIn files. Create sub-directories within an existing directory,
#'   optionally asking the user to choose an existing directory.
newdirs <- function(dirs, path = NULL) {
  cancel <- function() {stop("canceled by user.")}                               # FUNCTION to throw an error
  if (inll(path)) {                                                              # IF path is not supplied
    msgbox("In the next dialog, select a folder to create new sub-folders in.")  # : notify user of what to do next (title doesn't always appear in next box)
    path <- dirbox(x = "Choose a folder to create new sub-folders in:")$res      # : ask user to select a directory
  }                                                                              # END
  if (inll(path)) {cancel()} else if (!file.exists(path)) {cancel()}             # IF path is still NULL > error ELSE if it doesn't exist > error
  for (dir in dirs) {                                                            # FOR each new directory to create in the selected directory
    newdir <- file.path(path, dir)                                               # : build the path
    if (!dir.exists(newdir)) {dir.create(newdir)}                                # : IF it doesn't already exist > create it
  }                                                                              # END
}
