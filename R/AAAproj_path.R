#' Path to file (in the unix sense) within the project. The path is
#' composed of \code{getOption("bioc_contributions_project")} or, if
#' NULL, the home directory "~".
#'
#' @param file character(1) file or directory name within the project.
#' @param check.exists logical(1) indicating whether the file must
#'     exist
#' @export
proj_path <-
    function(file, check.exists=TRUE)
{
    stopifnot(is.character(file), length(file) == 1L)
    stopifnot(is.logical(check.exists), length(check.exists) == 1L,
              !is.na(check.exists))
    path <- file.path(getOption("bioc_contributions_project", "~"), file)
    if (check.exists && !file.exists(path))
        stop("path does not exist:\n  '", path, "'")
    path
}
