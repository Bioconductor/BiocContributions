## This is where code that is used for cleaning up a package and
## adding it to svn will live.

## I want to follow BiocCheck's lead here and do a cleanup command like:
## R CMD cleanup

## cleanup will then do the following:
## drop and lines from DESCRIPTION thet start with "packaged"
## rm -rf any build directories or  /inst/doc


## cp -r the package to the svn repos
## put the package into the most recent manifest


## This retrieves the short name for a package or it's true name (so
## no version numbers or extensions it's basically what the source dir
## would be called)
.getShortPkgName <- function(tarball){
    sub("_.*","", basename(tarball))
}

## remove .git directories
.cleanGIT <- function(dir) {
    gitdir <- sprintf("%s/.git", dir)
    stopifnot(unlink(gitdir, recursive=TRUE, force=TRUE) == 0L)
}

## This throws away unwanted extra lines and junk from the DESCRIPTION file
.cleanDESCRIPTION <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    DESC <- DESC[,!grepl("Packaged",colnames(DESC)),drop=FALSE]
    write.dcf(DESC, file=dirPath)
}

## white-list '.' directories
.cleanDotdirs <- function(dir) {
    ok <- c(".BBSoptions", ".Rbuildignore", ".Rinstignore", ".svnignore",
            ".gitignore", ".travis.yml")
    suspect <- dir(dir, pattern="^\\.", recursive=TRUE, all=TRUE,
                   include.dirs=TRUE)
    drop <- file.path(dir, setdiff(suspect, ok))
    stopifnot(unlink(drop, recursive=TRUE, force=TRUE) == 0L)
}

readDESCRIPTION <- function(tarball) {
    pkgdir <- tarball
    description <- "DESCRIPTION"
    if (endsWith(tarball, "tar.gz")) {
        ls <- untar(tarball, list = TRUE)
        fls <- ls[basename(ls) == description]
        if (length(fls) != 1L)
            stop("Could not find a unique DESCRIPTION file",
                 "\n  tarball: ", sQuote(tarball),
                 "\n  paths (if any): ",
                 "\n    ", paste(sQuote(fls), collapse="\n    "))
        description <- fls
        pkgdir <- tempdir()
        untar(tarball, files = description, exdir = pkgdir)
    }
    res <- read.dcf(file.path(pkgdir, description), all = TRUE)

    # generate a maintainer from Authors@R if none specified
    if (is.null(res$Maintainer)) {
      authors <- utils:::.read_authors_at_R_field(res$`Authors@R`)
      res$Maintainer <- Filter(function(x) "cre" %in% x$role, authors)
    } else {
      ## "'Ima Person' <ima@person.org>" --> "Ima Person <ima@person.org>"
      res$Maintainer <- sub("' *(.*) *' <", "\\1 <", res$Maintainer)
    }
    structure(res, class = c("description", "data.frame"))
}

print.description <- function(x, ...) {
    message(paste(names(x), x, sep = ": ", collapse = "\n"))
}

## This throws away dirs that are inserted into the tarball by 'R CMD build.'
.removeUnwantedDirs <- function(dir){
    instDoc <- file.path(dir, "inst", "doc")
    if(file.exists(instDoc)){
        unlink(instDoc, recursive=TRUE)
    }
    buildDir <- file.path(dir, "build")
    if(file.exists(buildDir)){
        unlink(buildDir, recursive=TRUE)
    }
    gitDir <- file.path(dir, ".git")
    if(file.exists(gitDir)){
        unlink(gitDir, recursive=TRUE)
    }
}

.removeUnwantedFiles <- function(dir){
    srcDir <- file.path(dir, "src")
    if(file.exists(srcDir)){
        unlink(file.path(srcDir,"*.o"))
    }
}

#' Clean a Software Package
#'
#' This is for cleaning up build tarballs, and then putting them into
#' svn (and emailing the authors to let them know this - when they
#' already have an account)
#' @inheritParams package_name
#' @param svnDir Directory of the Rpacks checkout
#' @param copyToSvnDir whether to copy the files to the SVN directory
clean <- function(tarball, svnDir=proj_path("Rpacks"), copyToSvnDir=TRUE,
                  svnAccountExists=FALSE)
{
    ## 1st re-run the checker from Dan to make sure we have the right thing...
    ## TODO: call Dans checker here?

    ## access the tarball
    if (endsWith(tarball, ".tar.gz"))
        untar(tarball)
    ## get the name of the actual dir that tarball will unpack to
    dir <- .getShortPkgName(tarball)
    .cleanGIT(dir)
    ## ^\\. directories
    .cleanDotdirs(dir)
    ## clean up DESCRIPTION file
    .cleanDESCRIPTION(dir)
    ## remove build and inst/doc dirs
    .removeUnwantedDirs(dir)
    ## remove unwanted files    
    .removeUnwantedFiles(dir)
    ## cp the dir to a default svn dir.
    if(copyToSvnDir){
        file.copy(from=dir, to=svnDir, recursive=TRUE)
    }
    ## cleanup
    unlink(dir, recursive=TRUE)

    file.path(svnDir, dir)
}



############################################################################
#### Test example for how I want this to work:
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## use helper argument for testing...

## clean(tarball, copyToSvnDir=FALSE)

## if we know that the user has an svn account, then I can just email
## them at the same time that we add their code to the repos.
## clean(tarball, svnAccountExists=TRUE)


## helper for making paths
.makeFullPaths <- function(x, name){
 file.path(name, unlist(x))
}


#' Extract a packages name from a tarball
#'
#' @param tarball package tarball
#' @return the package name
#' @examples
#' pkg <- system.file(package="BiocContributions",
#'   "testpackages", "RNASeqPower_1.11.0.tar.gz")
#' package_name(pkg)
package_name <- function(tarball) {
    desc <- readDESCRIPTION(tarball)
    desc$Package
}

#' Clean and copy a Data Experiment package
#'
#' @param svn_pkgs the location of Data Experiment \sQuote{pkgs} checkout.
#' @param svn_data_store the location of Data Experiment
#'     \sQuote{data_store} checkout.
#' @inheritParams package_name
#' @return File paths to the copied locations (invisibly).
#' @examples
#' \dontrun{
#' pkg <- system.file(package="BiocContributions",
#'   "testpackages", "RNASeqPower_1.11.0.tar.gz")
#' clean_data_package(pkg)
#' }
clean_data_package <-
    function(tarball, svn_pkgs = proj_path("experiment/pkgs"),
             svn_data_store = proj_path("experiment/data_store"),
             data_dirs = c("data", "inst/extdata")) {

    desc <- readDESCRIPTION(tarball)

    # Remove the Packaged field
    desc$Packaged <- NULL

    # Extract the tarball and cleanup afterwards
    if (endsWith(tarball, ".tar.gz")) {
        untar(tarball, exdir = tempdir())
        pkg_dir <- file.path(tempdir(), desc$Package)
        on.exit(unlink(pkg_dir, recursive = TRUE))
    } else {
        pkg_dir <- tarball
    }

    # Get all extracted files
    files <- dir(pkg_dir, recursive = TRUE)

    object_files <- "src/.*\\.(o|sl|so|dylib|a|dll|def)$"

    # find all unwanted files
    unwanted <- grepl(
        paste0("^", paste0(collapse = "|",
                    c("DESCRIPTION", "inst/doc", "build", "\\.git", object_files))),
        files)

    files <- files[!unwanted]

    # find data directories
    is_data <- grepl(
        paste0("^", paste0(collapse="|", data_dirs)),
        files)

    data_files <- file.path(desc$Package, files[is_data])

    non_data_files <- file.path(desc$Package, files[!is_data])

    copy_files <- function(from, to) {
        # create all directories in the new location
        lapply(unique(dirname(to)), dir.create, recursive = TRUE, showWarnings = FALSE)

        Map(file.copy, from, to)
    }

    # copy non data files to the pkgs
    copy_files(file.path(dirname(pkg_dir), non_data_files),
        file.path(svn_pkgs, non_data_files))

    # copy the data files to the data store
    copy_files(file.path(dirname(pkg_dir), data_files),
        file.path(svn_data_store, data_files))

    # write the data paths in external_data_store.txt
    writeLines(sub("^[^/]+/", "", unique(dirname(data_files))),
        file.path(svn_pkgs, desc$Package, "external_data_store.txt"))

    # write the modified description
    write.dcf(desc, file.path(svn_pkgs, desc$Package, "DESCRIPTION"))

    invisible(c(file.path(svn_pkgs, desc$Package),
        file.path(svn_data_store, desc$Package)))
}
