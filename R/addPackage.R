## This is where code that is used for cleaning up a package and
## adding it to svn will live.

## I want to follow BiocCheck's lead here and do a cleanup command like:
## R CMD cleanup

## cleanup will then do the following:
## drop and lines from DESCRIPTION thet start with "packaged"
## rm -rf any build directories or  /inst/doc


## cp -r the package to the svn repos
## put the package into the most recent manifest


.getShortPkgName <- function(tarball){
    sep <- .Platform$file.sep
    notTar <- paste("^",sep,".*",sep, sep="")
    tar <-  sub(notTar,"",tarball, perl=TRUE)
    sub("_.*gz","", tar, perl=TRUE)
}

.cleanDESCRIPTION <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    DESC <- DESC[,!grepl("Packaged",colnames(DESC)),drop=FALSE]
    write.dcf(DESC, file=dirPath)
}

.removeUnwantedDirs <- function(dir){
    instDoc <- file.path(dir, "inst", "doc")
    if(file.exists(instDoc)){
        unlink(instDoc, recursive=TRUE)
    }
    buildDir <- file.path(dir, "build")
    if(file.exists(buildDir)){
        unlink(buildDir, recursive=TRUE)
    }
}

clean <- function(tarball, svnDir="~/proj/Rpacks/", copyToSvnDir=TRUE){
    ## 1st re-run the checker from Dan to make sure we have the right thing...
    ## TODO: call Dans checker here.

    ## make sure we are in unix (otherwise default arg for svnDir is no good)
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}
  
    ## access the tarball
    untar(tarball)
    ## get the name of the actual dir that tarball will unpack to
    dir <- .getShortPkgName(tarball)
    ## clean up DESCRIPTION file
    .cleanDESCRIPTION(dir)
    ## remove build and inst/doc dirs
    .removeUnwantedDirs(dir)
    ## cp the dir to a default svn dir.
    if(copyToSvnDir){
        file.copy(from=dir, to=svnDir, recursive=TRUE)
    }
    ## TODO: add param for svnAccountExists=TRUE, and if true, call
    ## emailExistingUser(tarball)
}



############################################################################
#### Test example for how I want this to work:
##  library(BiocContributions); tarball <- system.file("testpackages", "savR_0.99.1.tar.gz", package="BiocContributions");

## use helper argument for testing...

## clean(tarball, copyToSvnDir=FALSE)


