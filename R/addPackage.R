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
    sep <- .Platform$file.sep
    notTar <- paste("^",sep,".*",sep, sep="")
    tar <-  sub(notTar,"",tarball, perl=TRUE)
    sub("_.*gz","", tar, perl=TRUE)
}

## This throws away unwanted extra lines and junk from the DESCRIPTION file
.cleanDESCRIPTION <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    DESC <- DESC[,!grepl("Packaged",colnames(DESC)),drop=FALSE]
    write.dcf(DESC, file=dirPath)
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
}

## This is for cleaning up build tarballs, and then putting them into
## svn (and emailing the authors to let them know this - when they
## already have an account)
clean <- function(tarball, svnDir="~/proj/Rpacks/", copyToSvnDir=TRUE,
                  svnAccountExists=FALSE){
    ## 1st re-run the checker from Dan to make sure we have the right thing...
    ## TODO: call Dans checker here?

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
    ## email, but only if the user is known to exist already..
    if(svnAccountExists == TRUE){
        emailExistingUser(tarball)
    }
}



############################################################################
#### Test example for how I want this to work:
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## use helper argument for testing...

## clean(tarball, copyToSvnDir=FALSE)

## if we know that the user has an svn account, then I can just email
## them at the same time that we add their code to the repos.
## clean(tarball, svnAccountExists=TRUE)


