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

.removeUnwantedFiles <- function(dir){
    srcDir <- file.path(dir, "src")
    if(file.exists(srcDir)){
        unlink(file.path(srcDir,"*.o"))
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
    ## remove unwanted files    
    .removeUnwantedFiles(dir)
    ## cp the dir to a default svn dir.
    if(copyToSvnDir){
        file.copy(from=dir, to=svnDir, recursive=TRUE)
    }
    ## email, but only if the user is known to exist already..
    if(svnAccountExists == TRUE){
        emailExistingUser(tarball)
    }
    ## cleanup
    unlink(dir, recursive=TRUE)
}



############################################################################
#### Test example for how I want this to work:
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## use helper argument for testing...

## clean(tarball, copyToSvnDir=FALSE)

## if we know that the user has an svn account, then I can just email
## them at the same time that we add their code to the repos.
## clean(tarball, svnAccountExists=TRUE)






###########################################################################
## Another clean function (this time for cleaning data packages)
cleanDataPkg <- function(tarball,
                         svnDir1="~/proj/experiment/pkgs",
                         svnDir2="~/proj/experiment/data_store",
                         copyToSvnDir=TRUE,
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
    ## remove unwanted files    
    .removeUnwantedFiles(dir)

    ## cp the dir to a default svn dir.
    if(copyToSvnDir){
        file.copy(from=dir, to=svnDir1, recursive=TRUE)
        svd1 <- file.path(svnDir1, dir)
        ## for now just touch this file.
        extDataStore <- file.path(svd1,'external_data_store.txt')
        extDataCon <- file(extDataStore)
        paths <- character()
        system(paste0('touch ',extDataStore))
        ## and then check the following:
        dataDir <- file.path(svd1,'data')
        if(file.exists(dataDir)){
            unlink(dataDir, recursive=TRUE)
            paths <- c(paths, 'data')
        }
        extdataDir <- file.path(svd1,'inst','extdata')
        if(file.exists(extdataDir)){
            unlink(extdataDir, recursive=TRUE)
            paths <- c(paths, 'inst/extdata')
        }
        writeLines(paths, con = extDataCon)       
        ## And here it just removes pretty much everything that isn't
        ## the stuff we tossed out in svd1...
        file.copy(from=dir, to=svnDir2, recursive=TRUE)
        svd2 <- file.path(svnDir2, dir)
        contents <- dir(svd2, recursive=TRUE)
        contents <- file.path(svd2, contents)
        paths <- file.path(svd2, paths)
        ## filter out the keepers.
        contents <- contents[!contents %in% paths]
        ## and throw out the rest.
        unlink(contents)
    }
    ## email, but only if the user is known to exist already..
    if(svnAccountExists == TRUE){
        emailExistingUser(tarball)
    }
    ## cleanup
    unlink(dir, recursive=TRUE)
}


## ## need an example here (TODO: add an example to inst/testpackages)
## cleanDataPkg(tarball)


## local test
## tarball = 'CopyNumber450k_0.99.4.tar.gz'; library(BiocContributions); cleanDataPkg(tarball, svnDir1="~/tasks/PkgReviews/AboutToAdd/expr/test/data", svnDir2="~/tasks/PkgReviews/AboutToAdd/expr/test/pkgs")
