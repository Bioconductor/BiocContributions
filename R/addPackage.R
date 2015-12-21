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

readDESCRIPTION <- function(tarball) {
    ls <- untar(tarball, list = TRUE)
    description <- ls[basename(ls) == "DESCRIPTION"]

    if (length(description) == 0) stop("No DESCRIPTION")
    if (length(description) > 1) {
        description <- description[which.min(nchar(description))]
        message("Multiple descriptions: using ", description)
    }

    untar(tarball, files = description, exdir = tempdir())
    res <- read.dcf(file.path(tempdir(), description), all = TRUE)

    # generate a maintainer from Authors@R if none specified
    if (is.null(res$Maintainer)) {
      authors <- utils:::.read_authors_at_R_field(res$`Authors@R`)
      res$Maintainer <- Filter(function(x) "cre" %in% x$role, authors)
    }
    structure(res,
              class = c("description", "data.frame"))
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


## helper for making paths
.makeFullPaths <- function(x, name){
 file.path(name, unlist(x))
}


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
        basePaths <- character()
        system(paste0('touch ',extDataStore))
        ## and then check the following:
        dataDir <- file.path(svd1,'data')
        if(file.exists(dataDir)){
            unlink(dataDir, recursive=TRUE)
            basePaths <- c(basePaths, 'data')
        }
        extdataDir <- file.path(svd1,'inst','extdata')
        if(file.exists(extdataDir)){
            unlink(extdataDir, recursive=TRUE)
            basePaths <- c(basePaths, 'inst/extdata')
        }
        writeLines(basePaths, con = extDataCon)
        close(extDataCon)
        ## And here it just removes pretty much everything that isn't
        ## the stuff we tossed out in svd1...
        file.copy(from=dir, to=svnDir2, recursive=TRUE)
        svd2 <- file.path(svnDir2, dir)
        ## get all contents of current dir
        contents <- dir(svd2, include.dirs=TRUE ,recursive=TRUE)
        ## then make all contents and paths "fully pathed"
        contents <- file.path(svd2, contents)        
        fullBasePaths <- file.path(svd2, basePaths)
        ## Then we have to get all the indiv, stuff in each basePath
        pathList <- lapply(fullBasePaths, FUN='dir', recursive=TRUE)
        pathList <- mapply(.makeFullPaths, pathList, fullBasePaths)
        paths <- unlist(pathList, use.names=FALSE) 
        paths <- unique(c(paths, fullBasePaths))        
        ## exception for 'inst':
        if(any(grepl('inst', basePaths))){
            instPath <- file.path(svd2, 'inst')
            paths <- c(instPath, paths)
        }
        ## filter out the paths we want to keep.
        contents <- contents[!contents %in% paths]
        ## and throw out the rest.
        unlink(contents, recursive=TRUE)
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
## tarball = 'CopyNumber450kData_0.99.1.tar.gz'; library(BiocContributions); debug(BiocContributions:::cleanDataPkg)
## cleanDataPkg(tarball, svnDir1="~/tasks/PkgReviews/AboutToAdd/expr/test/pkgs", svnDir2="~/tasks/PkgReviews/AboutToAdd/expr/test/data")


## tarball = 'Affymoe430Expr_0.99.0.tar.gz'; library(BiocContributions); debug(BiocContributions:::cleanDataPkg); cleanDataPkg(tarball, svnDir1="~/tasks/PkgReviews/AboutToAdd/expr/test/pkgs", svnDir2="~/tasks/PkgReviews/AboutToAdd/expr/test/data")
