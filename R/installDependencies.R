## Code to install dependencies even if it's a new tarball that is not
## yet in the project manifest/biocLite.


## Helper based on code that Dan originally needed for BiocCheck
.depToCharacter <- function(input){
    if (is.null(input)) return(NULL)
    output <- gsub("\\s", "", input)
    output <- gsub("\\([^)]*\\)", "", output)
    if(dim(output)[2] ==0){
        return(NULL)
    }else{
        res <- strsplit(output, ",")[[1]]
        res[which(res != "R")]
    }
}


## Helper to extract all dependencies and the return them as a character vector
.extractDependencies <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    deps <- .depToCharacter(DESC[,grepl("Depends",colnames(DESC)),drop=FALSE])
    sugs <- .depToCharacter(DESC[,grepl("Suggests",colnames(DESC)),drop=FALSE])
    imps <- .depToCharacter(DESC[,grepl("Imports",colnames(DESC)),drop=FALSE])
    enhs <- .depToCharacter(DESC[,grepl("Enhances",colnames(DESC)),drop=FALSE])
    lnkt <- .depToCharacter(DESC[,grepl("LinkingTo",colnames(DESC)),drop=FALSE])
    res <- c(deps, sugs, imps, enhs, lnkt)
    if(length(res) == 0){
        stop("there are no dependencies to install.")
    }
    res
}

installDeps <- function(tarball){
    untar(tarball)
    dir <- .getShortPkgName(tarball)
    dep <- .extractDependencies(dir)    
    require(BiocInstaller)
    biocLite(dep)
}


## library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");
## installDeps(tarball)

##  library(BiocContributions); installDeps('genomationData_0.99.tar.gz')
