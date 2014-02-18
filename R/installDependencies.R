## usage: installDeps("AnnotationDbi")
## installDeps <- function(pkgName){
##   d <- packageDescription(pkgName)
##   dep <- paste(d$Depends, d$Imports, d$Suggests, sep=",")
##   dep <- gsub("\n", "", dep)
##   dep <- gsub(" ", "", dep)
##   dep <- unlist(strsplit(dep, split=","))
##   library(BiocInstaller)
##   biocLite(dep)
## }



## Helper based on code that Dan originally needed for BiocCheck
.depToCharacter <- function(input){
    if (is.null(input)) return(NULL)
    output <- gsub("\\s", "", input)
    output <- gsub("\\([^)]*\\)", "", output)
    res <- strsplit(output, ",")[[1]]
    res[which(res != "R")]
}

## Helper to extract all dependencies and the return them as a character vector
.extractDependencies <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    deps <- .depToCharacter(DESC[,grepl("Depends",colnames(DESC)),drop=FALSE])
    sugs <- .depToCharacter(DESC[,grepl("Suggests",colnames(DESC)),drop=FALSE])
    imps <- .depToCharacter(DESC[,grepl("Imports",colnames(DESC)),drop=FALSE])
    c(deps, sugs, imps)
}

installDeps <- function(pkgName){
    untar(tarball)
    dir <- .getShortPkgName(tarball)
    dep <- .extractDependencies(dir)    
    library(BiocInstaller)
    biocLite(dep)
}


## library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");
## installDeps(tarball)
