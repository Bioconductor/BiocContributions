## usage: installDeps("AnnotationDbi")
installDeps <- function(pkgName){
  d <- packageDescription(pkgName)
  dep <- paste(d$Depends, d$Imports, d$Suggests, sep=",")
  dep <- gsub("\n", "", dep)
  dep <- gsub(" ", "", dep)
  dep <- unlist(strsplit(dep, split=","))
  library(BiocInstaller)
  biocLite(dep)
}


## TODO: Make a version of this that runs from a tarball instead of
## from a package (that would have to be previously installed)
