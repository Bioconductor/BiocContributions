## Here I aim to formalize my code that currently processes any
## package that does not contain source code into mac and windows
## packages.

## Basically the R version of these three shell loops:
## for pkg in `ls ./fixed/*.tar.gz`
## do
##     echo $pkg
##     /home/mcarlson/RInstalls/R-302/bin/R CMD INSTALL --build $pkg
## #    R CMD INSTALL --build $pkg
## done

## for pkg in `ls ./*.tar.gz`
## do
##     echo $pkg
##     pkgName=${pkg%_R_x86_64-unknown-linux-gnu.tar.gz}
##     cp $pkg ${pkgName}.tgz
##     tar zxf $pkg
##     ##change Built: line in DESCRIPTION to say windows instead of unix
##     sed -i -e "s/unix/windows/g" */DESCRIPTION
## done

## for pkg in `ls ./*.tar.gz` ## lists dirs
## do 
##     pkgName=${pkg%_R_x86_64-unknown-linux-gnu.tar.gz}       ## ./savR_0.99.1
##     pkgReal=`echo $pkg | cut -d _ -f 1`                     ## ./savR
## #     echo $pkgName
## #     echo $pkgReal
##     rm -f ${pkgReal}/MD5 ## new line
##     zip -r ${pkgName}.zip  $pkgReal
##     rm -rf $pkgReal
##     rm -rf $pkg
## done



.getLongPkgName <- function(tarball){
    sep <- .Platform$file.sep
    notTar <- paste("^",sep,".*",sep, sep="")
    tar <-  sub(notTar,"",tarball, perl=TRUE)
    sub(".tar.gz","", tar, perl=TRUE)
}

.windowIzeDESCRIPTION <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    DESC[,'Built'] <- sub('unix', 'windows', DESC[,'Built']) 
    write.dcf(DESC, file=dirPath)
}


## So 1st thing is that this function is implicitly a unix only command.
makeBins <- function(tarball){
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}

    ###############################################
    ## use system to call R CMD INSTALL -- build
    cmd <- paste("R CMD INSTALL --build", tarball)
    system(cmd)

    ###############################################
    ## make the Mac binary:
    ## now get the unmangled package name
    pkg <- .getLongPkgName(tarball)
    builtPkg <- paste(pkg, "_R_x86_64-unknown-linux-gnu.tar.gz", sep="")
    macPkg <- paste(pkg,".tgz",sep="")
    file.copy(builtPkg, to=macPkg) 

    ###############################################
    ## make the Windows Binary
    ## untar the built package  ### tar zxf $pkg
    untar(builtPkg)
    
    ## also need the 'true' packagename (aka the dir name)
    pkgDir <- .getShortPkgName(tarball)

    ## and change important line in DESCRIPTION to be windows.
    .windowIzeDESCRIPTION(dir=pkgDir)

    ## remove any MD5 files
    MD5File <- file.path(pkgDir,"MD5")
    if(file.exists(MD5File)){
        unlink(MD5File)
    }
    ## zip up the tarball
    zip(paste(pkg,".zip",sep=""),files=pkgDir)

    ## remove the unpacked pkgDir along with the builtPkg
    if(file.exists(pkgDir)){
        unlink(pkgDir, recursive=TRUE)
    }
    if(file.exists(builtPkg)){
        unlink(builtPkg)
    }
}


##  library(BiocContributions); tarball <- system.file("testpackages", "hgu95av2.db_2.10.1.tar.gz", package="BiocContributions");

## makeBins(tarball)

