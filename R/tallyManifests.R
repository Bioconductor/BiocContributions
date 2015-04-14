## Used to count and plot the amount of packages in the respos.

## helper that generates names for manifests.  You have to update this
## each release in order to get the current totals!
.makeManifestNames <- function(path, appendPath=TRUE){
    files <- dir(path)[grepl(glob2rx("bioc_*.manifest"),dir(path))]
    if(appendPath==TRUE){
        paste0(path,files)
    }else{
        files
    }
}

.makeExpManifestNames <- function(path, appendPath=TRUE){
    files <- dir(path)[grepl(glob2rx("bioc-data-experiment.*.manifest"),
                             dir(path))]
    if(appendPath==TRUE){
        paste0(path,files)
    }else{
        files
    }
}



## Helper to just read in one manifest and get the total number of packages.
.scanMani <- function(file){
    res <- scan(file, what="character",skip=1, quiet=TRUE)
    table(grepl("Package", res))[["TRUE"]]
}


## This extracts the package totals based on existing manifests
getPackageTotals <- function(path = "~/proj/Rpacks/"){
    manis <- .makeManifestNames(path)
    maniNames <- .makeManifestNames(path, appendPath=FALSE)
    ## Always update the most recent manifest file (at the very least)
    lastMani <- manis[length(manis)]
    system(paste0("svn up ", lastMani))
    setNames(unlist(lapply(manis, .scanMani)), maniNames)
}

## getPackageTotals()


## And this plots the package totals based on existing manifests
plotPackageTotals <- function(path = "~/proj/Rpacks/"){
    totals <- getPackageTotals(path)
    plot(totals)
    abline(a=100,b=20,col="red")
}

## plotPackageTotals()


getPackageDeltas <- function(path = "~/proj/Rpacks/"){
    tots <- getPackageTotals(path)
    res <- integer()
    names <- character()
    for(i in seq_along(tots)){
        res[i] <- tots[i+1] - tots[i]
        names[i] <- paste0(names(tots[i]),"_TO_",names(tots[i+1]))
        names(res) <- names
    }
    res <- res[1:(length(res)-1)]
    res
}
