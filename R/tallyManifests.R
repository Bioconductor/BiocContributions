## Used to count and plot the amount of packages in the respos.

## helper that generates names for manifests.  You have to update this
## each release in order to get the current totals!
.makeManifestNames <- function(path){
    range1 <- 6:9
    manis1 <- paste0(path, "bioc_1.",range1,".manifest")
    range2 <- 0:14
    manis2 <- paste0(path, "bioc_2.",range2,".manifest")
    range3 <- 0:2
    manis3 <- paste0(path, "bioc_3.",range3,".manifest")
    c(manis1, manis2, manis3)
}

.makeExpManifestNames <- function(path){
    range2 <- 10:14
    manis2 <- paste0(path, "bioc-data-experiment.2.",range2,".manifest")
    range3 <- 0:1
    manis3 <- paste0(path, "bioc-data-experiment.3.",range3,".manifest")
    c(manis2, manis3)
}




## Helper to just read in one manifest and get the total number of packages.
.scanMani <- function(file){
    res <- scan(file, what="character",skip=1, quiet=TRUE)
    table(grepl("Package", res))[["TRUE"]]
}


## This extracts the package totals based on existing manifests
getPackageTotals <- function(path = "~/proj/Rpacks/"){
    manis <- .makeManifestNames(path)
    maniNames <- .makeManifestNames("")
    ## Always update the most recent manifest file (at the very least)
    lastMani <- manis[length(manis)]
    system(paste0("svn up ", lastMani))
    setNames(unlist(lapply(manis, .scanMani)), maniNames)
}

## getPackageTotals()


## And this plots the package totals based on existing manifests
plotPackageTotals <- function(path = "~/proj/Rpacks/"){
    totals <- getPackageTotals()
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
