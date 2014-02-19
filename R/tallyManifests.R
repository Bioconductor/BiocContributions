## Used to count and plot the amount of packages in the respos.

## helper that generates names for manifests.  You have to update this
## each release in order to get the current totals!
.makeManifestNames <- function(path){
    range1 <- 6:9
    manis1 <- paste0(path, "bioc_1.",range1,".manifest")
    range2 <- 0:14
    manis2 <- paste0(path, "bioc_2.",range2,".manifest")
    c(manis1, manis2)
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



