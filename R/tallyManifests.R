## Used to count and plot the amount of packages in the respos.
## path <- "~/proj/Rpacks/"

## range <- 6:9
## manis1 <- paste0(path, "bioc_1.",range,".manifest")
## range <- 0:14
## manis2 <- paste0(path, "bioc_2.",range,".manifest")
## manis <- c(manis1, manis2)

## scanMani <- function(file){
##     res <- scan(file, what="character",skip=1)
##     table(grepl("Package", res))[["TRUE"]]
## }

## scans <- unlist(lapply(manis, scanMani))

## plot(scans)

## ## line if the slope was twenty (which is about what we started at
## abline(a=100,b=20,col="red")


## TODO: Make a function that makes the plot, and another function
## that makes just a named vector with the counts for each release.


