.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

.getPageContents <- function(biocVersion="3.1") {
    theurl <- paste0("http://www.bioconductor.org/checkResults/",biocVersion,
                     "/bioc-LATEST/")
    temp <- GET(theurl)
    html <- content(temp)
    html2 <- lapply(html["//tr"], xmlValue)
    html3 <- unlist(html2)
    stats <- head(html3,44)
    message(stats[1])
    html3 <- html3[-(1:44)]
    
    pkgInd <- grep("Package", html3)
    
    result <- lapply(pkgInd, function(x) {
        start <- x
        end <- x+7
        temp <- html3[start:end]
        p <- gsub( " .*$", "", temp[2] )
        moscato2 <- strsplit(grep("Windows", temp,value=TRUE),"x64")[[1]][2]
        morelia <- strsplit(grep("Mavericks", temp,value=TRUE),"x86_64")[[1]][2]
        petty <- strsplit(grep("Snow", temp,value=TRUE),"x86_64")[[1]][2]
        linux <- strsplit(grep("Linux", temp,value=TRUE),"x86_64")[[1]][2]
        
        list( pkgDetails = p, moscato2= .trim(moscato2), 
              morelia=.trim(morelia), petty=.trim(petty), 
              linux=.trim(linux))
    })
    pkgInf <- vapply(result, "[[", "", "pkgDetails")
    ## Some cleanup
    pkgInf <- gsub("\\.", "_", pkgInf)
    ## pkgInf <- strsplit(sub("\\W", "|", pkgInf),"\\|")
    pkgInf <- sub("\\W", "|", pkgInf)
    pkgInf <- gsub("_", "\\.", pkgInf)
    pkgInf <- strsplit(pkgInf,"\\|")
    
    pkg <- vapply(pkgInf, function(x){x[1]}, 'character')
    author <- vapply(pkgInf, function(x){x[2]}, 'character')

    linux <- vapply(result, "[[", "", "linux")
    petty <- vapply(result, "[[", "", "petty")
    morelia <- vapply(result, "[[", "", "morelia")
    moscato2 <- vapply(result, "[[", "", "moscato2")
    
    data.frame(pkg=pkg, author=author, linux=linux, morelia=morelia, 
                     moscato2=moscato2, petty=petty,
                     stringsAsFactors=FALSE)
}


.getIndiList <- function(start, end, df) {
    m1 <- min(grep(start, df[,1]))
    m2 <- max(grep(end, df[,1]))
    df[m1:m2, ]
}

.getErrorWarning <- function(reviewerPkgList, msg=c("ERROR","WARNINGS")) {
    petty <- grep(msg, reviewerPkgList[,"petty"])
    linux <- grep(msg, reviewerPkgList[,"linux"])
    morelia <- grep(msg, reviewerPkgList[,"morelia"])
    moscato2 <- grep(msg, reviewerPkgList[,"moscato2"])
    
    list(petty=reviewerPkgList[petty,1],
         linux=reviewerPkgList[linux,1],
         morelia=reviewerPkgList[morelia,1],
         moscato2=reviewerPkgList[moscato2,1])
}

.getEmail <- function(pkgName) {
    sapply(pkgName, function(p){
        url <- paste0("http://bioconductor.org/packages/3.1/bioc/html/",
                      p,".html")
        result <- GET(url)
        html <- content(result)
        html2 <- sapply(html["//p"], xmlValue)
        grep("Maintainer", html2, value=TRUE)
    })
}

getPackageRange <-  
    function(userName="Sonali", biocVersion ="3.1") {
    reviewer <- userName 
    df <- .getPageContents(biocVersion)
    df <- df[-nrow(df),]
    
    start <- switch(reviewer,
                    Dan= "a4", Herve="BUS" ,
                    Jim="deltaGseg", Marc="GeneRegionScan", Martin="IsoGeneGUI",
                    Nate="MSnID", Sonali="qpcrNorm", Val ="seqPattern")
    end <- switch(reviewer,
                  Dan= "bumphunter", Herve="DEGseq" ,
                  Jim="geneRecommender", Marc="isobar", Martin="MSnbase",
                  Nate="QDNAseq", Sonali="seqLogo", Val ="zlibbioc")
    
    reviewerPkgList <- .getIndiList(start, end, df)
    
    errorlist <- .getErrorWarning(reviewerPkgList, msg="ERROR")
    warningslist <- .getErrorWarning(reviewerPkgList, msg="WARNING")
    
    full_errorlist <- unlist(errorlist)
    names(full_errorlist) <- NULL
    
    full_warnlist <- unlist(warningslist)
    names(full_warnlist) <- NULL
        
    message("Total no of packages assigned to ", reviewer," : ",
            nrow(reviewerPkgList))
    message("No of Packages with Error:", length(unique(full_errorlist)))
    message("No of Packages with Warnings:", length(unique(full_warnlist)))
    
    list(reviwerPkgList=reviewerPkgList, errorlist=errorlist, 
         warningslist=warningslist)
}

#rlist <- c("Dan","Herve","Jim","Marc","Martin","Nate","Sonali", "Val")





