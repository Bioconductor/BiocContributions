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
    
    data.frame(pkg=pkg, linux=linux, mavericks=morelia, 
                     windows=moscato2, snow=petty,
                     stringsAsFactors=FALSE)
}


.getIndiList <- function(start, end, df) {
    m1 <- min(grep(start, df[,1]))
    m2 <- max(grep(end, df[,1]))
    df[m1:m2, ]
}

.getErrorWarning <- function(reviewerPkgList, msg=c("ERROR","WARNINGS")) {
    str <- c("windows", "linux", "snow","mavericks")
    df <- data.frame(platform=character(0),
        pkgName=character(0), email=character(0))
    for (i in 1:length(str)){
        s1 <- str[i]
        ind <- grep(msg, reviewerPkgList[,s1])
        if(length(ind)!=0){
            pkgName <- reviewerPkgList[ind,1]
            email <- .getEmail(pkgName)
            names(email) <- NULL
            tedf <- data.frame(platform=rep(as.character(s1),length(pkgName)),
               pkgName=pkgName, email=email)
            df <- rbind(df, tedf)   
         }
    }
    df
}

.getEmail <- function(pkgName) {
    sapply(pkgName, function(p){
        url <- paste0("http://bioconductor.org/packages/3.1/bioc/html/",
                      p,".html")
        result <- GET(url)
        html <- content(result)
        html2 <- sapply(html["//p"], xmlValue)
        grep("Maintainer:", html2, value=TRUE)
    })
}

getPackageRange <-  
    function(userName="Sonali", biocVersion ="3.1") {
    reviewer <- userName 
    df <- .getPageContents(biocVersion)
    df <- df[-(which(df$pkg=="Last")),]
    
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
    warnlist <- .getErrorWarning(reviewerPkgList, msg="WARNING")
    
    message("Total no of packages assigned to ", reviewer," : ",
            nrow(reviewerPkgList))
    message("No of Packages with Error:", length(unique(errorlist$pkgName)))
    message("No of Packages with Warnings:", length(unique(warnlist$pkgName)))
    
    list(reviewerPkgList=reviewerPkgList, errorlist=errorlist, 
         warningslist=warnlist)
}

#rlist <- c("Dan","Herve","Jim","Marc","Martin","Nate","Sonali", "Val")
#res= lapply(rlist, function(r) getPackageRange(r, "3.1"))




