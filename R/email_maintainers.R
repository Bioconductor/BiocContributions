# Another set of utilities for emailing maintainers (mtrs) of 
# broken packages. Uses slightly different utility functions for
# sending email than email.R. 

.getMtr <- function(package, software=TRUE)
{
    if (software)
        repos <- "bioc"
    else
        repos <- "data-experiment"
    url <- sprintf("http://master.bioconductor.org/checkResults/devel/%s-LATEST/meat-index.txt", 
        repos)
    txt <- content(GET(url))
    lines <- strsplit(txt, "\n")[[1]]
    curkpkg <- NULL
    scanMode <- FALSE
    for (line in lines)
    {
        if (line == sprintf("Package: %s", package))
            scanMode = TRUE
        if (scanMode && grepl("^MaintainerEmail:", line))
            return(sub("^MaintainerEmail: ", "", line))
    }
    stop("Couldn't find the maintainer!")
}

.getPackageFails <- function(package, software=TRUE)
{
    if (software)
        repos = "bioc"
    else
        repos = "data-experiment"
    ret <- list()
    for (version in c("release", "devel"))
    {
        url <- paste0("http://master.bioconductor.org/checkResults/",
            version, "/", repos, "-LATEST/STATUS_DB.txt")
        status_txt <- content(GET(url))
        lines <- strsplit(status_txt, "\n")[[1]]
        raw <- lines[grep(paste0("^", package, "#"), lines)]
        j <- unlist(strsplit(raw, " ")) 
        results <- unique(j[c(rep(FALSE,TRUE), TRUE)])
        if (length(results))
            results <- sort(results)
        results <-  results[!grepl("NotNeeded|skipped|OK", results)]
        if (length(results) && !(length(results) == 1 && results == "OK"))
            ret[[version]] <- results
    }
    if(is.null(names(ret)))
        stop("This package has no issues!")
    ret
}


failmail <- function(package, software=TRUE, from=getOption("fromEmail",
    "dtenenba@fredhutch.org"), sig=getOption("mail.sig", "Dan"),
    subject=sprintf("%s build problem", package), preview=TRUE,
    bccme=TRUE)
{
    if (!require(sendmailR)) stop("This function requires the sendmailR package.")
    if (is.null(getOption("email.options", NULL)))
        stop("Please set options(email.options). See ?sendmailR::sendmail_options")
    if (software)
        repos = "bioc"
    else
        repos = "data-experiment"
    results <- .getPackageFails(package, software)
    to <- .getMtr(package, software)
    msg <- sprintf("Hi,\n\nThere's an issue with %s on the build system.\n\n", package)
    for (version in c("release", "devel"))
    {
        if (!is.null(results[[version]]))
        {
            msg <- sprintf("%sIn %s, build results are %s on one or more platforms.\n\n",
                msg, version, paste(results[[version]], collapse=", "))
            msg <- sprintf("%sSee http://bioconductor.org/checkResults/%s/%s-LATEST/%s/\n",
                msg, version, repos, package)
            msg <- paste0(msg, "for more information.\n\n")
        }
    }
    custom <- c()
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> master
    cat("Enter a custom message y/N?")
    ans <- readLines(n=1)
    if (tolower(ans) == "y")
    {
        t <- tempfile(paste(package, "failmail", as.integer(Sys.time()), sep=")"))
        if (file.exists(t))
            unlink(t)
        file.edit(t)
        if (file.exists(t))
            msg <- paste0(msg, paste(readLines(t), collapse="\n"), "\n\n")

    }
<<<<<<< HEAD
=======
=======
    cat("Enter a custom message, . on a line by itself to end.\n")
    while(TRUE)
    {
        line <- readLines(n=1)
        if (line == ".")
            break
        custom <- append(custom, line)
    }
    if (length(custom))
        msg <- paste0(msg, paste(custom, collapse="\n"), "\n\n")
>>>>>>> email
>>>>>>> master

    msg <- paste0(msg, "Please take a look and fix this as soon as you can.\n")
    msg <- paste0(msg, "Let me know if you have any questions.\n\nThanks,\n", sig, "\n")
    if (preview)
    {
        cat ("Mesage preview:\n-------\n")
        cat(sprintf("From: %s\nTo: %s\nSubject: %s\n\n%s",
            from, to, subject, msg))
        cat("---\nIs this ok (y/N)? ")
        ans <- readLines(n=1)
        if (!tolower(ans) == "y")
            return(invisible(NULL))
    }
    bcc <- c()
    if (bccme)
        bcc <- from
    sendmail(from, to, subject, msg,
        bcc = bcc,
        headers=list("X-BiocContributions"="TRUE"),
        control=getOption("email.options"))
    invisible(NULL)
}