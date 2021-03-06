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
    require(httr)
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
    require(httr)
    if (software)
        repos = "bioc"
    else
        repos = "data-experiment"
    ret <- list()
    notfound <- c()
    for (version in c("release", "devel"))
    {
        url <- paste0("http://master.bioconductor.org/checkResults/",
            version, "/", repos, "-LATEST/STATUS_DB.txt")
        status_txt <- content(GET(url))
        lines <- strsplit(status_txt, "\n")[[1]]
        raw <- lines[grep(paste0("^", package, "#"), lines)]
        if (!length(raw))
            notfound <- append(notfound, TRUE)
        j <- unlist(strsplit(raw, " ")) 
        results <- unique(j[c(rep(FALSE,TRUE), TRUE)])
        if (length(results))
            results <- sort(results)
        results <-  results[!grepl("NotNeeded|skipped|OK", results)]
        if (length(results) && !(length(results) == 1 && results == "OK"))
            ret[[version]] <- results
    }
    if (all(notfound) && (length(notfound) > 1))
        stop(sprintf("Package %s not found.", package))
    if(is.null(names(ret)))
        stop("This package has no issues!")
    ret
}

## TODO - add special text (and arg to activate it) 
## when release date is approaching
## and error/warning MUST be fixed by some date.
failmail <- function(package, software=TRUE, from=getOption("fromEmail",
    "dtenenba@fredhutch.org"), sig=getOption("mail.sig", "Dan"),
    subject=sprintf("%s build problem", package), preview=TRUE,
    bccme=TRUE)
{
    if (!require(mailR)) 
    {
        message("Installing required custom version of mailR package...")
        library(devtools)
        BiocInstaller::biocLite("dtenenba/mailR",
            ref="useWithBiocContributions")
        library(mailR)
    }    
    if (is.null(getOption("email.options", NULL)))
        stop("Please set options(email.options) to a list, see ?failmail")
    package <- sub("\\/$", "", package)
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
    cat("Add custom message [(y)es/(N)o/use (e)ditor]? ")
    line <- readLines(n=1)
    if (tolower(line) == "y")
    {
        cat("Enter a custom message, . on a line by itself to end.\n")
        custom <- c()
        while(TRUE)
        {
            line <- readLines(n=1)
            if (line == ".")
                break
            custom <- append(custom, line)
        }
        if (length(custom))
            msg <- paste0(msg, paste(custom, collapse="\n"), "\n\n")
    } else if (tolower(line) == "e")
    {
        tmpfile <- tempfile(package)
        # if (file.exists(tmpfile))
        #     unlink(tmpfile)
        file.edit(tmpfile)
        cat("Press ENTER when done editing. ")
        line <- readLines(n=1)
        if (file.exists(tmpfile) && file.size(tmpfile) > 0)
        {
            line <- readLines(n=1)
            msg <- paste0(msg, 
                paste(readLines(tmpfile, warn=FALSE), 
                collapse="\n"), "\n\n")
        }
    }


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
    msg <- paste0("<pre>", msg, "</pre>")
    bcc <- c()
    if (bccme)
        bcc <- from
    debug <- FALSE
    if (!is.null(getOption("email.options")$debug))
        debug=getOption("email.options")$debug
    res <- send.mail(from,
        to=to,
        subject, msg,
         bcc = bcc,
         headers=list("X-BiocContributions"="TRUE"),
        authenticate=TRUE,
        html=TRUE,
        smtp=getOption("email.options"),
        debug=debug)
    if (getOption("email.options")[["port"]] == 1025)
            cat("Using a test email server, email not actually sent.")
    res
} 
