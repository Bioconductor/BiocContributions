CreatePackageAssignmentEmail <- function(assignInTracker=FALSE, secret="~/bioconductorseattle-gmail.json")
{
    ## Retrieve unassigned packages
    pkgs <- unassigned_packages()

    ## Generate code used for assigning packages
    code <- assign_new_packages(pkgs)

    ## Generate package assignment email
    email <- package_assignment_email(pkgs, code)

    ## Create a new draft email with assignment content (in Gmail drafts)
    gmailr::gmail_auth(scope="compose", secret_file=secret)
    gmailr::create_draft(email)

    if (assignInTracker) {
        assign_packages(pkgs, code)
        return (invisible(NULL))
    } else
        return (list(pkgs=pkgs, code=code))
}


DownloadNewPackageTarballs <- function(pre=pre_accepted_packages())
{
    ## Download tarballs:
    files <- unlist(lapply(pre$id, download, overwrite=T), recursive=F)
    filenames <- basename(names(files))

    cat('\n', filenames, sep='\n')

    return (list(pre=pre, files=files, filenames=filenames))
}


.LoadNewPackagesMetadata <- function(metadata.dir=".", filename.base="new-packages-metadata_")
{
    fileExtension = "RData"
    filenames = sort(grep("^.*?" %_% filename.base %_% "\\d{8}" %_% "\\." %_% fileExtension %_% "$", list.files(metadata.dir, full.names=TRUE), value=TRUE), decreasing=TRUE)

    e = new.env()
    if (length(filenames) != 0) {
      load(filenames[1], envir=e) # File extension "RData".
    }

    return (e)
}


.CheckUsersCredentials <- function(metadata, credPath="~/bioconductor.authz")
{
    d <- readLines(credPath)

    f <- metadata

    us <- sapply(f$filenames, function(x) {
        ms <- maintainers(x)
        u <- c()
        for (m in ms) {
            u <- c(u, tolower(paste(substr(m$given, 1, 1), m$family, sep=".")))
        }

        u
    })

    ex <- sapply(us, function(x) {
        any(sapply(x, grepl, d, simplify=T))
    })

    list(usernames=us, existing=ex)
}


ManageNewPackagesCredentials <- function(metadata)
{
    m <- new.env()
    if (missing(metadata))
        m <- .LoadNewPackagesMetadata()
    else
       m$f = metadata

    f = m$f

    cat('\n', "##### Check authorization file for existing users.", '\n\n', sep='')

    creds = .CheckUsersCredentials(f)
    print(creds)

    cat('\n', "##### Template e-mail to Carl Benson <scicomp@fhcrc.org>", '\n\n', sep='')

    email1a <- readLines(textConnection('
Subject: New SVN users for Hedgehog
From: Package Maintainer <packages@bioconductor.org>
To: scicomp@fhcrc.org

Hi Carl,

Could you please create new SVN account(s) on Hedgehog for
'))
    cat(email1a, sep='\n')

    dev.null <- sapply(f$filenames, function(x) print(maintainers(x)))

    email1b <- readLines(textConnection('
Thanks,

Martin
'))
    cat(email1b, sep='\n')

    cat('\n', "##### Create draft e-mails to maintainers", '\n\n', sep='')
    cat("gmailr::gmail_auth(scope='compose')\n")
    dev.null <- mapply(names(creds$usernames), creds$usernames, FUN=function(x, y) {
        cat("gmailr::create_draft(emailExistingUser('", x, "')\n", sep='')
        cat("gmailr::create_draft(emailNewUser('", x, "', userId='", y, "', password='XXXXXXXXX')\n", sep='')
    })

    cat('\n', "##### Accept packages", '\n\n', sep='')

    dev.null <- mapply(f$pre$id, f$filenames, FUN=function(x, y) {
        cat("accept_package(", x, ", '", y, "'\n", sep='')
    })

    browser()
}


SendWeeklySummaryEmail <- function()
{

}
