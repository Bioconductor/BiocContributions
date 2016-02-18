#' Workflow steps
#'
#' @name contrib-workflows
NULL
#> NULL

#' @rdname contrib-workflows
#' @param assignInTracker logical(1) indicating whether package
#'     assignments should be updated in the tracker.
#' @param secret character(1) path to JSON API client access secret.
#' @export
CreatePackageAssignmentEmail <-
    function(assignInTracker=FALSE,
             secret=proj_path("bioconductorseattle-gmail.json"))
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

#' @rdname contrib-workflows
#' @param pre data.frame() returned by \code{pre_accepted_packages()},
#'     retrieved from the tracker and indicating packages tagged as
#'     'pre-accepted'
#' @export
DownloadNewPackageTarballs <-
    function(pre=pre_accepted_packages())
{
    ## Download tarballs:
    files <- unlist(lapply(pre$id, download, overwrite=TRUE), recursive=FALSE)

    cat('\n', filenames, sep='\n')

    return (list(pre=pre, files=files, filenames=filenames))
}


.LoadNewPackagesMetadata <-
    function(metadata.dir=proj_path(), filename.base="new-packages-metadata_")
{
    filenames = sort(dir(metadata.dir, filename.base, full.names=TRUE),
                     decreasing=TRUE)
    e = new.env()
    if (length(filenames))
        load(filenames[1], envir=e) # File extension "RData".

    return (e)
}


.CheckUsersCredentials <-
    function(metadata, credPath=proj_path("bioconductor.authz"))
{
    d <- readLines(credPath)

    f <- metadata

    us <- sapply(f$filenames, function(x) {
        ms <- maintainers(x)
        u <- c()
        for (m in ms) {
            u <- c(u, tolower(paste(substr(m$given, 1, 1), m$family, sep=".")))
        }

        u #c(u, "h.simpson")
    }, simplify=FALSE)

    ex <-sapply(us, function(x) {
        sapply(x, function(y) any(grepl(y, d)), simplify=FALSE)
    }, simplify=FALSE)

    list(usernames=us, existing=ex)
}

#' @rdname contrib-workflows
#'
#' @param metadata Return value from
#'     \code{DownloadNewPackageTarballs}. If missing, search
#'     \code{proj_path()} for most recent saved version as RData with
#'     format "new-packages-metadata_20160211.RData"
#' @param createDraft logical(1) draft email to FHCRC for SVN new user
#'     credentials
#' @export
ManageNewPackagesCredentials <-
    function(metadata, createDraft=TRUE)
{
    m <- new.env()
    if (missing(metadata))
        m <- .LoadNewPackagesMetadata()
    else
       m$f = metadata

    f = m$f

    cat('\n', "##### Check authorization file for existing users.", '\n\n',
        sep='')

    creds = .CheckUsersCredentials(f)
    print(creds)

    cat("##### Gmail draft to Carl Benson <scicomp@fhcrc.org>", '\n\n', sep='')

    mimeDetails <- list(
        From = "packages@bioconductor.org",
        To = "scicomp@fhcrc.org",
        Subject = "New SVN users for Hedgehog"
    )

    email <- c("Hi Carl,", "",
               "Can you please create new SVN account(s) on Hedgehog for", "",
               "@@NEWUSERS@@", "",
               "Thanks,", "",
               "Martin", "")

    maints <- sapply(f$filenames, function(x) {
        maint <- maintainers(x)
        sapply(maint, function(y) {
            paste(y$given, y$family, '<' %_% y$email %_% '>')
        }, simplify=TRUE)
    }, simplify=TRUE)

    email <- paste(sub("@@NEWUSERS@@", paste(maints, collapse='\n'), email),
                   collapse='\n')

    for (i in seq_along(mimeDetails))
        cat(names(mimeDetails)[i], ": ", mimeDetails[[i]], "\n", sep="")
    cat(email, sep="\n")

    ## Create draft gmail.
    if (createDraft) {
        gmail_auth(scope="compose")
        gmailr::create_draft(mime(From=mimeDetails$From, To=mimeDetails$To,
                                  Subject=mimeDetails$Subject, body=email))
    }

    cat('\n', "##### Create draft e-mails to maintainers", '\n\n', sep='')
    cat("gmailr::gmail_auth(scope='compose')\n")
    for (package in names(creds$usernames))
        for (username in creds$usernames[[package]])
            cat("gmailr::create_draft(emailMaintainer('", package,
                "', userId='", username,
                "', password='XXXXXXXXX'))\n",
            sep='')

    cat('\n', "##### Accept packages", '\n\n', sep='')

    for (i in seq_along(f$filenames))
        cat("accept_package(", f$pre$id[[i]], ", '", f$filenames[[i]], "')\n",
            sep='')
}


#' @rdname contrib-workflows
#' @export
DraftWeeklySummaryEmail <- function()
{
    gmailr::gmail_auth(scope="compose")
    gmailr::create_draft(weeklyEmailPackagesOverview())
}
