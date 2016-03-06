#' Workflow steps
#'
#' @name workflow_standard
NULL
#> NULL

#' @rdname workflow_standard
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

#' @rdname workflow_standard
#' @param pre data.frame() returned by \code{pre_accepted_packages()},
#'     retrieved from the tracker and indicating packages tagged as
#'     'pre-accepted'
#' @export
DownloadNewPackageTarballs <-
    function(pre=pre_accepted_packages())
{
    ## Download tarballs:
    files <- unlist(lapply(pre$id, download, overwrite=TRUE), recursive=FALSE)
    filenames <- proj_path(basename(names(files)))
    list(pre=pre, files=files, filenames=filenames)
}


.LoadNewPackagesMetadata <-
    function(metadata.dir=proj_path(), filename.base="new-packages-metadata_")
{
    filenames = sort(dir(metadata.dir, filename.base, full.names=TRUE),
                     decreasing=TRUE)
    if (!length(filenames))
        stop(".LoadNewPackagesMetadata() did not find any saved metadata")
    local({
        load(filenames[1])
        if (length(ls()) != 1L)
            stop(".LoadNewPackagesMetadata() metadata must have one object",
                 "\n  found: ", paste(sQuote(ls()), collapse=", "))
        get(ls())
    })
}


.CheckUsersCredentials <-
    function(metadata, credPath=proj_path("bioconductor.authz"))
{
    d <- readLines(credPath, 3)[[3]]
    d <- strsplit(d, ", *")[[1]]

    us <- tolower(sapply(metadata$filenames, function(x) {
        ms <- maintainers(x)[[1]]
        sprintf("%s.%s", substr(ms$given[1], 1, 1), ms$family)
    }))

    list(usernames=us, existing=us[us %in% d])
}

#' @rdname workflow_standard
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
    if (missing(metadata))
        metadata <- .LoadNewPackagesMetadata()

    cat('\n', "##### Check authorization file for existing users.", '\n\n',
        sep='')

    creds = .CheckUsersCredentials(metadata)
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

    maints <- vapply(metadata$filenames, function(x) {
        as.character(maintainers(x))[[1]]
    }, character(1))

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

    for (i in seq_along(metadata$filenames))
        cat("accept_package(", metadata$pre$id[[i]], ", '",
            metadata$filenames[[i]], "')\n", sep='')
}


#' @rdname workflow_standard
#' @export
DraftWeeklySummaryEmail <- function()
{
    gmailr::gmail_auth(scope="compose")
    gmailr::create_draft(weeklyEmailPackagesOverview())
}
