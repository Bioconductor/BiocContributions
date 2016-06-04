## library(jsonlite)
## https://developer.github.com/v3/

options(
    bioc_contributions_github_user="mtmorgan",
    bioc_contributions_github_auth="6e55b2fa27337750d63a6eada17c400a1609792a"
)

library(httr)

repository <- "https://api.github.com/Bioconductor/ContributionsBeta"

.github_userpwd <-
    function()
{
    paste(
        getOption("bioc_contributions_github_user"),
        getOption("bioc_contributions_github_auth"),
        sep=":")
}

github_get <-
    function(path, api="https://api.github.com",
             path_root="/repos/Bioconductor/ContributionsBeta")
{
    query <- sprintf("%s%s%s", api, path_root, path)
    response <- GET(
        query,
        config(userpwd=.github_userpwd(), httpauth=1L),
        accept("application/vnd.github.v3+json"))
    stop_for_status(response)
    content(response)
}

github_post <-
    function(path, body, ..., api="https://api.github.com", encode="json")
{
    query <- sprintf("%s%s", api, path)
    response <- POST(
        query,
        config(userpwd=.github_userpwd(), httpauth=1L),
        accept("application/vnd.github.v3+json"),
        body=body,
        ...,
        encode=encode)
    stop_for_status(response)
    content(response)
}

github_patch <-
    function(path, body, ..., api="https://api.github.com",
             path_root="/repos/Bioconductor/ContributionsBeta",
             encode="json")
{
    query <- sprintf("%s%s%s", api, path_root, path)
    response <- PATCH(
        query,
        config(userpwd=.github_userpwd(), httpauth=1L),
        accept("application/vnd.github.v3+json"),
        body=body,
        ...,
        encode=encode)
    stop_for_status(response)
    content(response)
}


.github_download <- function(issue) {
    repos <- sub(".*Repository: *([[:alnum:]/:\\.]+).*", "\\1", issue$body)
    system2("git", sprintf("clone %s", repos), stdout=TRUE, stderr=TRUE)
    basename(repos)
}

.github_close <- function(issue) {
    path <- sprintf("/issues/%d", issue$number)
    github_patch(path, list(status="closed"))
    issue$state
}

github_accept <- function() {
    path <- "/issues?state=open&labels=3a.%20accepted"
    issues <- github_get(path)

    bioc_version <- getOption("bioc_contributions_devel_version", "3.4")

    svn_location <- proj_path("Rpacks")
    svn_manifest <-
        file.path(svn_location, sprintf("bioc_%s.manifest", bioc_version))

    owd <- setwd(proj_path())
    on.exit(setwd(owd))

    pkgs <- vapply(issues, function(issue) {
        pkg <- .github_download(issue)
        clean(pkg, svn_location)
    }, character(1))
    types <- bioc_views_classification(pkgs)
    stopifnot(length(types$ExperimentData) == 0L) # FIXME

    pkgs <- types$Software
    s <- svn(svn_location)
    s$update()

    pkg_names <- basename(pkgs)
    s$status()

    current <- s$read(svn_manifest)
    if (check_manifest(current, pkg_names)) {
        s$add(pkg_names)
        s$write(svn_manifest,
                append(current, paste0("Package: ", pkg_names, "\n")))
        s$status()
        s$commit(paste0("Adding ", paste(collapse = ", ", pkg_names)))
    }

    ## close issues
    issues <- setNames(issues, basename(pkgs))
    state <- vapply(issues, .github_close, character(1))
    
    types
}

.user_id <- function(maintainers) {
    tolower(sapply(maintainers, function(maintainer) {
        sprintf("%s.%s", substr(maintainer$given[1], 1, 1), maintainer$family)
    }))
}

.credentials_required <- function(maintainers) {
    credentials <- proj_path("bioconductor.authz")
    if (!file.exists(credentials))
        stop("local copy of 'bioconductor.authz' required at",
             "\n  ", credentials)
    ids <- strsplit(readLines(credentials, 3)[[3]], ", *")
    !.user_id(maintainers) %in% ids
}

github_svn_credentials_request_from_carl <- function(packages) {
    packages <- file.path(proj_path("Rpacks"), packages)
    names(packages) <- basename(packages)
    maintainers <- lapply(packages, function(x) maintainers(x)[[1]])
    maintainers <- maintainers[.credentials_required(maintainers)]

    if (length(maintainers)){
        newusers <- paste(sapply(maintainers, as.character), collapse="\n")
        from <- getOption("bioc_contributions_signature", "Bioconductor")
        body <- template("svn_credentials_request.txt",
                         newusers=newusers,
                         from=from)

        mime <- mime(From="packages@bioconductor.org",
                     To="scicomp@fhcrc.org",
                     Subject="New SVN users for hedgehog",
                     body=body)

        gmail_auth(scope = "compose")
        gmailr::create_draft(mime)
    }

    length(maintainers)
}

github_svn_credentials_draft_to_user <- function(packages) {
    packages <- file.path(proj_path("Rpacks"), packages)
    maintainers <- lapply(packages, function(x) maintainers(x)[[1]])
    user_id <- .user_id(maintainers)

    letters <- Map(emailMaintainer, packages, user_id,
                   MoreArgs=list(password="XXXXXXXXX"))

    gmailr::gmail_auth(scope='compose')
    for (letter in letters)
        gmailr::create_draft(letter)

    length(letters)
}
