.github_userpwd <-
    function()
{
    paste(
        getOption("bioc_contributions_github_user"),
        getOption("bioc_contributions_github_auth"),
        sep=":")
}

.github_get <-
    function(path, api="https://api.github.com",
             path_root="/repos/Bioconductor/Contributions")
{
    query <- sprintf("%s%s%s", api, path_root, path)
    response <- GET(
        query,
        config(userpwd=.github_userpwd(), httpauth=1L),
        accept("application/vnd.github.v3+json"))
    stop_for_status(response)
    content(response)
}

.github_patch <-
    function(path, body, ..., api="https://api.github.com",
             path_root="/repos/Bioconductor/Contributions",
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
    message("downloading ", sQuote(issue$title))
    repos <- sub(".*Repository: *([[:alnum:]/:\\.-]+).*", "\\1",
                 issue$body)

    path <- sprintf("/issues/%d/comments", issue$number)
    comments <- .github_get(path)
    tag <- "AdditionalPackage: *([[:alnum:]/:\\.-]+).*"
    for (comment in comments) {
        if (!grepl(tag, comment$body))
            next
        repos <- c(repos,
                   sub(tag, "\\1", comment$body))
    }

    for (repo in repos)
        system2("git", sprintf("clone --depth 1 %s", repo),
                stdout=TRUE, stderr=TRUE)
    setNames(basename(repos), rep(issue$title, length(repos)))
}

.github_close <- function(issue) {
    path <- sprintf("/issues/%d", issue$number)
    .github_patch(path, list(state="closed"))
    issue$state
}

.github_to_svn_Software <-
    function(pkgs, svn_location=proj_path("Rpacks"))
{
    if (!length(pkgs))
        return(pkgs)

    for (pkg in pkgs)
        clean(pkg)

    bioc_version <- getOption("bioc_contributions_manifest_version")
    stopifnot(!is.null(bioc_version))
    svn_manifest <-
        file.path(svn_location, sprintf("bioc_%s.manifest", bioc_version))

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
    file.path("Rpacks", pkgs)
}

.github_to_svn_ExperimentData <-
    function(pkgs, svn_location=proj_path("experiment"))
{
    if (!length(pkgs))
        return(pkgs)

    for (pkg in pkgs)
        clean_data_package(pkg)

    bioc_version <- getOption("bioc_contributions_manifest_version")
    stopifnot(!is.null(bioc_version))
    svn_manifest <- file.path(
        svn_location,
        sprintf("pkgs/bioc-data-experiment.%s.manifest", bioc_version))

    s <- svn(svn_location)
    s$update()

    pkg_names <- .getShortPkgName(pkgs)
    s$status()

    current <- s$read(svn_manifest)
    if (check_manifest(current, pkg_names)) {
        {
            s$add(file.path("pkgs", pkg_names))
            s$add(file.path("data_store", pkg_names))
        }
        s$write(svn_manifest, append(current, paste0("Package: ",
            pkg_names, "\n")))
        s$status()
        s$commit(paste0("Adding ", paste(collapse = ", ", pkg_names)))
    }
    file.path("pkgs", pkg_names)
}

#' @export
github_accept <- function() {
    path <- "/issues?state=open&labels=3a.%20accepted"
    issues <- .github_get(path)

    owd <- setwd(proj_path())
    on.exit(setwd(owd))

    pkgs <- unlist(lapply(issues, .github_download))
    types <- bioc_views_classification(pkgs)
    types$Software <- .github_to_svn_Software(types$Software)
    types$ExperimentData <-
        .github_to_svn_ExperimentData(types$ExperimentData)

    ## close issues
    state <- vapply(issues, .github_close, character(1))

    types$ExperimentData <- file.path("experiment", types$ExperimentData)
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
    ids <- strsplit(readLines(credentials, 3)[[3]], ", *")[[1]]
    !.user_id(maintainers) %in% ids
}

#' @export
github_svn_credentials_request_from_carl <- function(packages) {
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

#' @export
github_svn_credentials_draft_to_user <- function(packages) {
    packages <- file.path(proj_path(), packages)
    maintainers <- lapply(packages, function(x) maintainers(x)[[1]])
    user_id <- .user_id(maintainers)

    letters <- Map(emailMaintainer, packages, user_id,
                   MoreArgs=list(password="XXXXXXXXX"))

    gmailr::gmail_auth(scope='compose')
    for (letter in letters)
        gmailr::create_draft(letter)

    length(letters)
}
