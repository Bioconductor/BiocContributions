##' Update svn authz files
##' 
##' These functions generate text to copy / paste into software svn
##' file bioconductor.authz or data experiment svn file
##' bioc-data.authz
##'
##' @param filenames character() vector of (full) paths to package tar balls.
##' 
##' @param version character(1) version string as it appears in svn
##'     'RELEASE' branch, e.g., \code{"3_2"} corresponding to
##'     RELEASE_3_2.
##'
##' @name svn_auth_text
NULL

##' @rdname svn_auth_text
##' @export
svn_software_auth_text <-
    function(filenames,
             version=getOption("bioc_contributions_release_version"))
{
    version <- sprintf("%d_%d",
                       package_version(version)$major,
                       package_version(version)$minor)
    m <- lapply(filenames, maintainers)
    userid <- tolower(vapply(m, function(elt) {
        elt <- elt[[1]]                 # first maintainer only
        sprintf("%s.%s", substr(elt$given[1], 1, 1), elt$family)
    }, ""))
    package <- sub("_0.99.*", "", basename(filenames))

    ## read permission
    cat(paste(c("", userid), collapse=", "), "\n\n")

    ## groups
    cat(paste(package, userid, sep=" = ", collapse="\n"), "\n\n")

    ## permission
    for (p in package)
        cat(paste0(
            "[/trunk/madman/Rpacks/", p, "]\n",
            "@", p, " = rw\n",
            "\n",
            "[/branches/RELEASE_", version, "/madman/Rpacks/", p, "]\n",
            "@", p, " = rw\n"),
            "\n")
}


##' @rdname svn_auth_text
##' @export
svn_data_experiment_auth_text <-
    function(filenames,
             version=getOption("bioc_contributions_release_version"))
{
    version <- sprintf("%d_%d",
                       package_version(version)$major,
                       package_version(version)$minor)
    m <- lapply(filenames, maintainers)
    userid <- tolower(vapply(m, function(elt) {
        elt <- elt[[1]]                 # first maintainer only
        sprintf("%s.%s", substr(elt$given[1], 1, 1), elt$family)
    }, ""))
    package <- sub("_0.99.*", "", basename(filenames))

    ## read permission
    cat(paste(c("", userid), collapse=", "), "\n\n")

    ## groups
    cat(paste(package, userid, sep=" = ", collapse="\n"), "\n\n")

    ## permission
    for (p in package)
        cat(paste0(
            "[/trunk/experiment/pkgs/", p, "]\n",
            "@", p, " = rw\n",
            "\n",
            "[/trunk/experiment/data_store/", p, "]\n",
            "@", p, " = rw\n",
            "\n",
            "[/branches/RELEASE_", version, "/experiment/pkgs/", p, "]\n",
            "@", p, " = rw\n",
            "\n",
            "[/branches/RELEASE_", version, "/experiment/data_store/", p, "]\n",
            "@", p, " = rw\n"),
            "\n")
}
