##' Generate text to copy / paste into svn file Bioconductor.authz
##'
##' @param filenames character() vector of (full) paths to package tar balls.
##' @param version character(1) version string as it appears in svn
##'     'RELEASE' branch, e.g., \code{"3_2"} corresponding to
##'     RELEASE_3_2.
##' @export
svn_auth_text <-
    function(filenames, version=getOption("BC_auth_version", "3_2"))
{
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
        cat(paste0("[/trunk/madman/Rpacks/", p, "]\n",
                   "@", p, " = rw\n",
                   "\n",
                   "[/branches/RELEASE_", version, "/madman/Rpacks/", p, "]\n",
                   "@", p, " = rw\n"),
            "\n")
}
