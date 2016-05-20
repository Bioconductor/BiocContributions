bioc_views_from_files <- function(files) {
    names(files) <- basename(files)
    descriptions <- lapply(files, readDESCRIPTION)
    views <- vapply(descriptions, `[[`, character(1), "biocViews")
    strsplit(trimws(views), "[[:blank:],\n]+")
}

#' Extract and validiate biocViews terms from tarball
#'
#' @param files tar.gz file locations
#' @return named vector classifying each tar ball to biocViews hierarchy
#' @export
bioc_views_classification <- function(files) {
    names(files) <- basename(files)
    views <- bioc_views_from_files (files)
    curr <- biocViews::getCurrentbiocViews()
    curr <- setNames(unlist(curr, use.names=FALSE),
                     rep(names(curr), lengths(curr)))

    idx <- match(unlist(views), curr)
    if (anyNA(idx)) {
        msg <- paste0(unlist(views)[is.na(idx)], " (",
                      rep(names(views), lengths(views))[is.na(idx)],
                      ")", collapse="\n    ")
        stop("invalid biocViews:\n    ", msg)
    }

    class <- relist(names(curr)[idx], views)
    class1 <- lapply(class, unique)

    if (!all(lengths(class1) == 1L)) {
        ok <- lengths(class1) == 1L
        msg <- paste(Map(function(nm, v, c) {
            sprintf("%s: %s", nm, paste0(v, " (", c, ")", collapse=", "))
        }, names(views)[!ok], views[!ok], class[!ok]), collapse="\n    ")
        stop("conflicting biocViews subgraphs:\n    ", msg)
    }

    split(unname(files[names(class1)]), unlist(unname(class1)))
}
