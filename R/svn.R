svn <- function(dir = getwd()) {

    parse_xml <- function(x) {
        xml2::read_xml(paste(collapse = "\n", x))
    }
    status <- function(args) {
        files <- parse_xml(system2("svn", args = c("status", "--xml", args), stdout = TRUE))
        entries <- xml2::xml_find_all(files, "//entry")
        data.frame(filename = xml2::xml_attr(entries, "path"),
            type = xml2::xml_attr(xml2::xml_children(entries), "item"))
    }

    list(
         add = function(files, args = NULL) {
             withr::with_dir(dir, system2("svn", args = c("add", files, args)))
         },

         commit = function(message, args = NULL) {
             tmp <- tempfile()
             on.exit(unlink(tmp))
             writeLines(con = tmp, message)
             withr::with_dir(dir,
                 system2("svn",
                     args = c("commit", "--file", tmp, args)))
         },

         status = function(args = NULL) {
             withr::with_dir(dir, status(args))
         },
         log = function(args = NULL) {
             library(xml2)
             withr::with_dir(dir,
                 xml2::read_xml(
                     paste(collapse = "\n",
                         system2("svn",
                             args = c("log", "--xml", args),
                             stdout = TRUE))))
         },
         remove_untracked = function(args = NULL) {
             withr::with_dir(dir, {
                 files <- status(args)
                 unlink(subset(files, type == "unversioned")$filename, recursive = TRUE)
                 })
         },
         update = function(args = NULL) {
             withr::with_dir(dir, {
                 system2("svn", args = c("up", args))
                 })
         },
         read = function(filename) {
             withr::with_dir(dir, {
                 if (!(is.character(filename) && length(filename) == 1)) {
                     stop("Only read one file at a time", call. = FALSE)
                 }
                 readLines(con = filename)
                 })
         },
         write = function(filename, content) {
             content <- force(content)
             withr::with_dir(dir, {
                 if (!(is.character(filename) && length(filename) == 1)) {
                     stop("Only write one file at a time", call. = FALSE)
                 }
                 writeLines(con = filename, content)
                 })
         },
         ls = function(args = NULL) {
             withr::with_dir(dir, {
                 system2("svn", args = c("ls", args))
                 })
         },
         diff = function(args = NULL) {
             withr::with_dir(dir, {
                 system2("svn", args = c("diff", args))
                 })
         }
    )
}

#' Add software packages to SVN
#'
#' @param x package tarballs to add.
#' @param svn_location location of the SVN repository
#' @param manifest name of the manifest file
add_software_packages <- function(x, svn_location = "~/proj/Rpacks", manifest = "bioc_3.3.manifest") {
    lapply(x, clean)
    s <- svn(svn_location)
    s$update()

    pkg_names <- .getShortPkgName(x)
    message(s$status())
    s$add(pkg_names)
    s$write(manifest,
        append(s$read(manifest),
            paste0("Package: ", pkg_names, "\n")))
    message(s$status())
    s$commit(paste0("Adding ", paste(collapse = ", ", pkg_names)))
}

#' Add data experiment packages to SVN
#'
#' @inheritParams add_software_packages
add_data_experiment_packages <- function(x, svn_location = "~/proj/experiment",
    manifest = "pkgs/bioc-data-experiment.3.3.manifest") {

    lapply(x, clean_data_pkg)
    s <- svn(svn_location)
    s$update()

    pkg_names <- .getShortPkgName(x)
    message(s$status())
    s$add(file.path("pkgs", pkg_names))
    s$add(file.path("data_store", pkg_names))
    s$write(manifest,
        append(s$read(manifest),
            paste0("Package: ", pkg_names, "\n")))
    message(s$status())
    s$commit(paste0("Adding ", paste(collapse = ", ", pkg_names)))
}

#' @export
print.svn_logentry <- function(x, ...) {
  cat(sep = "\n",
      paste(sep = " | ",
          xml_attr(x, "revision"),
          xml_text(xml_find_one(x, ".//author")),
          xml_text(xml_find_one(x, ".//date"))
          ),
      xml_text(xml_find_one(x, ".//msg")))
}

#' Read authz permission file
#'
#' @param file location passed to rsync
#' @export
read_permissions <- function(file = "hedgehog:/extra/svndata/gentleman/svn_authz/bioconductor.authz") {
    tmp <- tempfile()
    system2("rsync", args = c(file, tmp))
    res <- readLines(tmp)
    group_locs <- grepl("^\\[", res)
    groups <- gsub("[][]", "", res[group_locs])
    res <- split(res, cumsum(group_locs))
    res <- Map(function(x, name) structure(list(data = x[-1], name = name), class = "authz_section"),
        res, groups, USE.NAMES = FALSE)
    structure(res, class = "authz")
}

#' Write authz permission file
#'
#' @param x object to write
#' @inheritParams read_permissions
#' @export
write_permissions <- function(x, file = "hedgehog:/extra/svndata/gentleman/svn_authz/bioconductor.authz", ...) {
  tmp <- tempfile()
  writeLines(format(x), con = tmp)
  system2("rsync", args = c(tmp, file))
}

#' @export
format.authz <- function(x, ...) {
    paste(collapse = "\n", vapply(x, format, character(1)))
}

#' @export
format.authz_section <- function(x, ...) {
        paste(sep = "\n",
            paste0("[", x$name, "]"),
            paste(collapse = "\n", x$data))
}

#' @export
print.authz_section <- function(x, ...) {
    cat(format(x, ...))
}

#' @export
print.authz <- function(x, ...) {
    cat(format(x, ...))
}
