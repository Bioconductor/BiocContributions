#' Declare an SVN instance
#'
#' This is primarily a helper function for using svn progromatically.
#' @param dir The SVN directory
#' @export
#' @examples
#' \dontrun{
#' s <- svn("my/svn/location")
#' s$status()
#' s$update()
#' s$add("file3")
#' s$commit("adding file3")
#' }
svn <- function(dir = getwd()) {

    parse_xml <- function(x) {
        xml2::read_xml(paste(collapse = "\n", x))
    }
    status <- function(args) {
        files <- parse_xml(system2("svn", args = c("status", "--xml", args),
                                   stdout = TRUE))
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
                 unlink(subset(files, type == "unversioned")$filename,
                        recursive = TRUE)
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
                 con <- file(filename, "wb")
                 writeLines(content, con, sep="\n")
                 close(con)
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

check_manifest <- function(x, pkgs) {
    match <- compact(Map(function(pkg) {
            grep(paste0("Package:[[:space:]]+", pkg, "\\b"), x)
             }, pkgs))
    if (length(match) > 0) {
        stop(paste0(sQuote(names(match)), collapse = ", "),
            " already in manifest line(s): ",
            paste0(collapse = ", ", unlist(match)), call. = FALSE)
    }
    TRUE
}

add_package_type <- function(svn_location, manifest, clean_function,
                             adding_code)
{
    eval(bquote(
        function(x, svn_location = .(svn_location), manifest = .(manifest)) {
            #lapply(x, .(clean_function), .(svn_location))
            lapply(x, .(clean_function))
            s <- svn(svn_location)
            s$update()

            pkg_names <- .getShortPkgName(x)
            s$status()

            current <- s$read(manifest)
            if (check_manifest(current, pkg_names)) {
                .(adding_code)
                s$write(manifest,
                    append(current, paste0("Package: ", pkg_names, "\n")))
                s$status()
                s$commit(paste0("Adding ", paste(collapse = ", ", pkg_names)))
            }
        }))
}

#' Add packages to SVN
#'
#' @param x package tarballs to add.
#' @param svn_location location of the SVN repository
#' @param manifest name of the manifest file
#' @name add_packages
NULL

#' @describeIn add_packages Software Packages
#' @export
add_software_packages <- add_package_type(
    svn_location = proj_path("Rpacks"),
    manifest = "bioc_3.3.manifest",
    clean_function = quote(clean),
    adding_code = quote(s$add(pkg_names)))

#' @describeIn add_packages Data Experiment Packages
#' @export
add_data_experiment_packages <- add_package_type(
    svn_location = proj_path("experiment"),
    manifest = "pkgs/bioc-data-experiment.3.3.manifest",
    clean_function = quote(clean_data_package),
    adding_code = quote({
        s$add(file.path("pkgs", pkg_names))
        s$add(file.path("data_store", pkg_names))
    }))

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
