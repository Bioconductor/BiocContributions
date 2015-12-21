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
    res <- Map(function(x, name) {
        structure(parse_authz_line(x[-1]), name = name, class = "authz_section")
      }, res, groups, USE.NAMES = FALSE)
    names(res) <- groups
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
    unlist(lapply(x, format, ...), use.names = FALSE)
}

#' @export
format.authz_section <- function(x, ...) {
    named <- if (is.null(names(x))) {
        rep(FALSE, length(x))
    } else {
        !is.na(names(x))
    }
    res <- character(length(x))
    res[named] <- paste0(names(x)[named], " = ",
        vapply(x[named], paste0, character(1), collapse = ", "))
    res[!named] <- x[!named]
    c(paste0("[", attr(x, "name"), "]"),
      res)
}

#' @export
print.authz_section <- print.authz <- print.authz_lines <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
}

add_software_permissions <- function(tarball,
    usernames = username(match_user(maintainers(tarball))),
    package = package_name(tarball), ...) {

    assert(length(package) == 1, "Only one package can be specified at a time")

    x <- read_permissions(...)

    bioconductor_readers_content <- parse_authz_line(x[[1]]$data[bioconductor_readers_line])
    missing_users <- !usernames %in% bioconductor_readers_content
    for (user in usernames) {
        #if (grepl()
    }
}

parse_authz_line <- function(x, ...) {
  itr <- 1
  res <- vector("list", length(x))
  assignments <- grepl("[[:graph:]]+[[:space:]]*=[[:space:]][[:graph:]]", x)
  for (splt in strsplit(x[assignments], "[[:space:]]*=[[:space:]]*|[[:space:]]*,[[:space:]]*")) {
      line_num <- which(assignments)[itr]
      if (length(splt) <= 1) {
          stop("incorrect parse in line: ", sQuote(x[line_num]), call. = FALSE)
      }
      res[[line_num]] <- splt[-1]
      names(res)[[line_num]] <- splt[1]
      itr <- itr + 1
  }
  res[!assignments] <- x[!assignments]
  res
}

generatePermissionEdits <- function(path = ".", pattern = "\\.tar\\.gz$"){
    ## start with tarballs in whatever dir we have here...
    tarballs <- dir(path = path, pattern = pattern, full.names = TRUE)
    ## store the above in a list object
    data <- lapply(tarballs, .getPkgNameAndUser)
    
    ### For all packages in that list:

    ## write out association part (for each - helper2)
    message(paste(sapply(data, .printAssociations), collapse=""))
    
    ## write out the tedious part (for each - helper3)
    message(paste(sapply(data, .printTediousStuff), collapse=""))
    
}
