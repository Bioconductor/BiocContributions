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
        authz_section(parse_authz_line(x[-1L]), name = name)
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

run_command_on_file <- function(command) {
    function(file = "hedgehog:/extra/svndata/gentleman/svn_authz/bioconductor.authz", args = NULL) {
        re <- rex::rex(start, capture(name = "server", graphs), ":", capture(name = "path", anything))
        match <- rex::re_matches(file, re)
        remote_file <- isTRUE(!is.na(match[[1]]))

        if (remote_file) {
            quoted_args <- shQuote(paste(shQuote(c(args, match$path)), collapse = " "))
            system2("ssh", args = c(match$server, command, quoted_args))
        } else {
            command_split <- strsplit(command, " ")[[1]]
            system2(command_split[1], args = paste(shQuote(c(command_split[-1], args, file)), collapse = " "))
        }
    }
}

#' Run commands on a file, possibly remote.
#'
#' @param args Additional arguments passed to the command.
#' @param inheritParams read_permissions
#' @details if the file is a remote location (server:path) the command is run
#' remotely.
#' @name run_commands
NULL

#' @describeIn run_commands Check out a RCS tracked file
rcs_check_out <- run_command_on_file("co -l")

#' @param message commit message for the check in
#' @describeIn run_commands Check in a RCS tracked file
rcs_check_in <- function(file = "hedgehog:/extra/svndata/gentleman/svn_authz/bioconductor.authz", message, args = NULL) {
    (run_command_on_file("ci -u"))(file = file, c(paste0("-m", message), args))
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
    cat(unlist(format(x, ...), use.names = FALSE), sep = "\n")
}

#' Edit the software permissions
#'
#' @param data a authz data file
#' @param version The release version number
#' @param x The edits to perform
#' @param ... Additional arguments passed to methods
edit_software_permissions <- function(x, ...) {
    UseMethod("edit_software_permissions")
}

#' @describeIn edit_software_permissions data.frame input, expects columns \sQuote{package} and \sQuote{user}
edit_software_permissions.data.frame <- function(x, data = read_permissions(),
    version = 3.2, ...) {
    assert(all(c("package", "user") %in% colnames(x)),
        "'x' must have two columns named 'package' and 'user'")

    edit_software_permissions(split(x$user, x$package))
}

#' @describeIn edit_software_permissions list input, expects a named list of packages and users
edit_software_permissions.list <- function(x, data = read_permissions(), version = "3.2") {
    assert(is_named(x), "Input must be a named list")

    x[] <- lapply(x, as.character)

    usernames <- unlist(x, use.names = FALSE)

    # Add any missing users to the bioconductor-readers group
    readers <- data$groups$`bioconductor-readers`
    missing_users <- !usernames %in% readers
    data$groups$`bioconductor-readers` <- append(readers, usernames[missing_users])

    new <- !names(x) %in% names(data$groups)

    # For existing groups, assign the new users
    data$groups[names(x)[!new]] <- x[!new]

    end_of_groups <- tail(which(!nzchar(data$groups)), n = 1L) - 1L
    if (any(new)) {
        data$groups <- append(data$groups, x[new], end_of_groups)
        new_packages <- names(x)[new]

        for (pkg in new_packages) {

            # Unfortunately you cannot use append for this as it calls c(),
            # which drops attributes :(. So we have to do the appending manually
            len <- length(data)
            obj <- list("rw", "")
            names(obj) <- c(paste0("@", pkg), NA)

            trunk_loc <- paste0("/trunk/madman/Rpacks/", pkg)
            data[[len + 1L]] <- authz_section(obj, name = trunk_loc)
            names(data)[[len + 1L]] <- trunk_loc

            release_loc <- paste0("/branches/RELEASE_", sub("[.]", "_", version), "/madman/Rpacks/", pkg)
            data[[len + 2L]] <- authz_section(obj, name = release_loc)
            names(data)[[len + 2L]] <- release_loc
        }
    }
    data
}

#' Edit the data experiment permissions
#'
#' @param data a authz data file
#' @param version The release version number
#' @param x The edits to perform
#' @param ... Additional arguments passed to methods
#' @export
edit_data_experiment_permissions <- function(x, ...) {
    UseMethod("edit_data_experiment_permissions")
}

#' @describeIn edit_data_experiment_permissions data.frame input, expects columns \sQuote{package} and \sQuote{user}
#' @export
edit_data_experiment_permissions.data.frame <- function(x, data = read_permissions("hedgehog:/extra/svndata/gentleman/svn_authz/bioc-data.authz"), version = 3.2, ...) {
    assert(all(c("package", "user") %in% colnames(x)),
        "'x' must have two columns named 'package' and 'user'")

    edit_data_experiment_permissions(split(x$user, x$package))
}

# This function is largely duplicated from edit_software_permissions, perhaps
# it is worth trying to use a common helper...

#' @describeIn edit_data_experiment_permissions list input, expects a named list of packages and users
#' @export
edit_data_experiment_permissions.list <- function(x, data = read_permissions("hedgehog:/extra/svndata/gentleman/svn_authz/bioc-data.authz"), version = "3.2") {
    assert(is_named(x), "Input must be a named list")

    x[] <- lapply(x, as.character)

    usernames <- unlist(x, use.names = FALSE)

    # Add any missing users to the bioconductor-readers group
    readers <- data$groups$`bioc-data-readers`
    missing_users <- !usernames %in% readers
    data$groups$`bioc-data-readers` <- append(readers, usernames[missing_users])

    new <- !names(x) %in% names(data$groups)

    # For existing groups, assign the new users
    data$groups[names(x)[!new]] <- x[!new]

    end_of_groups <- tail(which(!nzchar(data$groups)), n = 1L) - 1L
    if (any(new)) {
        data$groups <- append(data$groups, x[new], end_of_groups)
        new_packages <- names(x)[new]

        for (pkg in new_packages) {

            # Unfortunately you cannot use append for this as it calls c(),
            # which drops attributes :(. So we have to do the appending manually
            len <- length(data)
            obj <- list("rw", "")
            names(obj) <- c(paste0("@", pkg), NA)

            trunk_loc <- paste0("/trunk/experiment/pkgs/", pkg)
            data[[len + 1L]] <- authz_section(obj, name = trunk_loc)
            names(data)[[len + 1L]] <- trunk_loc

            trunk_loc <- paste0("/trunk/experiment/data_store/", pkg)
            data[[len + 2L]] <- authz_section(obj, name = trunk_loc)
            names(data)[[len + 2L]] <- trunk_loc

            release_loc <- paste0("/branches/RELEASE_", sub("[.]", "_", version), "/experiment/pkgs/", pkg)
            data[[len + 3L]] <- authz_section(obj, name = release_loc)
            names(data)[[len + 3L]] <- release_loc

            release_loc <- paste0("/branches/RELEASE_", sub("[.]", "_", version), "/experiment/data_store/", pkg)
            data[[len + 4L]] <- authz_section(obj, name = release_loc)
            names(data)[[len + 4L]] <- release_loc
        }
    }
    data
}

#' Generate a standard commit message for permission edits
#'
#' @param x the edits to make, if a data.frame will be coerced to a named list.
#' @export
standard_commit_message <- function(x) {
    if (is.data.frame(x)) {
        assert(all(c("package", "user") %in% colnames(x)),
            "'x' must have two columns named 'package' and 'user'")

        x <- split(x$user, x$package)
    } else {
        assert(is.list(x) && is_named(x), "Input must be a named list")
    }

    paste0(names(x), " = ", lapply(x, paste, collapse = ", "), collapse = "; ")
}

parse_authz_line <- function(x, ...) {
  itr <- 1
  res <- vector("list", length(x))
  assignments <- grepl("[[:graph:]]+[[:space:]]*=[[:space:]][[:graph:]]", x)
  for (splt in strsplit(x[assignments], "[[:space:]]*=[[:space:]]*|[[:space:]]*,[[:space:]]*")) {
      line_num <- which(assignments)[itr]
      if (length(splt) <= 1L) {
          stop("incorrect parse in line: ", sQuote(x[line_num]), call. = FALSE)
      }
      res[[line_num]] <- splt[-1L]
      names(res)[[line_num]] <- splt[1L]
      itr <- itr + 1L
  }
  res[!assignments] <- x[!assignments]
  res
}

authz_section <- function(x, name) {
    attr(x, "name") <- name
    class(x) <- "authz_section"
    x
}

#' Helper function to Add Software Permissions
#'
#' @param x Permissions to add, can be a named \code{list} or \code{data.frame}.
#' @param message Commit message to use
#' @param file File containing the permissions to edit
#' @export
add_software_permisions <- function(x, message = standard_commit_message(x),
    file =  "hedgehog:/extra/svndata/gentleman/svn_authz/bioconductor.authz") {

    # check out the permissions file
    check_out_file()

    # add new permissions
    perms <- read_permissions(file = file)
    perms <- edit_software_permissions(x, data = perms)
    write_permissions(perms)

    # check in the modified file
    check_in_file(message = message)
}
