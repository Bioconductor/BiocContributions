the <- new.env(parent=emptyenv())

#' Login to the issue tracker
#'
#' @param url tracker url
#' @param user username used to login
#' @param password password used to login
#' @export
tracker_login <- function(
    url = "https://tracker.bioconductor.org",
    user = getOption("tracker_user"),
    password = getOption("tracker_password")) {

    if (is.null(the$session)) {
        session <- rvest::html_session(url)

        login <- rvest::set_values(rvest::html_form(session)[[3]],
                                   `__login_name` = user,
                                   `__login_password` = password)

        the$session <- suppressMessages(rvest::submit_form(session, login))
    }

    the$session
}

#' Query the issue tracker
#'
#' @param session the HTTP session to use
#' @param columns which columns to return
#' @param sort A column to sort the data by
#' @param filter what columns are used to filter
#' @param status the status codes used to filter
#' @param ... Additional query parameters
#' @export
tracker_search <- function(session = tracker_login(),
                           columns = c("id","activity","title","creator","status"),
                           sort = desc("activity"),
                           filter=c("status","assignedto"),
                           status = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                           ...) {
    url <- "/issue"
    res <- rvest::jump_to(session,
                   url,
                   query = list("@columns" = paste(columns, collapse = ","),
                             "@sort" = sort,
                             "@filter" = paste(filter, collapse = ","),
                             "status" = paste(status, collapse = ","),
                             "@action" = "export_csv",
                             ...
                             ))
    data <- httr::content(res$response)
    data$activity <- roundup_datetime(data$activity)
    data
}

#' @export
#' @describeIn tracker_search retrieve unassigned packages
unassigned_packages <- function(session = tracker_login(), status = c(-1, 1), ...) {
    tracker_search(session = session, status = status)
}

#' Members of the devteam
devteam <- c("Jim Hester" = "jhester",
    "Martin Morgan" = "mtmorgan",
    "Valerie Obenchain" = "vobencha",
    "Hervé Pagès" = "herve",
    "Dan Tenenbaum" = "dtenenba")

#' Assign new packages
#'
#' This method uses \code{sample} to assign the packages and sets the random
#' seed based on the date.
#' @param team the devteam members to assign
#' @param date the date of assignment
#' @param pkgs packages to assign
#' @export
#' @inheritParams tracker_search
assign_new_packages <- function(session = tracker_login(),
                                team = names(devteam),
                                date = Sys.Date(),
                                pkgs = unassigned_packages(session)) {

    substitute({
        pkgs <- sort(pkgs_)

        team <- team_

        set.seed(as.Date(date_))

        setNames(sample(team, length(pkgs), replace = TRUE), pkgs)
    },
    list(pkgs_ = pkgs$title, team_ = team, date_ = as.character(date)))
}


#' Assign new packages
#'
#' This method uses a hash digest to assign the packages based on the package
#' name.
#' @inheritParams assign_new_packages
assign_new_packages_hash <- function(session = tracker_login(),
                                team = names(devteam),
                                pkgs = unassigned_packages(session)) {

    integer_hash <- function(x, ...) {
        hash <- vapply(x, digest::digest, character(1), ...)
        hash <- substr(hash, 1, 6)
        strtoi(hash, base = 16)
    }

    if (NROW(pkgs) == 0) {
        return()
    }

    substitute({
        integer_hash <- fun_

        pkgs <- pkgs_

        team <- team_

        setNames(team[integer_hash(pkgs) %% length(team) + 1], pkgs)
    },
    list(fun_ = integer_hash, pkgs_ = sort(pkgs$title), team_ = team))
}

desc <- function(x) {
    if (is.numeric(x)) {
        -x
    } else {
        paste0("-", x)
    }
}

roundup_datetime <- function(x, ...) {
    as.POSIXlt(format = "%Y-%m-%d.%H:%M:%S", tz = "PST", x, ...)
}

#' Retrieve all of the messages from an issue
#' @param number the issue number to retrieve
#' @inheritParams tracker_search
#' @export
get_issue <- function(session = tracker_login(), number) {
    response <- rvest::jump_to(session, paste0("/issue", number))

    rows <- rvest::html_nodes(response, ".messages tr")
    if (!NROW(rows)) {
        return(NULL)
    }

    metadata <- rows[seq(2, length(rows), 2)]

    parse_metadata <- function(x) {
        headers <- rvest::html_nodes(x, "th")
        data.frame(
            id = rvest::html_attr(headers[[1]][[1]], "href"),
            author = gsub(x = rvest::html_text(headers[[2]]), "Author: ", ""),
            time = roundup_datetime(
                gsub(x = rvest::html_text(headers[[3]]), "Date: ", "")),
            stringsAsFactors = FALSE
        )
    }
    res <- do.call(rbind, lapply(metadata, parse_metadata))

    parse_messages <- function(x) {
        preformatted <- rvest::html_nodes(x, "pre")
        lapply(preformatted, rvest::html_text)
    }
    messages <- vapply(rows[seq(3, length(rows), 2)],
        function(x) { trimws(gsub("\r", "", rvest::html_text(x))) },
        character(1))

    res$message <- messages

    attachments <- parse_attachments(rvest::html_nodes(response, ".files tr td"))

    res <- merge(res, attachments, by = c("time", "author"), all.x = TRUE,
                 sort = TRUE)
    rownames(res) <- res$id
    attr(res, "session") <- response

    class(res) <- c("issue", "data.frame")

    res
}

#' @param issue an issue object from \code{\link{get_issue}}
#' @param number an issue number, only used if \code{issue} is \code{NULL}
#' @param session the session to use, if \code{NULL} the issue's session is used.
#' @param note a note to post to the issue, defaults to opening your editor,
#' but you can also pass a character string.
#' @param file a file to attach to the issue, if \code{TRUE} choose a file using
#' \code{\link{file.choose}}
#' @param ... Additional arguments passed to rvest::set_values
post_issue <- function(issue = NULL, number = NULL, session = NULL, note = edit(), file = NULL, ...) {
    if (is.null(issue) && is.null(number)) {
        stop("One of ", sQuote("issue"), " or ", sQuote("number"),
            " must be set", call. = FALSE)
    }

    if (is.null(issue)) {
        issue <- get_issue(number = number)
    }

    if (is.null(session)) {
        session <- session(issue)
    }

    if (isTRUE(file)) {
        file <- file.choose()
    }
    form <- rvest::html_nodes(session, "form[name='itemSynopsis']") %>%
        magrittr::extract2(1) %>%
        rvest::html_form()

    form <-
        rvest::set_values(form,
            `@note` = note,
            `@file` = file,
            ...)

    rvest::submit_form(session, form)
}

#' @export
session <- function(...) UseMethod("session")

#' @export
session.issue <- function(x, ...) {
    attr(x, "session")
}

#' Download attachments from an issue
#' @inheritParams tracker_search
#' @param issue The issue object, or issue number to download files from
#' @param dir the location to store the files
#' @export
download <- function(session = tracker_login(), issue, dir = ".", ...) {

    # TODO handle msg123213 issue123123 cases
    if (is.character(issue) || is.numeric(issue)) {
        issue <- get_issue(session, issue)
    }
    Map(function(href, filename) {
        if (!is.na(href)) {
            rvest::jump_to(session,
                href,
                httr::write_disk(path = filename), ...)
        }}, issue$href, issue$filename)
}

parse_attachments <- function(x, ...) {
    links <- lapply(rvest::html_nodes(x[seq(1, length(x), 5)], "a"),
        function(xx) {
            data.frame(href = rvest::html_attr(xx, "href"),
                filename = rvest::html_text(xx),
                stringsAsFactors = FALSE)
        })

    meta <- lapply(x[seq(2, length(x), 5)],
        function(xx) {
            xx <- rvest::html_nodes(xx, "span")
            data.frame(author = rvest::html_text(xx[[1]]),
                time = roundup_datetime(rvest::html_text(xx[[2]])),
                stringsAsFactors = FALSE)
        })

    filetype <- rvest::html_text(x[seq(3, length(x), 5)])

    data.frame(do.call(rbind, links),
        do.call(rbind, meta), filetype,
        stringsAsFactors = FALSE)
}

#' @export
print.issue <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    invisible(x)
}

#' @export
format.issue <- function(x, ...) {
    paste0(
        crayon::bold("ID: "), x$id, "\n",
        crayon::bold("Author: "), x$author, "\n",
        crayon::bold("Time: "), x$time, "\n",
        ifelse(!is.na(x$filename),
            paste0(crayon::bold("Attachment: "), x$filename, "\n"),
            ""),
        x$message, "\n")
}
