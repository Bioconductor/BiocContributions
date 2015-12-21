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

#' @export
#' @describeIn tracker_search retrieve pre-accepted packages
pre_accepted_packages <- function(session = tracker_login(), status = 9, ...) {
    tracker_search(session = session, status = status)
}



#' @export
#' @describeIn tracker_search retrieve the logged in users packages
my_issues <- function(session = tracker_login(), user = NULL, status = c(-1, 1, 2, 3, 4, 5, 9, 10), ...) {
    if (is.null(user)) {
        links <- rvest::html_nodes(session, "a")
        text <- rvest::html_text(links)
        match <- grepl("Your Issues", text, fixed = TRUE)
        if (!any(match)) {
            stop("No links have text 'Your Issues'", call. = FALSE)
        }
        href <- rvest::html_attr(links[[which(match)[1]]], "href")
        user <- rex::re_matches(href, rex::rex("assignedto=",
                                               capture(name = "id", digits)))$id
    }
    tracker_search(session = session, assignedto = user, status = status, ...)
}

#' Members of the devteam
devteam <- c(
    "jhester" = "Jim Hester",
    "mtmorgan" = "Martin Morgan",
    "vobencha" = "Valerie Obenchain",
    "herve" = "Hervé Pagès",
    "dtenenba" = "Dan Tenenbaum",
    "blong" = "Brian Long",
    "priscian" = "Jim Java")

#' Assign new packages
#'
#' This method uses a hash digest to assign the packages based on the package
#' name.
#' @inheritParams assign_new_packages
#' @export
assign_new_packages <- function(session = tracker_login(),
                                team = devteam,
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
    list(fun_ = integer_hash, pkgs_ = sort(pkgs$title), team_ = unname(team)))
}

desc <- function(x) {
    if (is.numeric(x)) {
        -x
    } else {
        paste0("-", x)
    }
}

roundup_datetime <- function(x, ...) {
    as.POSIXct(format = "%Y-%m-%d.%H:%M:%S", tz = "PST", x, ...)
}

#' Retrieve all of the messages from an issue
#'
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

    # merge by closest time per author
    res <- ddply(res, "author",
        function(df) {
            merge_closest(df,
                attachments[attachments$author == df$author[1],
                            names(attachments) != "author", drop = FALSE],
                by = "time")})

    rownames(res) <- res$id

    class(res) <- c("issue", "data.frame")

    res
}

#' Post a message to an issue
#'
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
#'
#' @inheritParams tracker_search
#' @param issue Issue object, or issue number to download files from
#' @param dir Location to store the files
#' @param last_only If \code{TRUE} only download the last submitted tarball.
#' @param pattern Regular expression for files to download.
#' @param overwrite Will only overwrite existing \code{path} if TRUE.
#' @param ... Additional Arguments passed to \code{\link[rvest]{jump_to}}.
#' @export
download <- function(session = tracker_login(), issue, dir = ".", last_only = TRUE,
    pattern = "[.]tar[.]gz$", overwrite = FALSE, ...) {

    # TODO handle msg123213 issue123123 cases
    if (is.character(issue) || is.numeric(issue)) {
        issue <- get_issue(session, issue)
    }

    idx <- grep(pattern, issue$filename)
    if (!length(idx)) {
        stop("No downloads found for issue", issue$id, call. = FALSE)
    }
    if (last_only) {
        idx <- tail(idx, n = 1)
    }
    Map(function(href, filename) {
        if (!is.na(href)) {
            rvest::jump_to(session,
                href,
                httr::write_disk(path = filename, overwrite = overwrite),
                httr::progress(), ...)
        }}, issue$href[idx], issue$filename[idx])
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

#' Retrieve the user list
#' @inheritParams tracker_search
#' @export
users <- function(session = tracker_login(), ...) {
    session <- rvest::jump_to(session, "https://tracker.bioconductor.org/user")
    tbl <- as.data.frame(rvest::html_table(rvest::html_node(session, "table.list")))
    names(tbl) <- c("user_name", "name", "organisation", "email", "phone_number")

    tbl$id <- gsub("user", "", rvest::html_attr(rvest::html_nodes(session, "table.list tr td a"), "href"))
    class(tbl) <- c("tbl_df", "tbl", "data.frame")
    tbl
}

assign_packages_email <- function(pkgs = unassigned_packages(), date = Sys.Date(), code = assign_new_packages(pkgs = pkgs, ...), ...) {
    code <- format(code)

    # replace opening and closing {} with Rmd brackets
    code[c(1, length(code))] <- c("```{r}", "```")

    # remove leading spaces
    code <- gsub("^[[:space:]]{4}", "", code)

    render_markdown()
    code <- knit(text = code, envir = new.env())

    # HACK: markdown here breaks with codeblocks without a language, so set the output codeblock to R
    code <- gsub("```\n\n```\n##", "```\n\n```r\n##", code)

    code <- paste(collapse = "", c(code, "\n\n",
        sprintf("- [%s](https://tracker.bioconductor.org/issue%s)\n", pkgs$title, pkgs$id)))

    message <- gmailr::mime(subject = fmt("Package Assignments For {{date}}", list(date = date)), body = code, to = "devteam-bioc <devteam-bioc@fhcrc.org>", from = "Jim Hester <james.hester@bioconductor.org>")

    message
}

assign_package <- function(pkgs = unassigned_packages(), assignments = NULL, team = devteam, ...) {

    if (is.null(assignments)) {
        assignments <- eval(assign_new_packages(pkgs = pkgs, team = team, ...), envir = new.env())
    }

    if (NROW(pkgs) != length(assignments)) {
        stop("Number of packages must equal number of assignments", call. = FALSE)
    }

  for (i in seq_along(NROW(pkgs))) {
      pkg <- pkgs[i, ]
      assignment <- assignments[i]
      user <- names(team)[team == assignment]
      post_issue(number = pkg$id,
          note = paste0(assignment, " has been assigned to this package"),
          status = "preview-in-progress",
          assignedto = user)
  }
}

fmt <- whisker::whisker.render
