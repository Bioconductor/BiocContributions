#' Members of the devteam
devteam <- c(
    "Martin Morgan",
    "Valerie Obenchain",
    "Herve Pages",
    "Dan Tenenbaum")

#' and their github usernames
github_usernames <- c(
    "mtmorgan",
    "vobencha",
    "hpages",
    "dtenenba"
    )
#' Status code to name mapping
# I would prefer these were dynamic, but there doesn't seem to be a great way
# to do so...
status_map <- c(
    "1" = "new-package",
    "2" = "preview-in-progress",
    "3" = "sent-back",
    "4" = "modified-package",
    "5" = "review-in-progress",
    "6" = "accepted",
    "7" = "rejected",
    "8" = "closed",
    "9" = "pre-accepted",
    "10" = "testing")

#' storage object
the <- new.env(parent=emptyenv())

#' Login to the issue tracker
#'
#' @param url tracker url
#' @param user username used to login
#' @param password password used to login
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
#' @param columns which columns to return
#' @param sort A column to sort the data by
#' @param filter what columns are used to filter
#' @param status the status codes used to filter
#' @param ... Additional query parameters
#' @param session the HTTP session to use
#' @examples
#' tracker_search("@search_text" = "normalize450k")
tracker_search <-
    function(columns = c("id", "activity", "title", "creator", "status"),
             sort = desc("activity"),
             filter=c("status", "assignedto"),
             status = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
             ...,
             session = tracker_login())
{
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
    ## The following line now uses readr::read_csv(), which doesn't
    ## perform as well here as utils::read.csv().
    #data <- httr::content(res$response)
    data <- utils::read.csv(textConnection(httr::content(res$response, "text")))
    data$status <- status_map[data$status]
    data$activity <- roundup_datetime(data$activity)
    data
}

keyword_map <- memoise::memoise(function(url = "https://tracker.bioconductor.org/keyword?@template=item",
                                         session = tracker_login()) {
    res <- rvest::jump_to(session,
                          url)
    keyword_links <- rvest::html_nodes(res, "table.otherinfo td a")
    keyword_numbers <- sub("keyword", "", rvest::html_attr(keyword_links, "href"))
    keyword_names <- rvest::html_text(keyword_links)
    names(keyword_names) <- keyword_numbers
    keyword_names
})

#' @describeIn tracker_search retrieve unassigned packages
unassigned_packages <- function(status = c(-1, 1, 2, 3, 4, 5, 9), ..., session = tracker_login()) {
    tracker_search(session = session, status = status, assignedto = -1)
}

#' @describeIn tracker_search retrieve pre-accepted packages
pre_accepted_packages <-
    function(status = 9, ..., session = tracker_login())
{
    tracker_search(session = session, status = status)
}

#' Accept a package on the tracker
#'
#' @inheritParams post
#' @param note The acceptance note to post to the tracker.
#' @examples
#' \dontrun{
#' accept_package(1318, "transcriptR_0.99.4.tar.gz")
#' }
accept_package <- function(issue = issue,
                           tarball,
                           note = accept_note(tarball),
                           status = 6,
                           ...,
                           session = NULL) {
    post(issue = issue, session = session, note = note, status = status, ...)
}

#' @describeIn tracker_search retrieve the logged in users packages
my_issues <-
    function(user = NULL, status = c(-1, 1, 2, 3, 4, 5, 9, 10), ...,
             session = tracker_login())
{
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

#' Assign new packages
#'
#' This method uses a hash digest to assign the packages based on the package
#' name.
#' @param pkgs the packages to assign
#' @param team team members to assign to
assign_new_packages <- function(pkgs = unassigned_packages(session),
                                team = devteam) {

    if (NROW(pkgs) == 0)
        return(list())

    pkgs <- pkgs[order(pkgs$id),,drop=FALSE]

    substitute({

        ids <- ids_

        title <- title_

        team <- team_

        data.frame(reviewer = team[ids %% length(team) + 1],
                   title = as.character(title))
    },
    list(ids_ = pkgs$id, title_ = pkgs$title, team_ = team))
}

#' Retrieve all of the messages from an issue
#'
#' @param number the issue number to retrieve
#' @inheritParams tracker_search
issue <- function(number, session = tracker_login()) {
    response <- rvest::jump_to(session, paste0("/issue", number))

    rows <- rvest::html_nodes(response, ".messages tr")
    if (!NROW(rows)) {
        return(NULL)
    }

    metadata <- rows[seq(2, length(rows), 2)]

    parse_metadata <- function(x) {
        headers <- rvest::html_nodes(x, "th")
        data.frame(
            id = rvest::html_attr(rvest::html_nodes(headers, "a"), "href"),
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
    close_times <- function(y, cutoff = 5) {
        function(x) {
            times <- abs(difftime(x$time, y$time, units = "secs"))
            times[times > cutoff] <- NA
            times
        }
    }
    res <- ddply(res, "author",
        function(df) {
            att <- attachments[attachments$author == df$author[1], , drop = FALSE]
            merge_closest(df, att, close_times(df))
        })

    res$author <- res$author.x
    res[c("author.x", "author.y")] <- list(NULL)

    res$time <- res$time.x
    res[c("time.x", "time.y")] <- list(NULL)

    rownames(res) <- deduplicate(res$id)

    res <- res[order(res$time), ]

    attr(res, "session") <- response

    class(res) <- c("issue", "data.frame")

    res
}

#' Coerce to an issue object
#'
#' @param x object to be coerced
#' @param ... Additional arguments passed to methods
as.issue <- function(x, ...) UseMethod("as.issue")

as.issue.issue <- function(x, ...) x

as.issue.numeric <- function(x, ...) issue(number = x, ...)

as.issue.integer <- as.issue.numeric

as.issue.character <- as.issue.numeric

#' Post a message to an issue
#'
#' @param issue an issue object from \code{\link{issue}}
#' @param session the session to use, if \code{NULL} the issue's session is used.
#' @param note a note to post to the issue, defaults to opening your editor,
#' but you can also pass a character string.
#' @param file a file to attach to the issue, if \code{TRUE} choose a file using
#' \code{\link{file.choose}}
#' @param ... Additional arguments passed to rvest::set_values
post <- function(issue, session = NULL, note = edit(), file = NULL, ...) {
    issue <- as.issue(issue)

    if (is.null(session)) {
        session <- session(issue)
    }

    if (isTRUE(file)) {
        file <- file.choose()
    }
    form <- rvest::html_form(
        rvest::html_nodes(session, "form[name='itemSynopsis']")[[1]])

    form <-
        rvest::set_values(form,
            `@note` = note,
            `@file` = file,
            ...)

    rvest::submit_form(session, form)
}

session <- function(...) UseMethod("session")

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
download <- function(issue,
                     dir = proj_path(),
                     last_only = TRUE,
                     pattern = "[.]tar[.]gz$",
                     overwrite = FALSE,
                     ...,
                     session = tracker_login()) {
    issue <- as.issue(issue)

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
                httr::write_disk(path = file.path(dir, filename),
                                 overwrite = overwrite),
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

print.issue <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    invisible(x)
}

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
users <- memoise::memoise(function(session = tracker_login()) {
    session <- rvest::jump_to(session, "https://tracker.bioconductor.org/user")
    tbl <- as.data.frame(rvest::html_table(rvest::html_node(session, "table.list")))
    names(tbl) <- c("user_name", "name", "organisation", "email", "phone_number")

    tbl$id <- gsub("user", "", rvest::html_attr(rvest::html_nodes(session, "table.list tr td a"), "href"))
    class(tbl) <- c("tbl_df", "tbl", "data.frame")
    tbl
})

#' Generate the package assignments email given code to run
#' @param code code to run, output of \code{\link{assign_new_packages}()}
#' @param date date to title the email
#' @param additional arguments passed to \code{\link{assign_new_packages}()}
package_assignment_email <- function(pkgs = unassigned_packages(...),
                                     code = assign_new_packages(pkgs, ...),
                                     date = Sys.Date(),
                                     ...) {
    code <- format(code)

    # replace opening and closing {} with Rmd brackets
    code[c(1, length(code))] <- c("```{r}", "```")

    # remove leading spaces
    code <- gsub("^[[:space:]]{4}", "", code)

    env <- new.env()
    knitr::render_markdown()
    code <- knitr::knit(text = code, envir = env)

    # HACK: markdown here breaks with codeblocks without a language, so set the
    # output codeblock to R
    code <- gsub("```\n\n```\n##", "```\n\n```r\n##", code)

    # match packages in code to passed in packages
    pkgs <- pkgs[match(env$pkgs, pkgs$title), ]

    code <- paste(collapse = "", c(code, "\n\n",
        sprintf("- [%s](https://tracker.bioconductor.org/issue%s)\n",
                pkgs$title, pkgs$id)))

    code <- gsub("\n", "<br>", code)

    message <- gmailr::mime(subject = fmt("Package Assignments For {{date}}",
            list(date = date)),
        to = "devteam-bioc <devteam-bioc@fhcrc.org>",
        from = sprintf("%s <%s>", getOption("bioc_contributions_signature"),
                       getOption("bioc_contributions_email")))
    message <- gmailr::html_body(message, code)

    message
}

#' @describeIn assign_package assign multiple packages with assignment code
assign_packages <- function(pkgs, code = assign_new_packages(...), ...) {
    assignments <- eval(code, envir = baseenv())

    x <- merge(pkgs, assignments)

    Map(assign_package, x$id, x$reviewer)
}

#' Assign a specific package
#'
#' @param number issue number
#' @param assignment lookup assignee by name
assign_package <- function(issue, assignee, ...) {
    issue <- as.issue(issue)

    # lookup tracker username for assignee
    user <- subset(users(), name == assignee)$user_name

    post(issue = issue,
               note = paste0(assignee, " has been assigned to this package"),
               status = "preview-in-progress",
               assignedto = user,
               ...)
}

accept_note <-
    function(tarball, type = c("software", "experiment-data"),
             senderName = getOption("bioc_contributions_signature",
                                    "Bioconductor"))
{
    type <- match.arg(type)
    description <- readDESCRIPTION(tarball)
    email <- .extractEmails(description)
    author <- paste(vapply(email$given, "[[", "", 1), collapse=", ")

    switch(type,
           software = template(
               "tracker.txt",
               author = author,
               tarball = basename(tarball),
               package = description$Package,
               senderName = senderName,
               when = "Everyday",
               type = "bioc-LATEST"),
           `experiment-data` = template(
               "tracker.txt",
               author = author,
               tarball = basename(tarball),
               package = description$Package,
               senderName = senderName,
               when = "Wednesday and Saturday",
               type = "data-experiment-LATEST")
           )
}

attachment_size <-
    function(issue, session = tracker_login())
{
    issue <- as.issue(issue)

    size <- function(x) {
        res <- httr::HEAD(paste0(session$url, x), session$config,
                          handle = session$handle)

        as.numeric(httr::headers(res)$`content-length`)
    }
    vapply(na.omit(issue$href), size, numeric(1))
}
