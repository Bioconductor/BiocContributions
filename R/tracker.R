tracker_login <- function(
    url = "https://tracker.bioconductor.org",
    user = getOption("tracker_login"),
    password = getOption("tracker_password")) {

    session <- rvest::html_session(url)

    login <- rvest::set_values(rvest::html_form(session)[[3]],
        `__login_name` = user,
        `__login_password` = password)

    rvest::submit_form(tracker, login)
}
tracker_search <- function(session,
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
    data$activity <- as.POSIXlt(data$activity,
                                format = "%Y-%M-%d.%H:%M:%S",
                                tz = "PST")
    data
}

unassigned_packages <- function(session, status = c(-1, 1), ...) {
    tracker_search(session = session, status = status)
}

assign_new_packages <- function(session = tracker_login(),
                                team = c("Jim Hester",
                                         "Martin Morgan",
                                         "Valerie Obenchain",
                                         "Hervé Pagès",
                                         "Dan Tenenbaum"),
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


assign_new_packages_hash <- function(session = tracker_login(),
                                team = c("Jim Hester",
                                         "Martin Morgan",
                                         "Valerie Obenchain",
                                         "Hervé Pagès",
                                         "Dan Tenenbaum"),
                                pkgs = unassigned_packages(session)) {

    integer_hash <- function(x) {
        hash <- vapply(x, digest::digest, character(1))
        hash <- substr(hash, 1, 6)
        strtoi(hash, base = 16)
    }

    substitute({
        integer_hash <- fun_

        pkgs <- pkgs_

        team <- team_

        setNames(team[integer_hash(pkgs) %% length(team) + 1], pkgs)
    },
    list(fun_ = integer_hash, pkgs_ = pkgs$title, team_ = team))
}

desc <- function(x) {
    if (is.numeric(x)) {
        -x
    } else {
        paste0("-", x)
    }
}
