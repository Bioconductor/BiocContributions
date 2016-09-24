#' See if a person already exists in the user db
#'
#' @param x a person object to lookup
#' @examples
#' pkg <- system.file(package="BiocContributions",
#'   "testpackages", "RNASeqPower_1.11.0.tar.gz")
#'
#' maintainer <- maintainers(pkg)
#' match_user(maintainer)
match_user <- function(x) {
    assert(inherits(x, "person"), "'x' must be a person object")

    db <- user_db()
    users <- db2person(db[match(x$email, db$`E-mail Address`), ])

    attr(users, "input") <- x
    class(users) <- "user_matches"

    # Otherwise try to find name based matches
    unmatched <- lengths(users) == 0
    first_name_matches <- last_name_matches <- replicate(length(users), person())

    match_column <- function(column) {
        function(x) db2person(db[x == tolower(db[[column]]), ])
    }
    first_name_matches[unmatched] <-
        lapply(tolower(x[unmatched]$given), match_column("First Name"))
    last_name_matches[unmatched] <-
        lapply(tolower(x[unmatched]$family), match_column("Last Name"))
    attr(users, "first_matches") <- first_name_matches
    attr(users, "last_matches") <- last_name_matches

    users
}

db2person <- function(x) {
    Map(person,
        given = x$`First Name`,
        family = x$`Last Name`,
        email = x$`E-mail Address`,
        comment = x$`SVN User ID`,
        USE.NAMES = FALSE)
}

no_match <- function(x) {
  vapply(x, function(x) length(x[[1]]) == 0, logical(1))
}

print.user_matches <- function(x, ...) {
    inputs <- lapply(attr(x, "input"), format)
    matches <- lapply(x, format)
    unmatched <- lengths(x) == 0
    if (any(unmatched)) {
        first_matches <- lapply(attr(x, "first_matches")[unmatched],
                                function(x) paste0(collapse = "\n", "  ", lapply(x, format)))
        last_matches <- lapply(attr(x, "last_matches")[unmatched],
                                function(x) paste0(collapse = "\n", "  ", lapply(x, format)))
        matches[unmatched] <- paste0("No Exact Match", "\n",
                                     "First Name Matches:\n",
                                     first_matches, "\n",
                                     "Last Name Matches:\n",
                                     last_matches)
    }
    cat(paste0(collapse = "\n", inputs, "\n", matches, "\n"))
}

#' Extract the email from an object
#'
#' @param x the object to extract email from
#' @param ... Additional arguments passed to methods.
email <- function(x, ...) {
    UseMethod("email")
}

#' @describeIn email person object
email.person <- function(x, ...) {
    x$email
}

#' @describeIn email user match object from \code{\link{match_user}}
email.user_matches <- function(x, ...) {
    format(attr(x, "input"))
}

#' @describeIn email list - calls \code{\link{email}} on every item in the list.
email.list <- function(x, ...) {
    unlist(lapply(x, email))
}

#' @describeIn email - simply returns the character vector unchanged.
email.character <- function(x, ...) {
    x
}

#' Retrieve the maintainers from a tarball
#'
#' @return each maintainer as a 'person' object.
#' @param tarball the package tarball to read
#' @examples
#' pkg <- system.file(package="BiocContributions",
#'   "testpackages", "RNASeqPower_1.11.0.tar.gz")
#' maintainers(pkg)
maintainers <- function(tarball) {
    description <- readDESCRIPTION(tarball)
    .extractEmails(description)
}

#' Retrieve the remote user database
#'
#' This is an internal function which retrieves the user database file
#' 'user_db.csv', which holds user information for users with SVN
#' credentials.  this assumes 'rsync' is available on your path, which
#' is true by default for linux and OSX machines, but probably not for
#' windows.
#' 
#' The information is cached so calling this function repeatably will
#' result in the same information being returned. Use
#' \code{memoise::forget(user_db)} to reset the cache if needed.
#' @return the result is a data.frame of the data
#' @examples
#' user_db()
#' # second call will be nearly instantaneous
#' user_db()
#' # clear the cache
#' memoise::forget(user_db)
#' user_db()
user_db <- memoise::memoise(function() {
    location <- getOption("userDbFile", NULL)
    if (is.null(location)) {
        stop("please set the userDbFile option to the location of 'user_db.csv'", call. = FALSE)
    }

    tmp <- tempfile()
    on.exit(unlink(tmp))
    system2("rsync", args = c(location, tmp))
    read.csv(tmp, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
})

#' Request new credentials for users
#'
#' @param x an object with an \code{\link{email}} method defined.
#' @param sender The name to use in the signature
#' @return A \code{\link[gmailr]{mime}} object.
#' @examples
#' pkg <- system.file(package="BiocContributions",
#'   "testpackages", "RNASeqPower_1.11.0.tar.gz")
#' request_credentials(match_user(maintainers(pkg)))
#' request_credentials("asdf@asd.com")
request_credentials <-
    function(x, sender = getOption("bioc_contributions_signature"))
{
    emails <- email(x)
    gmailr::mime(To = "scicomp@fhcrc.org",
        Subject = "New SVN users for Hedgehog",
        body = fmt(paste0(
            "Could you please create new SVN account(s) on Hedgehog for\n\n",
            "{{{users}}}\n\n",
            "Thanks,\n",
            "  {{{sender}}}"),
        list(users = paste0(collapse = "\n", emails),
            sender = sender)))
}

username <- function(x, ...) {
    UseMethod("username")
}

username.person <- function(x, ...) {
    x$comment
}

username.list <- username.user_matches <- function(x, ...) {
    unlist(lapply(x, username))
}
