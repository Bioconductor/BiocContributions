#' Make a GitHub API request using an OAuth token
#'
#' @param uri github uri such as '/orgs/octokit/repos'
#' @param method httr verb function (GET, POST, etc)
#' @param postdata data to POST (if method == POST)
#' @param include_message Whether to include the message in the result
#' @export
#' @import httr
#' @return a data frame based on the result from github

# FIXME - currently not dealing with pagination at all!
# https://developer.github.com/v3/#pagination
# https://developer.github.com/guides/traversing-with-pagination

make_github_request <- function(uri, method=GET, postdata=NULL,
  include_message=FALSE)
{
    stopifnot(nchar(Sys.getenv("GITHUB_TOKEN")) > 0)
    url <- sprintf("https://api.github.com%s", uri)
    token <- sprintf("token %s", Sys.getenv("GITHUB_TOKEN"))
    args <- list(url=url, add_headers(Authorization=token))
    if (!is.null(postdata))
        args$body <- postdata
    # args <- list(url="http://httpbin.org/headers", body=postdata, add_headers(Authorization="token haha"))
    response <- do.call(method, args)
    # FIXME - do error handling here based on status_code(response)?
    results_to_data_frame(content(response), include_message)
}

get_tracker_repos <- function()
    "dtenenba/settings" # change this when the time is right

#' Query the issue tracker
#'
#' @param columns which columns to return
#' @param sort A column to sort the data by
#' @param filter what columns are used to filter
#' @param status the status codes used to filter
#' @param ... Additional query parameters
#' @param session the HTTP session to use
#' @export
#' @examples
#' tracker_search("@search_text" = "normalize450k")
gh_tracker_search <-
    function(columns = c("id", "activity", "title", "creator", "status"),
             sort = desc("activity"),
             filter=c("status", "assignedto"),
             status = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
             ...,
             session = tracker_login())
{
    # see https://developer.github.com/v3/search/#search-issues
    # for documentation on searching issues via the api
    # here is an example search:
    # https://api.github.com/search/issues?q=+repo:dtenenba/settings+state:open&sort=created&order=asc
}


# Want to end up with something like this:
# > unassigned_packages()
#     id            activity                           title creator
# 1 1437 2016-03-23 14:06:08                            EGAD     881
# 2 1432 2016-03-23 07:44:36                       DAPARdata     837
# 3 1434 2016-03-22 16:04:49                         CHRONOS     943
# 4 1436 2016-03-22 15:20:14                    MultiDataSet     872
# 5 1435 2016-03-22 13:26:07                          BgeeDB     944
# 6 1433 2016-03-22 10:08:17                       pqsfinder     445
# 7 1430 2016-03-21 20:53:12                    ImmuneSpaceR     609
# 8 1431 2016-03-21 01:48:26 FlowSorted.CordBloodNorway.450k     305
# 9 1429 2016-03-18 16:23:07                     pcaExplorer     669
#                status
# 1         new-package
# 2         new-package
# 3         new-package
# 4         new-package
# 5           sent-back
# 6           sent-back
# 7           sent-back
# 8         new-package
# 9 preview-in-progress



as.data.frame.search.results  <- function(results, include_message)
{
    rows <- results$total_count
    if (rows == 0)
        return(NULL)
    items <- results$items
    # there will be 5 columns (id, activity, title, creator, status)
    cols <- 5
    id <- integer(rows)
    activity <- seq( as.Date("1970-01-01"), by=1, len=rows)
    title <- character(rows)
    creator <- character(rows)
    status <- character(rows)
    message <- character(rows)
    for (i in 1:length(items))
    {
        item <- items[[i]]
        id[i] <- item$number
        activity[i] <- strptime(item$updated_at, "%Y-%m-%dT%H:%M:%S", tz="UTC")
        title[i] <- item$title
        creator[i] <- item$user$login
        if (length(item$labels)) {
            labels <- sort(unlist(lapply(item$labels, function(x) x$name)))
            status[i] <- paste(labels, collapse=", ")
        } else {
            status[i] <- NA
        }
        if (include_message)
        {
            message[i] <- item$body
        }
    }
    df <- data.frame(id, activity, title, creator, status)
    if (include_message)
        df <- cbind(df, message)
    df
}

as.data.frame.issue.results  <- function(results, include_message)
{
    # [1] "id"       "message"  "href"     "filename" "filetype" "author"   "time"
    l = list(id=results$number, href=results$html_url, #filename=...,
        author=results$user$login, time=results$created_at)
    if (include_message)
        l = append(l, list(message=results$body), 1)
    as.data.frame(l)
}

as.data.frame.issue.comments.results <- function(results, include_message)
{

}


results_to_data_frame <- function(results, include_message=FALSE)
{
    if (length(results) == 0) return(NULL)
    # what kind of results do we have?
    if (!is.null(results$total_count)) # search results
    {
        class(results) <- "search.results"
        # return(search_results_to_data_frame(results, include_message))
    } else if (!is.null(results$number))  {# issue results
        class(results) <- "issue.results"
        # return(issue_results_to_data_frame(results, include_message))
    } else if (is.list(results[[1]]) && !is.null(results[[1]]$id)) { # issue comments results
        class(results) <- "issue.comments.results"
        # return(issue_comments_results_to_data_frame(results, include_message))
    } else {
        stop("unknown results")
    }
    as.data.frame(results, include_message)
}



github_search <- function(params)
{
    url <- sprintf("/search/issues?q=+repo:%s+state:open+%s&sort=created&order=desc",
        get_tracker_repos(), params)
    make_github_request(url, "issue_search")
}


#' Show unassigned packages
#'
#' @export
#' @return a data frame of unassigned packages or NULL if none.
#' @examples
#' gh_unassigned_packages()
gh_unassigned_packages <- function()
{
    github_search("no:assignee")
}

#' Show pre-accepted packages
#'
#' @export
#' @return a data frame of pre-accepted packages or NULL if none.
#' @examples
#' gh_pre_accepted_packages()
gh_pre_accepted_packages <- function()
{
    github_search("label:pre-accepted")
}

#' Show packages assigned to a github user
#'
#' @param github_username The github username
#' @export
#' @return a data frame of packages assigned to \code{github_username}
#' @examples
#' gh_packages_assigned_to("dtenenba")

gh_packages_assigned_to <- function(github_username)
{
    github_search(sprintf("assignee:%s", github_username))
}


#' Retrieve all messages associated with an issue
#'
#' @param number The issue number
#' @export
#' @return a data frame/issue of messages associated with issue \code{number}
#' @examples
#' gh_issue(21)


# forget about fancy issue S3 class for now. the colnames returned by the old
# version of this function are:
# [1] "id"       "message"  "href"     "filename" "filetype" "author"   "time"
gh_issue <- function(number)
{
    make_github_request(sprintf("/repos/%s/issues/%s", get_tracker_repos(),
        number))
    github_search(sprintf("assignee:%s", github_username))
}
