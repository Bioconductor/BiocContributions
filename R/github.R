#' Make a GitHub API request using an OAuth token
#'
#' @param uri github uri such as '/orgs/octokit/repos'
#' @param method httr verb function (GET, POST, etc)
#' @param postdata data to POST (if method == POST)
#' @export
#' @import httr
#' @return an R object created by reading the JSON of the response


make_github_request <- function(uri, method=GET, postdata=NULL)
{
    stopifnot(nchar(Sys.getenv("GITHUB_TOKEN")) > 0)
    url <- sprintf("https://api.github.com%s", uri)
    token <- sprintf("token %s", Sys.getenv("GITHUB_TOKEN"))
    args <- list(url=url, add_headers(Authorization=token), body=postdata)
    # args <- list(url="http://httpbin.org/headers", body=postdata, add_headers(Authorization="token haha"))
    response <- do.call(method, args)
    content(response)
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
