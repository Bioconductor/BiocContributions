tracker_spreadsheet <- function(x) {
  if (is.null(the$ss)) {
      the$ss <- googlesheets::gs_title("Bioconductor Pkg Review")
  }
  the$ss
}

#' Add packages to the tracking worksheet
#'
#' This function uses the googlesheets package to add data retrieved from the
#' tracker.
#' @param x issues to add
#' @param ws The worksheet number or name to append to.
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_add_row}}
#' @export
#' @examples
#' \dontrun{
#' pkgs <- unassigned_packages()
#' add_to_spreadsheet(pkgs)
#' }
add_to_spreadsheet <- function(x, ..., ws = 1, ss = tracker_spreadsheet()) {
    data <- data.frame("Package to be added" = x$title,
        "Issue" = paste0("https://tracker.bioconductor.org/issue", x$id),
        "Github" = "",
        "accepted on tracker" = x$activity,
        "type of package" = vapply(x$keyword, paste, collapse = ", ", character(1)),
        check.names = FALSE,
        stringsAsFactors = FALSE)
    apply(data, 1, googlesheets::gs_add_row, ss = tracker_spreadsheet(), ws = ws, ...)
}
