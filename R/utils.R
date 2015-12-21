# simple implementation of plyr::ddply
# subset a data.frame by grouping variables and apply a function to each group
ddply <- function(x, by, fun, ...) {
    do.call(rbind,
        by(simplify = FALSE,
            x,
            lapply(by, getElement, object = x),
            fun))
}

# use findInterval to merge x and y by the closest type
# @param by the column to merge by
# @param decreasing to sort results increasing or decreasing
merge_closest <- function(x, y, by, decreasing = FALSE) {

  # findInterval needs to be sorted ascending
  x <- x[order(x[[by]]), ]
  y <- y[order(y[[by]]), ]

  # generate idx columns for both datasets
  x$idx <- seq_len(NROW(x))
  y$idx <- findInterval(y[[by]], x[[by]])

  x <- merge(x, y[, names(y) != by], by = "idx", all.x = TRUE)

  # remove the idx column from result
  x <- x[names(x) != "idx"]

  if (isTRUE(decreasing)) {
      x[order(x, decreasing = decreasing), ]
  } else {
      x
  }
}
