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
merge_closest <- function(x, y, fun, ...) {

  # generate idx columns for both datasets
  x$idx <- seq_len(NROW(x))
  single_bracket <- function(x, i, j, ..., drop = TRUE) {
    x[i, j, ..., drop = drop]
  }
  row_apply <- function(x, fun, ...) {
      lapply(
          lapply(seq_len(NROW(x)), single_bracket, x = x),
          fun, ...)
  }
  y$idx <- vapply(row_apply(y, fun), function(x) { x <- which.min(x); if (length(x)) x else NA }, integer(1))

  res <- merge(x, y, by = "idx", all.x = TRUE)

  # remove the idx column from result
  res <- res[names(res) != "idx"]

  res
}
