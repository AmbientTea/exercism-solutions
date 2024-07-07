pascals_triangle <- function(n) {
  stopifnot(is.numeric(n))

  rows <- list()
  next_row <- c(1)

  for(rn in seq_len(n)) {
    rows <- c(rows, list(next_row))

    next_row <- c(1, head(next_row, -1) + tail(next_row, -1), 1)
  }

  rows
}
