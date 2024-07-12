shift <- function(v, n) {
  c(tail(v, -n), head(v, n))
}

triangle <- function(x, y, z) {
  tr <- sort(c(x, y, z))
  stopifnot(all(tr > 0))
  stopifnot(all(tr + shift(tr, 1) > shift(tr, 2)))

  lens <- length(unique(tr))

  structure(tr, class = c(
    if(lens == 3) "scalene",
    if(lens < 3) "isosceles",
    if(lens == 1) "equilateral"
  ))
}
