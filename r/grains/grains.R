square <- function(n) {
  stopifnot(all(0 < n & n < 65))
  2^(n-1)
}

total <- function() {
  sum(square(1:64))
}
