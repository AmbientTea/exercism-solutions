saddle_point <- function(input) {
  r <- apply(input, 1, max)
  c <- apply(input, 2, min)

  data.frame(which(input == r & t(t(input) == c), arr.ind=TRUE))
}
