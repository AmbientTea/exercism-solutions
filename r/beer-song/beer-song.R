lyrics <- function(first, last) {
  paste(verse(first:last), collapse = "\n")
}

verse <- function(n) {
  paste(verse1(n), "\n", verse2(n), "\n", sep="")
}

verse1 <- function(n) {
  ifelse(n == 0, "No more bottles of beer on the wall, no more bottles of beer.",
         paste(n, bottle(n), "of beer on the wall,", n, bottle(n), "of beer."))
}

verse2 <- function(n) {
  ifelse(n == 0, "Go to the store and buy some more, 99 bottles of beer on the wall.",
         ifelse(n == 1, "Take it down and pass it around, no more bottles of beer on the wall.",
                paste("Take one down and pass it around,", n-1, bottle(n-1), "of beer on the wall.")))
}

bottle <- function(n) {
  ifelse(n < 2, "bottle", "bottles")
}
