library(stringi)

## diamond <- function(letter) {
##   ls <- toupper(letters)
##   ls <- ls[ seq(from=1, to=which(ls == letter)) ]
##   ll <- seq(ls)-1

##   quadrant <- paste(strrep(" ", rev(ll)), ls, strrep(" ", ll), sep="")
##   half <- c(quadrant, rev(quadrant)[-1])
##   whole <- paste(half, sub(".", "", stri_reverse(half)), sep="")
##   paste(whole, collapse = "\n")
## }

diamond <- function(letter) {
  ln <- which(LETTERS == letter)

  dia <- matrix(" ", 2*ln - 1, 2*ln - 1)

  d <- c(seq_len(ln), rev(seq_len(ln-1))) - 1
  for(i in seq(d)) {
    dia[i,c(ln - d[i], ln + d[i])] = LETTERS[1 + d[i]]
  }

  paste(apply(dia, 1, FUN=paste, collapse=""), collapse = "\n")
}
