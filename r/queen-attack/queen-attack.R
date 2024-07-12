create <- function(row, col) {
  stopifnot(all(c(row, col) %in% 0:7))
  c(row, col)
}

can_attack <- function(queen1, queen2) {
  dif <- abs(queen1 - queen2)

  any(dif == 0) || (length(unique(dif)) == 1 && unique(dif) != 0)
}
