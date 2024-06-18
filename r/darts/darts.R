score <- function(x, y) {
  dist <- x^2 + y^2
  if(dist > 100) {
    0
  } else if(dist > 25) {
    1
  } else if(dist > 1) {
    5
  } else {
    10
  }
}
