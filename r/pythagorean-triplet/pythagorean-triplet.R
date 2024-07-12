pythagorean_triplet <- function(n) {
  result <- list()

  for(a in seq_len(n %/% 3)) {
    for(b in seq(from = a+1, to = (n-a) %/% 2)) {
      c = n - a - b
      if(a^2 + b^2 == c^2) {
        result = c(result, list(c(a, b, c)))
      }
    }
  }


  result
}
