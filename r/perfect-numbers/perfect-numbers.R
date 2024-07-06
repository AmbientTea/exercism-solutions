aliquot <- function(n) {
  stopifnot(all(0 < n))

  sapply(n, function(n) {
    factors <- seq(n-1)
    if(n == 1) 0 else sum(factors[n %% factors == 0])
  })
}

number_type <- function(n){
  alq <- aliquot(n)
  ifelse(n == alq, "perfect", ifelse(n < alq, "abundant", "deficient"))
}
