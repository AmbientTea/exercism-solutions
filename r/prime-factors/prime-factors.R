prime_factors <- function(number) {
  factors = double()
  factor <- 2
  while(factor <= number) {
    if(number %% factor == 0) {
      factors <- c(factors, factor)
      number = number / factor
    } else {
      factor = factor + 1
    }
  }

  switch(length(factors) != 0, factors)
}
