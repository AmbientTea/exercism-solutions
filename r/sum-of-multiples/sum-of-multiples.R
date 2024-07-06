sum_of_multiples <- function(factors, limit) {
   n <- seq(limit-1)

   divisors <- lapply(n, function(n) n %% factors == 0) %>% sapply(any)

   sum(n[divisors])
}
