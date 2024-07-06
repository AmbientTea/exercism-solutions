sieve <- function(limit) {
  stopifnot(0 < limit)

  prime <- c(FALSE, rep(TRUE, limit-1))
  for(n in seq(limit)[prime]) {
    prime[seq(n, limit, by=n)[-1]] = FALSE
  }

  if(any(prime)) seq(limit)[prime]
  else NULL
}
