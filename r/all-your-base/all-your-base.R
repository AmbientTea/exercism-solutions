
from_base <- function(digits, base) {
  sum(digits * base ^ (rev(seq_len(length(digits))) - 1))
}

to_base <- function(n, base) {
  result <- list()
  i <- 1
  while(n > 0) {
    result[i] <- n %% base
    n <- n %/% base
    i <- i + 1
  }

  rev(unlist(result))
}

rebase <- function(from, digits, to) {
  if(from < 2) stop("input base must be >= 2")
  if(!all(0 <= digits & digits < from))
    stop("all digits must satisfy 0 <= d < input base")
  if(to < 2) stop("output base must be >= 2")

  digits %>%
    from_base(from) %>%
    to_base(to) %>%
    if(length(.) != 0) . else 0
}
