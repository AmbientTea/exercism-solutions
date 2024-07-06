collatz <- function(n) {
  stopifnot(0 < n)
  steps <- 0
  while(1 < n) {
    steps = steps + 1
    n = if(n %% 2 == 0) n/2 else 3*n+1
  }
  steps
}

collatz_step_counter <- function(n) {
  stopifnot(all(0 < n))

  sapply(n, collatz)
}
