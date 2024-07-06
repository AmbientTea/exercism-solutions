fizz_buzz <- function(n) {
  n <- seq(n)
  fizz <- ifelse(n %% 3 == 0, "Fizz", "")
  buzz <- ifelse(n %% 5 == 0, "Buzz", "")
  fizzbuzz <- trimws(paste(fizz, buzz))
  ifelse(fizzbuzz != "", fizzbuzz, as.character(n))
}

fizz_buzz <- function(n) {
  n <- seq(n)
  ifelse(n %% 15 == 0, "Fizz Buzz",
  ifelse(n %% 5 == 0, "Buzz",
  ifelse(n %% 3 == 0, "Fizz", as.character(n))))
}

fizz_buzz <- function(n) {
  n <- seq(n)
  fizzbuzz <- n
  fizzbuzz[n %% 5 == 0] <- "Buzz"
  fizzbuzz[n %% 3 == 0] <- "Fizz"
  fizzbuzz[n %% 15 == 0] <- "Fizz Buzz"
  fizzbuzz
}
