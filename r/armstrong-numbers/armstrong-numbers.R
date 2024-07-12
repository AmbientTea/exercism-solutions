digits <- function(n) {
  l10 = if(n == 0) 0 else floor(log10(n))
  floor(n / 10^(l10:0)) %% 10
}

is_armstrong_number <- function(n) {
  ds <- digits(n)

  n == sum(ds ^ length(ds))
}
