roman_digits <- c(
  M = 1000,
  CM = 900,
  D = 500,
  CD = 400,
  C = 100,
  XC = 90,
  L = 50,
  XL = 40,
  X = 10,
  IX = 9,
  V = 5,
  IV = 4,
  I = 1
)

roman <- function(arabic) {
  rom = c()
  for(i in seq(roman_digits)) {
    digit <- roman_digits[i]
    name <- names(roman_digits)[i]
    rom = c(rom, rep(name, arabic %/% digit))
    arabic = arabic %% digit
  }

  paste(rom, collapse = "")
}
