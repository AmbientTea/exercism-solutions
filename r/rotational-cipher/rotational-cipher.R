library(dplyr)

rot <- function(ascii, key) {
  a <- as.numeric(charToRaw("a"))
  A <- as.numeric(charToRaw("A"))

  ifelse(
    ascii < a,
    A + (ascii + key - A) %% 26,
    a + (ascii + key - a) %% 26
  )
}

rotate <- function(text, key) {
  ascii <- utf8ToInt(text);

  ifelse(charClass(text, "alpha"), rot(ascii, key), ascii) %>%
    intToUtf8
}
