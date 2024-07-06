acronym <- function(input) {
  words <- unlist(strsplit(input, " |-|_"))

  words %>%
    substring(1, 1) %>%
    paste(collapse="") %>%
    toupper()
}
