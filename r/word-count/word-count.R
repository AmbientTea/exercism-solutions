library(dplyr)

sanitize <- function(str) {
    gsub("^'|'$", "", str)
}

word_count <- function(input) {
  words <- input %>%
    tolower %>%
    strsplit(split="[^a-z0-9']") %>%
    unlist %>%
    sanitize

  words[words != ""] %>%
    table %>%
    as.list
}
