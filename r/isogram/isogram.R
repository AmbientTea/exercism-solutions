is_isogram <- function(word) {
  gsub("[^a-zA-Z]", "", word) %>%
    tolower() %>%
    strsplit("") %>%
    sapply(anyDuplicated) %>%
    !.
}
