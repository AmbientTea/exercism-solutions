is_pangram <- function(input) {
  gsub("[^a-zA-Z]", "", input) %>%
    tolower() %>%
    strsplit("") %>%
    table() %>%
    length(.) == 26
}
