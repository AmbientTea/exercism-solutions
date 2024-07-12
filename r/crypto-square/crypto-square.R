normalized_plaintext <- function(input) {
  gsub("[^a-z0-9]", "", tolower(input))
}

normalized_chars <- function(input) {
  chars <- unlist(strsplit(normalized_plaintext(input), ""))

  if(length(chars) == 0) "" else chars
}

fill_to <- function(v, elem, len) {
  c(v, rep(elem, len))[seq_len(len)]
}

segments <- function(m) {
  apply(m, 1, paste, collapse="")
}

square_matrix <- function(chars, fill) {
  len <- length(chars)
  ncols <- ceiling(sqrt(len))
  nrows <- ceiling(len / ncols)
  m <- matrix(fill_to(chars, fill, ncols * nrows), nrows, ncols, byrow=TRUE)
}

plaintext_segments <- function(input) {
  normalized_chars(input) %>%
    square_matrix("") %>%
    segments()
}

encoded <- function(input) {
  normalized_chars(input) %>%
    square_matrix("") %>%
    t() %>%
    segments() %>%
    paste(collapse="")
}

ciphertext <- function(input) {
  normalized_chars(input) %>%
    square_matrix(" ") %>%
    t() %>%
    segments() %>%
    paste(collapse=" ")
}
