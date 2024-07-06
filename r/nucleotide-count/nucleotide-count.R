nucleotide_count <- function(input) {
  counts <- list(
    A = 0,
    C = 0,
    G = 0,
    T = 0
  )

  for(ch in unlist(strsplit(toupper(input), ""))) {
    stopifnot(ch %in% names(counts))

    counts[ ch ] = unlist(counts[ch]) + 1
  }

  counts

}
