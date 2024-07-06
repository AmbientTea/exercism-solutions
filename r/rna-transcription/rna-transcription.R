dna_to_rna <- list(
  "G" = "C",
  "C" = "G",
  "T" = "A",
  "A" = "U"
)

to_rna <- function(dna) {
  dna <- unlist(strsplit(dna, ""))
  rna <- dna_to_rna[dna]

  stopifnot(!any(sapply(rna, is.null)))

  paste(rna, collapse="")
}
