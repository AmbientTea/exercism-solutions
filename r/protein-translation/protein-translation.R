library(magrittr)

proteins <- rbind(
  data.frame(codon = c("AUG"), protein = "Methionine"),
  data.frame(codon = c("UUU", "UUC"), protein = "Phenylalanine"),
  data.frame(codon = c("UUA", "UUG"), protein = "Leucine"),
  data.frame(codon = c("UCU", "UCC", "UCA", "UCG"), protein = "Serine"),
  data.frame(codon = c("UAU", "UAC"), protein = "Tyrosine"),
  data.frame(codon = c("UGU", "UGC"), protein = "Cysteine"),
  data.frame(codon = c("UGG"), protein = "Tryptophan"),
  data.frame(codon = c("UAA", "UAG", "UGA"), protein = "STOP")
) %>% { setNames(.$protein, .$codon) }

chunk <- function(v, size) {
  split(v, (seq(v) - 1) %/% size)
}

take_until <- function(v, f) {
  ind <- which(f(v))[1]
  if(is.na(ind)) v else head(v, ind-1)
}

translate <- function(bases) {
  prots <- unlist(strsplit(bases, "")) %>%
    chunk(3) %>%
    sapply(paste, collapse = "") %>%
    unlist() %>%
    proteins[.] %>%
    take_until(function(prot) prot == "STOP") %>%
    as.vector()

  stopifnot(!any(is.na(prots)))

  switch(length(prots) != 0, prots)
}
