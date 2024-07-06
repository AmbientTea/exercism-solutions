sorted_str <- function(str) {
    sapply(lapply(strsplit(str,""), sort), paste, collapse="")
}

anagram <- function(subject, candidates) {
  subject_u <- toupper(subject)
  candidates_u <- toupper(candidates)

  anagrams <- subject_u != candidates_u & sorted_str(subject_u) == sorted_str(candidates_u);

  if(any(anagrams))
    candidates[anagrams]
  else
    NULL
}
