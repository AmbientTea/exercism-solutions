allergy_key <- c(
  "eggs",
  "peanuts",
  "shellfish",
  "strawberries",
  "tomatoes",
  "chocolate",
  "pollen",
  "cats"
  )
bits <- 2^(seq(allergy_key)-1)

allergy <- function(num) {
  bitwAnd(bits, num) %>%
    as.logical() %>%
    setNames(allergy_key)
}

allergic_to <- function(allergy_object, allergy) {
  allergy_object[[allergy]]
}

list_allergies <- function(allergy_object) {
  names(allergy_object)[allergy_object]
}
