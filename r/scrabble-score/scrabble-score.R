scores = rbind(
  data.frame(letter = c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T"), score = 1),
  data.frame(letter = c("D", "G"), score = 2),
  data.frame(letter = c("B", "C", "M", "P"), score = 3),
  data.frame(letter = c("F", "H", "V", "W", "Y"), score = 4),
  data.frame(letter = c("K"), score = 5),
  data.frame(letter = c("J", "X"), score = 8),
  data.frame(letter = c("Q", "Z"), score = 10)
)

scrabble_score <- function(input){
  toupper(input) %>%
    strsplit("") %>%
    unlist() %>%
    data.frame(letter = .) %>%
    merge(scores) %>%
    { sum(. $ score) }
}
