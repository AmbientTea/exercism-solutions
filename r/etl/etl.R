etl <- function(input) {
  mapping <- list()
  for(value in names(input)) {
    letters <- tolower(input[[value]])
    mapping[letters] <- as.numeric(value)
  }

  mapping[order(names(mapping))]
}
