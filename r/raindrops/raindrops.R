raindrops <- function(number) {
  drops <- c("Pling" = 3, "Plang" = 5, "Plong" = 7);
  sounds <- names(drops)[number %% drops == 0];

  if(length(sounds))
    paste(sounds, collapse = "")
  else
    toString(number)
}
