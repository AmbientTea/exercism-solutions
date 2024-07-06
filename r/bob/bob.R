
bob <- function(input) {
  input <- gsub("[^a-zA-Z0-9?]", "", input)

  # silence
  if(input == "")
    return("Fine. Be that way!")

  chars <- unlist(strsplit(input, ""))
  letters <- chars[charClass(input, "alpha")]

  question <- tail(chars, 1) == "?"
  yell <- length(letters) & all(letters == toupper(letters))

  if(question & yell)
    "Calm down, I know what I'm doing!"
  else if(yell)
    "Whoa, chill out!"
  else if(question)
    "Sure."
  else
    "Whatever."

}
