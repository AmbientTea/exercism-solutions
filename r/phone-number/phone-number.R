parse_phone_number <- function(number_string) {
  chars <- unlist(strsplit(number_string, split = ""))
  digits <- as.numeric(chars[charClass(number_string, "digit")]);

  if(length(digits) == 11) {
    if(digits[1] == 1)
      digits <- tail(digits, -1)
    else
      return(NULL)
  }

  if(length(digits) == 10 && all(digits[c(1,4)] > 1))
    paste(digits, collapse="")
  else
    NULL
}
