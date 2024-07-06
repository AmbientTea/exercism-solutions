# Determine whether the number is valid.
is_valid <- function(input) {
  input <- gsub(" ", "", input)

  if(any(!charClass(input, "digit")) | nchar(input) < 2)
    return(FALSE)

  digits <- rev(as.numeric(charToRaw(input)) - as.numeric(charToRaw("0")))
  digits <- ifelse(seq(digits) %% 2 == 0, 2 * digits, digits)
  digits <- ifelse(digits > 9, digits - 9, digits)

  sum(digits) %% 10 == 0
}
