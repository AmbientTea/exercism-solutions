largest_series_product <- function(digits, span){
  stopifnot(nchar(digits) >= span)

  digits <- as.numeric(strsplit(digits, "")[[ 1 ]])

  stopifnot(!any(is.na(digits)))

  sum <- digits
  for(i in seq_len(span-1)) {
    print(sum)
    sum = head(sum, -1) * tail(digits, -i)
  }

  max(sum)
}
