## egg_count <- function(display_value) {
##   if(display_value == 0) 0
##   else sum(as.logical(bitwAnd(display_value, 2^(0:floor(log2(display_value))))))
## }

egg_count <- function(display_value) {
  sum(as.numeric(intToBits(display_value)))
}
