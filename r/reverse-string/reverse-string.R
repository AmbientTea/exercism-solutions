reverse <- function(text) {
    paste(rev(unlist(strsplit(text,""))), collapse="")
}
