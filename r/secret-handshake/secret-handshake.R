signs <- c(
  "wink",
  "double blink",
  "close your eyes",
  "jump"
)
handshake <- function(n) {
  bits <- bitwAnd(2^(0:4), n) != 0
  actions <- signs[head(bits, -1)]
  if(bits[5]) {
    actions <- rev(actions)
  }

  switch(length(actions) != 0, actions)
}
