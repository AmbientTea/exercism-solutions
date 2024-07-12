library(tibble)
setClass("robot",
         slots = c(coordinates = "numeric", direction = "character"))

new_robot <- function(coordinates, direction) {
  structure(
    list(
      coordinates = coordinates,
      direction = direction
    ),
    class = "robot"
  )
}

shift <- function(v, n) {
  c(tail(v, -n), head(v, n))
}

directions <- tibble(
  name = c("NORTH", "EAST", "SOUTH", "WEST"),
  dxy = I(list(c(0,1), c(1,0), c(0,-1), c(-1, 0))),
  right = shift(name, 1),
  left = shift(name, -1)
)

move <- function(a_robot, commands) {
  UseMethod("move")
}
# nolint start
move.robot <- function(a_robot, commands) {
  commands <- unlist(strsplit(commands, ""))

  for(command in commands) {
    direction <- directions[ directions$name == a_robot$direction , ]
    if(command == "R") {
      a_robot$direction <- direction$right
    } else if(command == "L") {
      a_robot$direction <- direction$left
    } else if(command == "A") {
      a_robot$coordinates <- a_robot$coordinates + unlist(direction$dxy)
    }
  }

  a_robot
}
# nolint end
