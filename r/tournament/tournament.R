library(dplyr)

tournament <- function(input) {
  input <- input[grep("^[^;]+;[^;]+;[^;]+$",input)]
  results <- read.table(
    text = input, sep = ";",
    header = FALSE, col.names = c("Team1", "Team2", "Result")
  ) %>%
    filter(Result %in% c("win", "draw", "loss"))

  team1outcomes <- results %>%
    transmute(
      Team = Team1,
      W = Result == "win",
      D = Result == "draw",
      L = Result == "loss",
      P = c(3, 1, 0)[match(Result, c("win", "draw", "loss"))]
    )
  team2outcomes <- results %>%
    transmute(
      Team = Team2,
      W = Result == "loss",
      D = Result == "draw",
      L = Result == "win",
      P = c(0, 1, 3)[match(Result, c("win", "draw", "loss"))]
    )

  rbind(team1outcomes, team2outcomes) %>%
    group_by(Team) %>%
    summarize(
      MP = n(),
      W = sum(W),
      D = sum(D),
      L = sum(L),
      P = sum(P)
    ) %>%
    arrange(-P) %>%
    as.data.frame()
}
