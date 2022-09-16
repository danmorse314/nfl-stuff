source("madden/get_madden_ratings.R")

ratings <- get_madden_ratings(update = TRUE)

ratings |>
  readr::write_csv("madden/madden_23_player_ratings.csv")
