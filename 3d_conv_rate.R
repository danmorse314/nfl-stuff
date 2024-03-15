library(tidyverse)

part <- nflreadr::load_participation()

rosters <- nflreadr::load_rosters()

glimpse(part)
glimpse(rosters)

geno <- filter(rosters, full_name == "Geno Smith") |>
  pull(gsis_id)
drew <- filter(rosters, full_name == "Drew Lock") |>
  pull(gsis_id)

filter(part, possession_team == "SEA") |>
  mutate(week = as.numeric(substr(nflverse_game_id, 6,7))) |>
  count(week)

pbp <- filter(pbp.reg, season == 2023) |>
  left_join(
    part |>
      select(offense_players, play_id, nflverse_game_id),
    by = c(
      "game_id" = "nflverse_game_id",
      "play_id"
    )
  )

plays.geno <- pbp |>
  mutate(
    qb = case_when(
      str_detect(offense_players, geno) ~ "Geno",
      str_detect(offense_players, drew) ~ "Drew",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(qb))

plays.geno |>
  group_by(qb) |>
  summarise(
    gp = length(unique(game_id)),
    plays = n(),
    mean_epa = mean(epa, na.rm = TRUE),
    epa_pass = mean(epa[pass==1], na.rm = TRUE),
    d3_conv_rate = sum(first_down == 1 & down == 3, na.rm = TRUE) / sum(down == 3, na.rm = TRUE),
    .groups = "drop"
  )
