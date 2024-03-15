library(tidyverse)

pbp_all <- nflreadr::load_pbp(seasons = 1999:2023)

pbp.reg <- filter(pbp_all, season_type == "REG") |>
  filter(!is.na(down) & !is.na(epa))

# offense --------------------

team.3.and.out <- pbp.reg |>
  mutate(drive_id = paste(game_id, drive, sep = "_")) |>
  group_by(team = posteam, season, drive_id) |>
  summarise(
    plays = n(),
    spikes = sum(play_type == "qb_spike", na.rm = TRUE),
    kneels = sum(play_type == "qb_kneel", na.rm = TRUE),
    pass_rush_plays = sum(pass + rush, na.rm = TRUE),
    punts = sum(play_type == "punt", na.rm = TRUE),
    fg = sum(play_type == "field_goal", na.rm = TRUE),
    td = sum(pass_touchdown + rush_touchdown, na.rm = TRUE),
    fd = sum(first_down, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(spikes < 1 & kneels < 1) |>
  mutate(
    is_3_and_out = ifelse(
      (punts + fg == 1) & fd == 0,
      1,
      0
    )
  ) |>
  group_by(team, season) |>
  summarise(
    drives = n(),
    d_3_and_out = sum(is_3_and_out),
    d_fg = sum(fg),
    d_td = sum(td),
    .groups = "drop"
  ) |>
  mutate(
    across(starts_with("d_"), ~.x / drives, .names = "{.col}_rate")
  ) |>
  arrange(-season, d_3_and_out_rate)

team.3.and.out |> write_csv("data/team_3_and_outs_by_season.csv")

# defense ----------------------------

team.3.and.out.d <- pbp.reg |>
  mutate(drive_id = paste(game_id, drive, sep = "_")) |>
  group_by(team = defteam, season, drive_id) |>
  summarise(
    plays = n(),
    spikes = sum(play_type == "qb_spike", na.rm = TRUE),
    kneels = sum(play_type == "qb_kneel", na.rm = TRUE),
    pass_rush_plays = sum(pass + rush, na.rm = TRUE),
    punts = sum(play_type == "punt", na.rm = TRUE),
    fg = sum(play_type == "field_goal", na.rm = TRUE),
    td = sum(pass_touchdown + rush_touchdown, na.rm = TRUE),
    fd = sum(first_down, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(spikes < 1 & kneels < 1) |>
  mutate(
    is_3_and_out = ifelse(
      (punts + fg == 1) & fd == 0,
      1,
      0
    )
  ) |>
  group_by(team, season) |>
  summarise(
    drives = n(),
    d_3_and_out = sum(is_3_and_out),
    d_fg = sum(fg),
    d_td = sum(td),
    .groups = "drop"
  ) |>
  mutate(
    across(starts_with("d_"), ~.x / drives, .names = "{.col}_rate")
  ) |>
  arrange(-season, -d_3_and_out_rate)

team.3.and.out.d |> write_csv("data/team_defense_3_and_outs_by_season.csv")
