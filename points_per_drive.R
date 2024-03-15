library(tidyverse)

pbp_all <- nflreadr::load_pbp()
part <- nflreadr::load_participation()

pbp <- pbp_all |>
  filter(pass == 1 | rush == 1) |>
  filter(!is.na(posteam)) |>
  left_join(
    part,
    by = c("game_id" = "nflverse_game_id",
           "play_id", "old_game_id",
           "posteam" = "possession_team")
  )

rosters <- nflreadr::load_rosters()

qbs <- filter(rosters, position == "QB") |>
  select(player_id = gsis_id, qb = full_name)

pbp.clean <- pbp |>
  tidyr::separate_longer_delim(
    cols = offense_players,
    delim = ";"
  ) |>
  rename(player_id = offense_players) |>
  left_join(qbs, by = "player_id") |>
  left_join(
    qbs |>
      rename(qb_alt = qb),
    by = c("passer_player_id" = "player_id")
  ) |>
  mutate(
    qb = case_when(
      posteam == "SEA" & week == 15 ~ "Drew Lock",
      posteam == "SEA" & week == 16 ~ "Geno Smith",
      is.na(qb) & !is.na(qb_alt) ~ qb_alt,
      TRUE ~ qb
    )
  ) |>
  filter(!is.na(qb)) |>
  mutate(
    split = ifelse(week <= 9, "1-9", "10-16"),
    drive_id = paste(week, posteam, defteam, drive, sep = "_")
  )

tbl <- pbp.clean |>
  group_by(split, drive_id) |>
  mutate(
    drive_start_qb = na.omit(first(qb)),
    drive_end_qb = na.omit(last(qb)),
    drive_qb = ifelse(drive_start_qb == drive_end_qb, drive_end_qb, paste(drive_start_qb, drive_end_qb, sep = ";"))
  ) |>
  slice(1) |>
  ungroup() |>
  mutate(
    points = case_when(
      fixed_drive_result == "Field goal" ~ 3,
      fixed_drive_result == "Touchdown" ~ 6,
      fixed_drive_result == "Opp touchdown" ~ -6,
      TRUE ~ 0
    )
  ) |>
  select(week, split, posteam, drive_id, drive_qb, points)

ppd.stats <- tbl |>
  filter(str_detect(drive_qb, ";", negate = TRUE)) |>
  group_by(split, posteam, drive_qb) |>
  summarise(
    drives = n(),
    ppd = mean(points),
    .groups = "drop"
  ) |>
  filter(drives >= 20) |>
  group_by(split) |>
  mutate(
    rank = rank(desc(ppd), ties.method = "min"),
    rank.display = paste(rank, max(rank), sep = "/")
  ) |>
  ungroup() |>
  arrange(split, rank)

filter(ppd.stats, posteam == "SEA")

ppd.stats |> write_csv("data/ppd_stats_qb_2023.csv")

drew <- pbp.clean |>
  filter(qb == "Drew Lock") |>
  distinct(drive_id)

drew |> print(n=27)
