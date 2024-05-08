library(tidyverse)
library(gt)

draft <- nflreadr::load_draft_picks()

draft |>
  arrange(-season) |>
  glimpse()

chart <- draft |>
  left_join(
    draft |>
      group_by(season, round) |>
      mutate(last_round_picks = max(pick), round = round + 1) |>
      ungroup() |>
      distinct(season, round, last_round_picks),
    by = c("round","season")
  ) |>
  mutate(
    last_round_picks = ifelse(is.na(last_round_picks), 0, last_round_picks),
    round.pick = paste0(round, ".", formatC(pick - last_round_picks, width = 2, flag = "0")),
    player_pick = paste(pick, pfr_player_name, sep = ". ")
  ) |>
  filter(pick <= 50 & position == "QB") |>
  group_by(season) |>
  summarise(
    qbs = n(),
    selections = paste0("<p>",paste(unique(player_pick), collapse = "<br>"),"</p>"),
    `career gp` = paste0("<p>",paste(games, collapse = "<br>"),"</p>"),
    .groups = "drop"
  ) |>
  arrange(-season)

tbl1 <- chart |>
  filter(qbs >= 5) |>
  slice(1:3) |>
  gt() |>
  fmt_markdown() |>
  gtExtras::gt_theme_538() |>
  cols_align(columns = 1:2, align = "center") |>
  tab_header("Every NFL Draft with 5+ Top-50 QBs") |>
  tab_source_note("nflreadr | Pro Football Reference")

tbl2 <- chart |>
  filter(qbs >= 5) |>
  slice(4:6) |>
  gt() |>
  fmt_markdown() |>
  gtExtras::gt_theme_538() |>
  cols_align(columns = 1:2, align = "center") |>
  tab_header(".")

gtExtras::gt_two_column_layout(tables = list(tbl1, tbl2))

chart <- draft |>
  left_join(
    draft |>
      group_by(season, round) |>
      mutate(last_round_picks = max(pick), round = round + 1) |>
      ungroup() |>
      distinct(season, round, last_round_picks),
    by = c("round","season")
  ) |>
  mutate(
    last_round_picks = ifelse(is.na(last_round_picks), 0, last_round_picks),
    round.pick = paste0(round, ".", formatC(pick - last_round_picks, width = 2, flag = "0")),
    player_pick = paste(pick, pfr_player_name, sep = ". ")
  ) |>
  filter(round == 1 & position == "QB") |>
  group_by(season) |>
  summarise(
    qbs = n(),
    selections = paste0("<p>",paste(unique(player_pick), collapse = "<br>"),"</p>"),
    `career gp` = paste0("<p>",paste(games, collapse = "<br>"),"</p>"),
    .groups = "drop"
  ) |>
  arrange(-season)

chart |>
  filter(qbs >= 5) |>
  gt() |>
  fmt_markdown() |>
  gtExtras::gt_theme_538() |>
  cols_align(columns = 1:2, align = "center") |>
  tab_header("Every NFL Draft with 5+ 1st Rd. QBs") |>
  tab_source_note(".")
