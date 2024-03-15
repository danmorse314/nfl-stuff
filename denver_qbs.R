# best broncos QBs since Peyton

library(tidyverse)

pbp.all <- nflreadr::load_pbp(seasons = 2014:2023)

rosters <- nflreadr::load_rosters(seasons = 2014:2023)

qb.stats <- pbp.all |>
  mutate(
    player_id = coalesce(passer_player_id, rusher_player_id)
  ) |>
  filter(
    (pass == 1 | rush == 1) &
      !is.na(epa) &
      !is.na(player_id) &
      !is.na(posteam)
  ) |>
  filter(season >= 2015) |>
  group_by(player_id, team = posteam) |>
  summarise(
    seasons = ifelse(
      min(season) == max(season),
      as.character(min(season)),
      paste(min(season), max(season), sep = "-")
    ),
    gp = length(unique(game_id)),
    plays = n(),
    db = sum(pass),
    dc = sum(rush),
    ypa = mean(yards_gained[pass_attempt == 1], na.rm = TRUE),
    comp_perc = mean(complete_pass, na.rm = TRUE),
    mean_epa = mean(epa),
    cpoe = mean(cpoe, na.rm = TRUE),
    epa_pass = mean(epa[pass == 1]),
    epa_rush = mean(epa[rush == 1]),
    .groups = "drop"
  ) |>
  left_join(
    rosters |>
      distinct(player_id = gsis_id, position, name = full_name),
    by = "player_id"
  ) |>
  filter(
    (team == "DEN" & db >= 10) |
      (position == "QB" & db >= 100)
  ) |>
  arrange(-mean_epa) |>
  select(player_id, name, position, team, everything()) |>
  mutate(epa_rush = ifelse(is.nan(epa_rush), 0, epa_rush))

qb.den <- filter(qb.stats, team == "DEN")

qb.stats <- filter(qb.stats, db >= 200)

glimpse(qb.den)

library(gt)

qb.den |>
  gt() |>
  cols_label(
    position = "Pos",
    db = "Dropbacks",
    dc = "Carries",
    ypa = "Y/A",
    comp_perc = "Comp%",
    mean_epa = "EPA/play",
    epa_pass = "EPA/DB",
    epa_rush = "EPA/Carry"
  ) |>
  fmt_number(
    columns = contains("epa"),
    decimals = 2
  ) |>
  fmt_percent(
    columns = comp_perc,
    decimals = 1
  ) |>
  fmt_number(
    columns = c(cpoe, ypa),
    decimals = 1
  ) |>
  cols_hide(
    columns = c(player_id, team)
  ) |>
  tab_spanner(
    label = "PLAYER",
    columns = name:seasons
  ) |>
  tab_spanner(
    label = "SNAP COUNTS",
    columns = gp:dc
  ) |>
  tab_spanner(
    label = "STATS",
    columns = ypa:epa_rush
  ) |>
  cols_align(
    columns = seasons,
    align = "center"
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = ypa,
    domain = c(min(qb.stats$ypa), max(qb.stats$ypa))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = comp_perc,
    domain = c(min(qb.stats$comp_perc), max(qb.stats$comp_perc))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = mean_epa,
    domain = c(min(qb.stats$mean_epa), max(qb.stats$mean_epa))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = cpoe,
    domain = c(min(qb.stats$cpoe), max(qb.stats$cpoe))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = epa_pass,
    domain = c(min(qb.stats$epa_pass), max(qb.stats$epa_pass))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = epa_rush,
    domain = c(min(qb.stats$epa_rush), max(qb.stats$epa_rush))
  ) |>
  tab_footnote(
    locations = cells_body(
      rows = name == "Peyton Manning",
      columns = name
    ),
    footnote = "won Super Bowl"
  ) |>
  tab_header(
    title = "Denver Broncos QBs, 2015-present",
    subtitle = "Reg. + Postseason"
  ) |>
  tab_source_note(
    "data: nflfastR"
  ) |>
  gtExtras::gt_theme_espn()

# by season
qb.den.season <- pbp.all |>
  mutate(
    player_id = coalesce(passer_player_id, rusher_player_id)
  ) |>
  filter(
    (pass == 1 | rush == 1) &
      !is.na(epa) &
      !is.na(player_id) &
      !is.na(posteam)
  ) |>
  filter(season >= 2015) |>
  group_by(player_id, team = posteam, season) |>
  summarise(
    gp = length(unique(game_id)),
    plays = n(),
    db = sum(pass),
    dc = sum(rush),
    ypa = mean(yards_gained[pass_attempt == 1], na.rm = TRUE),
    comp_perc = mean(complete_pass, na.rm = TRUE),
    mean_epa = mean(epa),
    cpoe = mean(cpoe, na.rm = TRUE),
    epa_pass = mean(epa[pass == 1]),
    epa_rush = mean(epa[rush == 1]),
    .groups = "drop"
  ) |>
  left_join(
    rosters |>
      distinct(player_id = gsis_id, position, name = full_name),
    by = "player_id"
  ) |>
  filter(
    (team == "DEN" & db >= 10) |
      (position == "QB" & db >= 100)
  ) |>
  arrange(-mean_epa) |>
  select(player_id, name, position, team, everything()) |>
  mutate(epa_rush = ifelse(is.nan(epa_rush), 0, epa_rush)) |>
  filter(team == "DEN")

qb.den.season |>
  filter(gp >= 5) |>
  gt() |>
  cols_label(
    position = "Pos",
    db = "Dropbacks",
    dc = "Carries",
    ypa = "Y/A",
    comp_perc = "Comp%",
    mean_epa = "EPA/play",
    epa_pass = "EPA/DB",
    epa_rush = "EPA/Carry"
  ) |>
  fmt_number(
    columns = contains("epa"),
    decimals = 2
  ) |>
  fmt_percent(
    columns = comp_perc,
    decimals = 1
  ) |>
  fmt_number(
    columns = c(cpoe, ypa),
    decimals = 1
  ) |>
  cols_hide(
    columns = c(player_id, team)
  ) |>
  tab_spanner(
    label = "PLAYER",
    columns = name:season
  ) |>
  tab_spanner(
    label = "SNAP COUNTS",
    columns = gp:dc
  ) |>
  tab_spanner(
    label = "STATS",
    columns = ypa:epa_rush
  ) |>
  cols_align(
    columns = season,
    align = "center"
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = ypa,
    domain = c(min(qb.stats$ypa), max(qb.stats$ypa))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = comp_perc,
    domain = c(min(qb.stats$comp_perc), max(qb.stats$comp_perc))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = mean_epa,
    domain = c(min(qb.stats$mean_epa), max(qb.stats$mean_epa))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = cpoe,
    domain = c(min(qb.stats$cpoe), max(qb.stats$cpoe))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = epa_pass,
    domain = c(min(qb.stats$epa_pass), max(qb.stats$epa_pass))
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = epa_rush,
    domain = c(min(qb.stats$epa_rush), max(qb.stats$epa_rush))
  ) |>
  tab_footnote(
    locations = cells_body(
      rows = season == 2015,
      columns = name
    ),
    footnote = "won Super Bowl"
  ) |>
  tab_header(
    title = "Denver Broncos QBs, 2015-present",
    subtitle = "Reg. + Postseason, min. 5 GP"
  ) |>
  tab_source_note(
    "data: nflfastR"
  ) |>
  gtExtras::gt_theme_espn()

# by season, all qbs

team.season <- pbp.all |>
  mutate(
    player_id = coalesce(passer_player_id, rusher_player_id)
  ) |>
  filter(
    (pass == 1 | rush == 1) &
      !is.na(epa) &
      !is.na(player_id) &
      !is.na(posteam)
  ) |>
  filter(season >= 2015) |>
  left_join(
    rosters |>
      distinct(player_id = gsis_id, position, last_name, season),
    by = c("player_id", "season")
  ) |>
  filter(position == "QB") |>
  group_by(team = posteam, season) |>
  summarise(
    qbs = paste(unique(last_name), collapse = ", "),
    gp = length(unique(game_id)),
    plays = n(),
    db = sum(pass),
    dc = sum(rush),
    ypa = mean(yards_gained[pass_attempt == 1], na.rm = TRUE),
    comp_perc = mean(complete_pass, na.rm = TRUE),
    mean_epa = mean(epa),
    cpoe = mean(cpoe, na.rm = TRUE),
    epa_pass = mean(epa[pass == 1]),
    epa_rush = mean(epa[rush == 1]),
    .groups = "drop"
  ) |>
  arrange(-mean_epa) |>
  select(team, qbs, everything()) |>
  mutate(epa_rush = ifelse(is.nan(epa_rush), 0, epa_rush)) |>
  filter(!is.na(team)) |>
  group_by(season) |>
  mutate(across(ypa:epa_rush, ~rank(desc(.x), ties.method = "min"))) |>
  ungroup()

den.season <- filter(team.season, team == "DEN")

den.season |>
  gt() |>
  cols_label(
    qbs = "QUARTERBACKS",
    db = "Dropbacks",
    dc = "Carries",
    ypa = "Y/A",
    comp_perc = "Comp%",
    mean_epa = "EPA/play",
    epa_pass = "EPA/DB",
    epa_rush = "EPA/Carry"
  ) |>
  cols_hide(
    columns = team
  ) |>
  tab_spanner(
    label = "PLAYER",
    columns = qbs:season
  ) |>
  tab_spanner(
    label = "SNAP COUNTS",
    columns = gp:dc
  ) |>
  tab_spanner(
    label = "SEASON RANKS",
    columns = ypa:epa_rush
  ) |>
  cols_align(
    columns = season,
    align = "center"
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = ypa:epa_rush,
    domain = c(32,1),
    reverse = TRUE
  ) |>
  tab_footnote(
    locations = cells_body(
      rows = season == 2015,
      columns = season
    ),
    footnote = "won Super Bowl"
  ) |>
  tab_header(
    title = "Denver Broncos QBs, 2015-present",
    subtitle = "Reg. + Postseason"
  ) |>
  tab_source_note(
    "data: nflfastR"
  ) |>
  gtExtras::gt_theme_espn()
