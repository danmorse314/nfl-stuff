# run dynasty league simulations
library(dplyr)

# get current year
year <- as.numeric(substr(Sys.Date(),1,4))

# connect to sleeper
sl_conn <- ffscrapr::sleeper_connect(
  season = year, league_id = "999807305069699072"
)

# get user names and team names
user_names <- ffsimulator::ffs_franchises(sl_conn) |>
  select(franchise_id, user_name)

# get current rosters
rosters <- ffscrapr::ff_rosters(sl_conn) |>
  left_join(user_names, by = "franchise_id") |>
  mutate(user_franchise = glue::glue("{user_name} ({franchise_name})"))

# get standings
current <- ffscrapr::ff_standings(sl_conn)

# run season simulations
sl_sim <- ffsimulator::ff_simulate(
  conn = sl_conn,
  n_seasons = 1000,
  actual_schedule = TRUE,
  pos_filter = c("QB","RB","WR","TE"),
  seed = 48
)

# season projections
if(!is.null(current)){
  
  is_ties <- sum(current$h2h_ties) > 0
  
  proj.year <- sl_sim$summary_season |>
    left_join(
      current |>
        group_by(franchise_name) |>
        mutate(
          franchise_id = as.character(franchise_id),
          current_record = ifelse(is_ties,glue::glue("{h2h_wins}-{h2h_losses}-{h2h_ties}"), glue::glue("{h2h_wins}-{h2h_losses}"))
        ) |>
        ungroup() |>
        arrange(-h2h_wins, -points_for) |>
        mutate(
          place.c = row_number()
        ) |>
        select(
          franchise_id,
          place.c,
          current_record,
          wins.c = h2h_wins,
          pf.c = points_for,
          pa.c = points_against,
          pot.c = potential_points
        ),
      by = "franchise_id"
    )
  
} else {
  
  proj.year <- league_proj$summary_season |>
    mutate(
      place.c = 1,
      current_record = "0-0",
      wins.c = 0,
      pf.c = 0,
      pa.c = 0,
      pot.c = 0
    )
  
}

proj.year <- proj.year |>
  left_join(user_names, by = "franchise_id")  |>
  mutate(
    h2h_wins = h2h_wins + wins.c,
    points_for = points_for + pf.c,
    points_against = points_against + pa.c,
    potential_points = potential_points + pot.c,
    user_franchise = glue::glue("{user_name} ({franchise_name})"),
    point_diff = points_for - points_against
  )

proj.year <- proj.year |>
  group_by(user_franchise) |>
  mutate(
    mean_pf = round(mean(points_for)),
    mean_pa = round(mean(points_against)),
    mean_pd = round(mean(point_diff)),
    mean_wins = round(mean(h2h_wins),1),
    mean_losses = 14 - mean_wins,
    median_wins = median(h2h_wins),
    median_losses = 14-median_wins,
    proj.record = glue::glue("{sprintf('%.1f', mean_wins)} - {sprintf('%.1f', mean_losses)}"),
    mean_potential = round(mean(potential_points)),
    sims = max(season)
  ) |>
  ungroup() |>
  group_by(season) |>
  arrange(-h2h_wins, -points_for) |>
  mutate(place = row_number(),
         playoffs = ifelse(place <= 6, 1, 0)) |>
  ungroup() |>
  group_by(user_franchise) |>
  mutate(playoff_odds = mean(playoffs)) |>
  ungroup()

# save
sl_sim |> saveRDS(paste0("ff/season_simulation_",year,".rds"))
user_names |> saveRDS(paste0("ff/franchises_",year,".rds"))
rosters |> saveRDS(paste0("ff/rosters_",year,".rds"))
current |> saveRDS(paste0("ff/standings_",year,".rds"))
