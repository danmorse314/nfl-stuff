# install packages
install.packages(c("ffscrapr","remotes","dplyr"))
remotes::install_github("ffverse/ffsimulator")

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
  mutate(user_franchise = glue::glue("{user_name} ({franchise_name})"))

# get transactions

# load prior transaction logs
#   this will be helpful in january once the year changes so we can combine prior years with new years all in one dataset
trans.old <- readRDS(url("https://github.com/danmorse314/nfl-stuff/raw/main/ff/transaction_log.rds"))

trans <- ffscrapr::ff_transactions(sl_conn) |>
  filter(timestamp > max(trans.old$timestamp)) |>
  left_join(
    user_names |>
      select(franchise_id, user_name),
    by = "franchise_id"
  )

if(nrow(trans) > 0){
  # new transactions found
  trans.clean <- trans |>
    mutate(pick_id = ifelse(nchar(player_id) > 6, player_id, NA_character_)) |>
    tidyr::separate(
      pick_id, into = c("pick_year","axe1","pick_round","axe2","axe3","axe4","pick_franchise"),
      sep = "_"
    ) |>
    mutate(
      player_name = case_when(
        !is.na(player_name) ~ player_name,
        pick_round == 1 ~ glue::glue("1st Round ({pick_year})"),
        pick_round == 2 ~ glue::glue("2nd Round ({pick_year})"),
        pick_round == 3 ~ glue::glue("3rd Round ({pick_year})"),
        pick_round == 4 ~ glue::glue("4th Round ({pick_year})"),
      )
    ) |>
    left_join(
      user_names |>
        select(pick_franchise = franchise_id, pick_user = user_name),
      by = "pick_franchise"
    ) |>
    mutate(
      pick_name = ifelse(!is.na(pick_user), glue::glue("{player_name} ({pick_user})"), NA_character_)
    ) |>
    select(timestamp:franchise_name, user_name, sleeper_id = player_id, player_name, pick_name, pos:comment) |>
    # add back to old logs
    bind_rows(trans.old) |>
    arrange(desc(timestamp))
} else {
  trans.clean <- trans.old
}

user_names <- user_names |>
  select(franchise_id, user_name)

# get current rosters
rosters <- ffscrapr::ff_rosters(sl_conn) |>
  left_join(user_names, by = "franchise_id") |>
  mutate(user_franchise = glue::glue("{user_name} ({franchise_name})"))

# get standings
current <- ffscrapr::ff_standings(sl_conn)

# check if there's been a tie
is_ties <- sum(current$h2h_ties) > 0

# run season simulations
sl_sim <- ffsimulator::ff_simulate(
  conn = sl_conn,
  n_seasons = 1000,
  actual_schedule = TRUE,
  pos_filter = c("QB","RB","WR","TE"),
  seed = 48
)

# check if we need to simulate or if the season is over
if(!is.null(sl_sim$summary_season)){
  
  # save sim
  sl_sim |> saveRDS(paste0("ff/season_simulation_",year,".rds"))
  
  # season projections
  if(!is.null(current)){
    
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
    ungroup() |>
    mutate(dtupdated = Sys.time())
  
  # weekly sims
  proj.week <- sl_sim$summary_week |>
    left_join(user_names, by = "franchise_id") |>
    mutate(
      user_franchise = glue::glue("{user_name} ({franchise_name})"),
      point_diff = team_score - opponent_score,
      sims = max(season)
    ) |>
    group_by(week, user_franchise) |>
    mutate(
      mean_pf = round(mean(team_score)),
      mean_pa = round(mean(opponent_score)),
      mean_pd = round(mean(point_diff)),
      wp_raw = sum(result == 'W') / sims,
      opp_wp_raw = 1 - wp_raw,
      wp = glue::glue("{round(100*wp_raw)}%"),
      opp_wp = glue::glue("{round(100*opp_wp_raw)}%")
    ) |>
    ungroup() |>
    mutate(dtupdated = Sys.time())
  
  proj.week |> saveRDS(paste0("ff/season_simulation_weekly_",year,".rds"))
  
} else {
  # season is over, use final standings
  
  proj.year <- current |>
    arrange(-h2h_wins, -points_for) |>
    mutate(
      place.c = row_number(),
      proj.place = place.c,
      clinched = case_when(
        place.c <= 2 ~ "y",
        between(place.c, 3, 6) ~ "x",
        TRUE ~ "e"
      ),
      current_record = glue::glue("{h2h_wins}-{h2h_losses}"),
      mean_wins = h2h_wins,
      playoff_odds = ifelse(place.c <= 6, 1, 0),
      bye_odds = ifelse(place.c <= 2, 1, 0),
      mean_pf = points_for,
      mean_pa = points_against,
      mean_pd = points_for - points_against,
      mean_potential = potential_points,
      efficiency = points_for / potential_points
    ) |>
    left_join(
      user_names |>
        mutate(franchise_id = as.integer(franchise_id)),
      by = "franchise_id"
    ) |>
    select(
      place.c, proj.place, clinched, user_name, franchise_name,
      current_record, mean_wins, playoff_odds, bye_odds,
      mean_pf, mean_pa, mean_pd, mean_potential, efficiency
    )
}

# save
proj.year |> saveRDS(paste0("ff/season_simulation_yearly_",year,".rds"))
user_names |> saveRDS(paste0("ff/franchises_",year,".rds"))
rosters |> saveRDS(paste0("ff/rosters_",year,".rds"))
current |> saveRDS(paste0("ff/standings_",year,".rds"))
trans.clean |> saveRDS("ff/transaction_log.rds")
