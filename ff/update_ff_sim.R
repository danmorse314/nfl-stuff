# install packages
install.packages(c("ffscrapr","remotes","dplyr"))
remotes::install_github("ffverse/ffsimulator")

# run dynasty league simulations
library(dplyr)

# get current year
year <- as.numeric(substr(Sys.Date(),1,4))

# sims to run
nsims <- 1000

# are schedules released for the sim year?
actual_schedule <- T

# league ID
#lid <- "999807305069699072"  # 2023
#lid <- "1073536596231753728" # 2024
lid <- "1205299893619404800" # 2025

# keep trade cut values
#source("update_ktc_values.R")

# connect to sleeper
sl_conn <- ffscrapr::sleeper_connect(
  season = year, league_id = lid
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
    select(timestamp:franchise_name, user_name, sleeper_id = player_id, player_name, pick_name, pos, everything()) |>
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

# get starters
starters <- ffscrapr::sleeper_getendpoint(glue::glue("league/{sl_conn$league_id}/rosters")) %>%
  purrr::pluck("content") %>%
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  dplyr::select(player_id = starters) |>
  tidyr::unnest_longer(player_id) |>
  dplyr::mutate(starter = 1)

# get taxi squad
taxi_squad <- ffscrapr::sleeper_getendpoint(glue::glue("league/{sl_conn$league_id}/rosters")) %>%
  purrr::pluck("content") %>%
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  dplyr::select(player_id = taxi) |>
  tidyr::unnest_longer(player_id) |>
  dplyr::mutate(taxi = 1)

# get IR players
injured_reserve <- ffscrapr::sleeper_getendpoint(glue::glue("league/{sl_conn$league_id}/rosters")) %>%
  purrr::pluck("content") %>%
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  dplyr::select(player_id = reserve) |>
  tidyr::unnest_longer(player_id) |>
  dplyr::mutate(ir = 1)

rosters <- rosters |>
  dplyr::left_join(taxi_squad, by = "player_id") |>
  dplyr::left_join(injured_reserve, by = "player_id") |>
  dplyr::left_join(starters, by = "player_id") |>
  dplyr::mutate(taxi = ifelse(is.na(taxi), 0, taxi),
                ir   = ifelse(is.na(ir),   0, ir),
                starter = ifelse(is.na(starter), 0, starter),
                roster_status = dplyr::case_when(
                  taxi == 1 ~ "Taxi Squad",
                  ir == 1 ~ "Injured Reserve",
                  starter == 1 ~ "Starting Lineup",
                  TRUE ~ "Bench"
                ))

# get standings
ff_st_safe <- purrr::safely(~ffscrapr::ff_standings(sl_conn))
current <- ff_st_safe()

current <- current$result

# check if there's been a tie
is_ties <- sum(current$h2h_ties) > 0

# run season simulations
classic_sim <- FALSE
if(classic_sim){
  sl_sim <- ffsimulator::ff_simulate(
    conn = sl_conn,
    n_seasons = nsims,
    #actual_schedule = TRUE,
    pos_filter = c("QB","RB","WR","TE"),
    seed = 48
  )
} else {
  library(ffsimulator)
  
  seed <- 48
  
  set.seed(seed)
  
  year.is <- ifelse(
    between(as.numeric(substr(Sys.Date(), 6,7)), 3, 9),
    year - 1,
    year
  )
  
  franchises <- ffsimulator::ffs_franchises(sl_conn) |>
    mutate(user_franchise = glue::glue("{user_name} ({franchise_name})"))
  
  scoring_history <- ffscrapr::ff_scoringhistory(sl_conn, 2012:year.is)
  
  latest_rankings <- ffsimulator::ffs_latest_rankings(type = "draft") |>
    filter(pos %in% c("QB","RB","WR","TE"))
  
  lineup_constraints <- ffsimulator::ffs_starter_positions(sl_conn)
  
  adp_outcomes <- ffsimulator::ffs_adp_outcomes(
    scoring_history = scoring_history,
    gp_model = "simple", # or "none"
    pos_filter = c("QB","RB","WR","TE")
  )
  
  fp_ids <- readRDS(url("https://github.com/dynastyprocess/data/raw/master/files/db_playerids.rds")) |>
    filter(position %in% c("QB","RB","TE","WR"))
  
  rosters_rl <- rosters |>
    mutate(sleeper_id = player_id) |>
    inner_join(
      fp_ids |>
        select(sleeper_id, fantasypros_id),
      by = "sleeper_id"
    ) |>
    mutate(league_id = lid) |>
    ffs_add_replacement_level(
      latest_rankings = latest_rankings,
      franchises = franchises,
      lineup_constraints = lineup_constraints,
      pos_filter = c("QB","RB","WR","TE")
    )
  
  projected_scores <- ffs_generate_projections(
    adp_outcomes = adp_outcomes,
    latest_rankings = latest_rankings,
    n_seasons = 1000, # number of seasons
    weeks = 1:14, # specifies which weeks to generate projections for
    rosters = rosters_rl # optional, reduces the sample to just rostered players
  )
  
  roster_scores <- ffs_score_rosters(
    projected_scores = projected_scores,
    rosters = rosters_rl
  )
  
  optimal_scores <- ffs_optimise_lineups(
    roster_scores = roster_scores,
    lineup_constraints = lineup_constraints,
    lineup_efficiency_mean = 0.775,
    lineup_efficiency_sd = 0.05,
    best_ball = FALSE,
    pos_filter = c("QB","RB","WR","TE")
  )
  
  if(actual_schedule) {
    schedules <- ffs_schedule(sl_conn)
    
    if(nrow(schedules) > 0){
      schedules <- ffs_repeat_schedules(n_seasons = 1000,
                                        actual_schedule = schedules)
    } else {
      message("\nregular season over")
    }
    
  } else {
    schedules <- ffs_build_schedules(
      n_seasons = 1000,
      n_weeks = 14,
      seed = seed,
      franchises = franchises
    )
  }
  
  if(nrow(schedules) > 0){
    summary_week <- ffs_summarise_week(optimal_scores, schedules)
    summary_season <- ffs_summarise_season(summary_week)
    summary_simulation <- ffs_summarise_simulation(summary_season)
  } else {
    summary_season <- current |>
      mutate(
        season = 1,
        league_id = lid,
        allplay_games = allplay_wins + allplay_losses,
        franchise_id = as.character(franchise_id),
        across(
          c(h2h_wins, h2h_winpct, points_for, points_against, potential_points,
            allplay_wins, allplay_games, allplay_winpct),
          ~0
        )
      ) |>
      select(
        season,
        league_id,
        franchise_id,
        franchise_name,
        h2h_wins,
        h2h_winpct,
        allplay_wins,
        allplay_games,
        allplay_winpct,
        points_for,
        points_against,
        potential_points
      )
  }
  
  sl_sim <- list(
    summary_week = summary_week,
    summary_season = summary_season,
    summary_simulation = summary_simulation
  )
}


# check if we need to simulate or if the season is over
if(!is.null(sl_sim$summary_season)){
  
  # save sims
  if(!is.null(sl_sim$league_info)){
    sl_sim_save <- list(
      summary_simulation = sl_sim$summary_simulation,
      league_info = sl_sim$league_info,
      simulation_params = sl_sim$simulation_params
    )
  } else {
    sl_sim_save <- list(summary_simulation = sl_sim$summary_simulation)
  }
  
  sl_sim_save |> saveRDS(paste0("ff/season_simulation_",year,".rds"))
  
  # season projections
  if(!is.null(current)){
    
    proj.year.raw <- sl_sim$summary_season |>
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
            place.c = row_number(),
            across(c(points_for,points_against,potential_points),
                   ~ifelse(is.na(.x), 0, .x))
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
    
    proj.year.raw <- sl_sim$summary_season |>
      mutate(
        current_record = "0-0",
        wins.c = 0,
        pf.c = 0,
        pa.c = 0,
        pot.c = 0
      )
    
  }
  
  proj.year.raw <- proj.year.raw |>
    left_join(user_names, by = "franchise_id")  |>
    mutate(
      h2h_wins = h2h_wins + wins.c,
      points_for = points_for + pf.c,
      points_against = points_against + pa.c,
      potential_points = potential_points + pot.c,
      user_franchise = glue::glue("{user_name} ({franchise_name})"),
      point_diff = points_for - points_against
    )
  
  # old code that I don't wanna delete
  proj.year.old <- proj.year.raw |>
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
  
  # new season sims
  proj.year <- proj.year.raw |>
    group_by(season) |>
    arrange(-h2h_wins, -points_for) |>
    mutate(place = row_number(),
           playoffs = ifelse(place <= 6, 1, 0),
           bye = ifelse(place <= 2, 1, 0)) |>
    ungroup() |>
    group_by(user_name, franchise_name, current_record, pot.c, wins.c, pf.c) |>
    summarise(
      mean_pf = round(mean(points_for)),
      mean_pa = round(mean(points_against)),
      mean_pd = round(mean(point_diff)),
      mean_wins = round(mean(h2h_wins),1),
      mean_losses = 14 - mean_wins,
      median_wins = median(h2h_wins),
      median_losses = 14-median_wins,
      proj.record = glue::glue("{sprintf('%.1f', mean_wins)} - {sprintf('%.1f', mean_losses)}"),
      mean_potential = round(mean(potential_points)),
      playoff_odds = mean(playoffs),
      bye_odds = mean(bye),
      pick_1_odds = mean(place == 12),
      pick_2_odds = mean(place == 11),
      pick_3_odds = mean(place == 10),
      pick_4_odds = mean(place == 9),
      pick_5_odds = mean(place == 8),
      pick_6_odds = mean(place == 7),
      pick_7_odds = mean(place == 6),
      pick_8_odds = mean(place == 5),
      pick_9_odds = mean(place == 4),
      pick_10_odds = mean(place == 3),
      pick_11_odds = mean(place == 2),
      pick_12_odds = mean(place == 1),
      sims = max(season),
      .groups = "drop"
    ) |>
    arrange(-wins.c, -pf.c) |>
    mutate(place.c = row_number()) |>
    select(
      place.c, user_name, franchise_name, current_record, proj.record,
      mean_wins, median_wins, playoff_odds, bye_odds, mean_pf, mean_pa,
      mean_pd, mean_potential, pot.c,
      starts_with("pick_"),
      # for clinching
      wins.c, pf.c, place.c, sims
    ) |>
    mutate(dtupdated = Sys.time())
  
  # weekly sims
  proj.week <- sl_sim$summary_week |>
    left_join(user_names, by = "franchise_id") |>
    mutate(
      user_franchise = glue::glue("{user_name} ({franchise_name})"),
      point_diff = team_score - opponent_score,
      sims = max(season)
    ) |>
    group_by(week, user_name, franchise_name, user_franchise, opponent_name) |>
    summarise(
      mean_pf = round(mean(team_score),2),
      mean_pa = round(mean(opponent_score),2),
      mean_pd = round(mean(point_diff),2),
      wp_raw = sum(result == 'W') / max(sims),
      opp_wp_raw = 1 - wp_raw,
      wp = glue::glue("{round(100*wp_raw)}%"),
      opp_wp = glue::glue("{round(100*opp_wp_raw)}%"),
      .groups = "drop"
    ) |>
    ungroup() |>
    mutate(dtupdated = Sys.time(), season = year)
  
  if(nrow(proj.week)>0){
    proj.week |> saveRDS(paste0("ff/season_simulation_weekly_",year,".rds"))
  }
  
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

# save projections as longitudinal data
if(file.exists(paste0("ff/season_simulation_yearly_",year,"_long.rds"))){
  proj.year.total <- readRDS(paste0("ff/season_simulation_yearly_",year,"_long.rds")) |>
  bind_rows(proj.year)
} else {
  proj.year.total <- proj.year
}

# save
proj.year |> saveRDS(paste0("ff/season_simulation_yearly_",year,".rds"))
user_names |> saveRDS(paste0("ff/franchises_",year,".rds"))
rosters |> saveRDS(paste0("ff/rosters_",year,".rds"))
current |> saveRDS(paste0("ff/standings_",year,".rds"))
trans.clean |> saveRDS("ff/transaction_log.rds")
proj.year.total |> saveRDS(paste0("ff/season_simulation_yearly_",year,"_long.rds"))
