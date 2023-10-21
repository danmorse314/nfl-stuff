# run dynasty league simulations

# get current year
year <- as.numeric(substr(Sys.Date(),1,4))

# connect to sleeper
sl_conn <- ffscrapr::sleeper_connect(
  season = year, league_id = "999807305069699072"
)

# run season simulations
sl_sim <- ffsimulator::ff_simulate(
  conn = sl_conn,
  n_seasons = 1000,
  actual_schedule = TRUE,
  pos_filter = c("QB","RB","WR","TE"),
  seed = 48
)

# get roster user names and team names
user_names <- ffsimulator::ffs_franchises(sl_conn)

# save
sl_sim |> saveRDS(paste0("ff/season_simulation_",year,".rds"))
user_names |> saveRDS(paste0("ff/franchises_",year,".rds"))
