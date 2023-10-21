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
  n_seasons = 10000,
  actual_schedule = TRUE,
  pos_filter = c("QB","RB","WR","TE"),
  seed = 48
)

# save
sl_sim |> saveRDS(paste0("ff/season_simulation_",year,".rds"))
