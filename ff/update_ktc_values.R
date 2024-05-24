library(rvest)

add.name_id <- function(df){
  
  # player_name
  # display_name
  # name
  # full_name
  if("player_name" %in% names(df)){
    df$name_id <- df$player_name
  } else if("full_name" %in% names(df)){
    df$name_id <- df$full_name
  } else if("display_name" %in% names(df)){
    df$name_id <- df$display_name
  } else if("player" %in% names(df)){
    df$name_id <- df$player
  } else if("name" %in% names(df)){
    df$name_id <- df$name
  } else {
    stop("Argument `df` must contain column named one of 'player_name', 'full_name', 'display_name', 'player', or 'name'")
  }
  
  df.return <- df |>
    dplyr::mutate(
      name_id = tolower(stringr::str_remove_all(name_id, "[[:punct:] ]+"))
    )
  
  return(df.return)
}

draft_year <- 2024

ktc <- NULL
for(pg in 0:9){
  
  url <- paste0("https://keeptradecut.com/dynasty-rankings?page=",pg,"&filters=QB|WR|RB|TE|RDP&format=2")
  
  site <- read_html(url)
  
  player_names <- site |>
    html_element("#rankings-page-rankings") |>
    html_elements(".player-name") |>
    html_elements("a") |>
    html_text2()
  
  player_urls <- site |>
    html_element("#rankings-page-rankings") |>
    html_elements(".player-name") |>
    html_elements("a") |>
    html_attr("href")
  
  player_vals <- site |>
    html_element("#rankings-page-rankings") |>
    html_elements(".value") |>
    html_text2() |>
    as.numeric()
  
  ktc <- bind_rows(
    ktc,
    tibble(
      player = player_names,
      ktc_url = player_urls,
      ktcv = player_vals
    )
  )
  
  rm(pg, url, player_names, player_urls, player_vals)
  
  Sys.sleep(5)
  
}

# edit pick values

pick_list <- tibble(
  draft_year = rep(draft_year, 48),
  round = c(rep(1,12),rep(2,12),rep(3,12),rep(4,12)),
  pick = rep(1:12, 4),
  estimate = rep(c(rep("Early",4),rep("Mid",4),rep("Late",4)),4)
)

ktc.picks <- ktc |>
  filter(grepl("[0-9]", player)) |>
  tidyr::separate(
    player, into = c("draft_year", "estimate", "round_ordinal"),
    sep = "\\ ", convert = TRUE, remove = FALSE
  ) |>
  mutate(round = as.integer(substr(round_ordinal, 1, 1)))

ktc.picks <- pick_list |>
  left_join(
    ktc.picks,
    by = c("draft_year", "round", "estimate"),
    multiple = "all"
  ) |>
  mutate(
    player = paste0(draft_year, " Pick ", round, ".", formatC(pick, width = 2, format = "d", flag = "0"))
  ) |>
  select(player, estimate, ktcv) |>
  bind_rows(
    ktc.picks |>
      filter(draft_year > max(pick_list$draft_year)) |>
      filter(estimate == "Mid") |>
      mutate(player = paste0(round_ordinal, " Round (",draft_year,")")) |>
      select(player, estimate, ktcv)
  ) |>
  select(-estimate) |>
  arrange(-ktcv)

player_list <- readRDS("data/player_list.rds") |>
  mutate(player_id = trimws(gsis_id), name = full_name) |>
  fix_names() |>
  add.name_id()

ktc.out <- ktc |>
  filter(!grepl("[0-9]", player)) |>
  add.name_id() |>
  left_join(
    player_list |>
      filter(position %in% c("QB","FB","RB","WR","TE")) |>
      distinct(player_id, sleeper_id, name_id) |>
      mutate(
        priority = case_when(
          !is.na(player_id) & !is.na(sleeper_id) ~ 1,
          !is.na(player_id) & is.na(sleeper_id) ~ 2,
          is.na(player_id) & !is.na(sleeper_id) ~ 3,
          TRUE ~ 4
        )
      ) |>
      group_by(name_id) |>
      arrange(priority) |>
      slice(1) |>
      ungroup() |>
      select(-priority),
    by = "name_id"
  ) |>
  bind_rows(ktc.picks) |>
  arrange(-ktcv)

ktc.out |> saveRDS("dynasty-tools/data/ktc_values.rds")
