# madden ratings scraper

get_madden_ratings <- function(edition = 23, update = FALSE){
  
  # edition: game number, ie '23' returns ratings for Madden 23
  #             -only works for 23 & 22 as of now
  
  team_list <- nflfastR::teams_colors_logos |>
    dplyr::select(team_abbr, full_team_name = team_name, team_nick) |>
    dplyr::filter(
      team_abbr != "SD" & team_abbr != "OAK" &
        team_abbr != "LAR" & team_abbr != "STL"
    ) |>
    dplyr::mutate(
      team_nick = ifelse(team_abbr == "WAS","Commanders",team_nick),
      full_team_name = ifelse(team_abbr == "WAS","Washington Commanders",full_team_name)
    )
  
  # raw json ratings url
  url <- paste0("https://ratings-api.ea.com/v2/entities/m",edition,"-ratings?sort=overall_rating:DESC,firstName:ASC&limit=3000")
  
  # only loads first 1k entries
  # iterate
  
  ratings <- NULL
  for(i in seq(0,3000,1000)){
    
    # add offset to skip ratings 1k at a time
    url_sub <- paste0(url,"&offset=",i)
    
    # read json
    site <- jsonlite::read_json(url_sub)
    
    # put into a tibble
    df <- site$docs |>
      dplyr::bind_rows()
    
    # compile
    ratings <- dplyr::bind_rows(ratings, df)
    
    rm(site, df, i, url_sub)
  }
  
  # light cleaning
  ratings <- janitor::clean_names(ratings) |>
    dplyr::mutate(
      season = as.numeric(paste0(20,edition)),
      short_name = paste0(substr(first_name,1,1),".",last_name),
      position_group = dplyr::case_when(
        position %in% c("LT","LG","C","RG","RT") ~ "OL",
        position %in% c("RE","LE","DT") ~ "DL",
        position %in% c("K","P") ~ "K/P",
        stringr::str_detect(position, "LB") ~ "LB",
        position %in% c("FS","SS") ~ "S",
        position %in% c("HB","FB") ~ "RB",
        TRUE ~ position
      ),
      throw_accuracy_mean_rating = (
        throw_accuracy_short_rating +
          throw_accuracy_mid_rating +
          throw_accuracy_deep_rating
      ) / 3 |>
        round(1)
    ) |>
    dplyr::rename(team_nick = team) |>
    dplyr::left_join(team_list, by = "team_nick") |>
    dplyr::select(
      name = full_name_for_search, team_abbr, position, position_group,
      archetype, overall_rating, age, height, weight, iteration,
      dplyr::everything()
    )
  
  # combine with previous scrape
  if(update == TRUE){
    ratings <- ratings |>
      dplyr::bind_rows(
        readr::read_csv("madden/madden_23_player_ratings.csv",
                        col_types = readr::cols())
      ) |>
      # remove duplicates - incase scraping ratings that haven't changed
      # change will show up under the 'iteration' column
      dplyr::distinct()
  }
  
  
  return(ratings)
}

