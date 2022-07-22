# madden ratings scraper

# raw json ratings url
url <- "https://ratings-api.ea.com/v2/entities/m23-ratings?sort=overall_rating:DESC,firstName:ASC&limit=2370"

# only loads first 1k entries
# iterate

ratings <- NULL
for(i in seq(0,10000,1000)){
  
  # add ofset to skip ratings 1k at a time
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
  dplyr::select(
    name = full_name_for_search, team, position, overall_rating, age, tidyselect::everything()
  )

# save
ratings |>
  write_csv("madden_23_launch_ratings.csv")
