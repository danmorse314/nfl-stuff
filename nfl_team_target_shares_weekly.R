library(tidyverse)

nflreadr::.clear_cache()

pbp <- nflreadr::load_pbp()

team_colors <- nflfastR::teams_colors_logos

target_plot <- function(tm, save = TRUE){
  team <- pbp |>
    filter(posteam == tm & !is.na(receiver_player_name)) |>
    group_by(week, defteam) |>
    summarize(
      team_targets = n(),
      team_catches = sum(complete_pass, na.rm = TRUE),
      team_yards = sum(yards_gained, na.rm = TRUE),
      team_total_epa = sum(epa, na.rm = TRUE),
      team_air_yards = sum(air_yards, na.rm = TRUE),
      .groups = "drop"
    )
  
  rec <- pbp |>
    filter(posteam == tm & !is.na(receiver_player_name)) |>
    group_by(week, defteam, receiver = receiver_player_name) |>
    summarize(
      targets = n(),
      catches = sum(complete_pass, na.rm = TRUE),
      yards = sum(yards_gained, na.rm = TRUE),
      total_epa = sum(epa, na.rm = TRUE),
      air_yards = sum(air_yards, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(team, by = c("week","defteam")) |>
    mutate(
      target_share = targets / team_targets,
      catch_share = catches / team_catches,
      yard_share = yards / team_yards,
      epa_share = total_epa / team_total_epa,
      air_yards_share = air_yards / team_air_yards
    )
  
  chart <- rec |>
    filter(target_share > .1) |>
    mutate(rec_label = receiver) |>
    bind_rows(
      rec |>
        filter(target_share <= .1) |>
        group_by(week, defteam, team_targets) |>
        summarize(
          recs = n(),
          receiver = ifelse(recs == 1, receiver, "Less than 10% each"),
          rec_label = ifelse(recs == 1, receiver, paste(recs, "players")),
          targets = sum(targets),
          target_share = sum(target_share),
          .groups = "drop"
        )
    )
  
  rcvrs <- chart |>
    group_by(receiver) |>
    filter(receiver != "Less than 10% each") |>
    summarize(
      targets = sum(targets),
      team_targets = sum(team_targets),
      target_share = targets / team_targets,
      .groups = "drop"
    ) |>
    arrange(target_share) |>
    pull(receiver)
  
  chart <- chart |>
    mutate(
      rec_fac = factor(
        receiver,
        levels = c("Less than 10% each",rcvrs)
        #levels = c("Less than 10% each","D.Houston","P.Hendershot","J.Ferguson","T.Pollard","N.Brown","D.Schultz","M.Gallup","C.Lamb")
      )
    ) |>
    left_join(
      team_colors, by = c("defteam" = "team_abbr")
    )
  
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7","#000000")
  cbPalette <- c("#000000",rep(c("#CC79A7","#D55E00","#0072B2","#F0E442","#009E73","#56B4E9","#E69F00","#999999","#F5F5F5"),2))[1:length(unique(chart$rec_fac))]
  
  chart_labs <- chart |>
    arrange(week, desc(rec_fac)) |>
    group_by(week) |>
    mutate(
      cum_share = cumsum(target_share),
      lag_share = ifelse(is.na(lag(target_share)), 0, lag(cum_share)),
      x_lab = lag_share + (.5 * target_share),
      text_size = case_when(
        nchar(rec_label)/150 > target_share ~ 2.3,
        nchar(rec_label)/100 > target_share & nchar(rec_label)/150 < target_share ~ 3,
        TRUE ~ 4
        )
    ) |>
    ungroup()
  
  team_name <- filter(team_colors, team_abbr == tm) |>
    pull(team_name)
  
  plot1 <- chart |>
    ggplot(aes(target_share, as.factor(desc(week)))) +
    geom_col(aes(fill = rec_fac), position = "stack", show.legend = FALSE) +
    ggimage::geom_image(
      data = select(chart, team_logo_espn, defteam, week) |> distinct(),
      aes(image = team_logo_espn, x = -.025),
      size = .04, asp = 9/5
    ) +
    scale_fill_manual(values = cbPalette) +
    geom_text(
      data = chart_labs,
      aes(x = x_lab, label = rec_label, size = text_size),
      color = ifelse(chart_labs$rec_label %in% c(rcvrs[9],rcvrs[4]),"black","white")
    ) +
    scale_size_identity() +
    scale_x_continuous(
      limits = c(-.05,1.02),
      expand = c(0,0),
      breaks = seq(0,1,.25),
      labels = paste0(seq(0,100,25),"%")
    ) +
    scale_y_discrete(
      labels = unique(arrange(chart,-week)$week)
    ) +
    theme_bw() +
    theme(
      axis.ticks.y = element_blank()
    ) +
    labs(
      x = "Target Share", y = "Week",
      caption = "data: nflfastR · chart: @danmorse_",
      title = glue::glue("{team_name} targets by week"),
      subtitle = "all players with at least 10% share in given week"
    )
  
  if(save == TRUE){
    ggsave(plot = plot1, glue::glue("figures/target_share_weekly_{tm}.png"), width = 9, height = 5)
  }
  
  return(plot1)
}
