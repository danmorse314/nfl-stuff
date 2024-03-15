# break-even rates in kickoff returns

library(tidyverse)
library(gt)

pbp <- nflreadr::load_pbp(seasons = 1999:2023)

# add some vars

pbp <- pbp |>
  group_by(game_id) |>
  mutate(
    next_yardline = lead(yardline_100),
    next_yardline = ifelse(play_type == "kickoff" & lead(posteam) == return_team, next_yardline, NA_integer_),
    next_ep = lead(ep),
    next_ep = ifelse(play_type == "kickoff", next_ep, NA_real_),
    kick_location = next_yardline - return_yards,
    past_25 = ifelse(next_yardline < 75, 1, 0)
  ) |>
  ungroup()

# epa of 1st down at own 25 yard line in 1st quarter

pbp_25 <- pbp |>
  filter(qtr == 1 & yardline_100 == 75 & down == 1 & ydstogo == 10 & posteam_score == defteam_score)

pbp |>
  filter(qtr == 1 & yardline_100 == 75 & down == 1 & ydstogo == 10 & posteam_score == defteam_score) |>
  filter(game_seconds_remaining == 3600) |>
  ggplot(aes(ep)) +
  geom_density() +
  facet_wrap(~posteam_type) +
  theme_bw()

pbp_25 |>
  ggplot(aes(ep)) +
  geom_density() +
  facet_wrap(~posteam_type) +
  theme_bw()

# noticable different EP for home vs away team

# epa of a kick return touchdown relative to a touchback

kickoff_eps <- pbp_25 |>
  group_by(posteam_type) |>
  summarise(ep = mean(ep),
            .groups = "drop") |>
  mutate(ep_td = 6,
         diff = ep_td - ep)

# probability of return td

kickoff_td_probs <- pbp |>
  filter(play_type == "kickoff") |>
  filter(touchback == 0) |>
  group_by(posteam_type) |>
  summarise(
    returns = n(),
    td_prob = mean(touchdown, na.rm = TRUE),
    past_25_prob = mean(past_25, na.rm = TRUE),
    ep_past_25 = mean(next_ep[past_25 == 1], na.rm = TRUE),
    .groups = "drop"
  )

# combined

model.data <- kickoff_eps |>
  left_join(
    kickoff_td_probs, by = "posteam_type"
  ) |>
  # td breakevens
  mutate(
    return_ep_diff = ep_past_25 - ep,
    return_td_value = td_prob * diff,
    return_value = ep_past_25 * past_25_prob,
    touchback_value = ep,
    return_td_breakeven = ep / diff
  )

glimpse(model.data)

tbl <- model.data |>
  mutate(touchback_prob = 1) |>
  # breakeven return probability
  mutate(
    return_prob_breakeven = touchback_value / ep_past_25
  ) |>
  select(posteam_type,
         touchback_prob, ep_touchback = ep, touchback_value,
         past_25_prob, ep_past_25, return_value,
         return_prob_breakeven)

tbl |>
  gt() |>
  fmt_number(
    columns = c(ep_touchback, touchback_value,
                ep_past_25, return_value),
    decimals = 2
  ) |>
  fmt_percent(
    columns = contains("prob"),
    decimals = 0
  ) |>
  tab_spanner(
    columns = contains("touchback"),
    label = "Touchback"
  ) |>
  tab_spanner(
    columns = c(past_25_prob, ep_past_25, return_value),
    label = ">25-Yard Return"
  ) |>
  cols_label(
    posteam_type = "Team",
    touchback_prob = "Prob.",
    ep_touchback = "Avg. EP",
    touchback_value = "Value",
    past_25_prob = "Prob.",
    ep_past_25 = "Avg. EP",
    return_value = "Value",
    return_prob_breakeven = html("Breakeven<br>Rate")
  )

# how likely is it to get past the 25 based on where the kick lands?

returns <- pbp |>
  filter(play_type == "kickoff" & yardline_100 == 30) |>
  filter(kick_location >= -9 & !is.na(kick_location)) |>
  filter(touchback == 0) |>
  group_by(kick_location) |>
  summarise(
    n = n(),
    past_25_prob = mean(past_25),
    .groups = "drop"
  )

returns |>
  ggplot(aes(kick_location, past_25_prob)) +
  geom_point(aes(size = n), alpha = .5) +
  #geom_smooth() +
  scale_x_reverse(
    breaks = seq(110,0,-10),
    labels = c(seq(-10,50,10),seq(40,0,-10)),
    limits = c(110,0),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0,1,.1),
    labels = paste0(seq(0,100,10),"%")
  ) +
  theme_bw() +
  labs(
    title = "Kickoffs from 30-yard line, with return",
    subtitle = "1999-2023 NFL seasons",
    caption = "data: nflfastR"
  )
