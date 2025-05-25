#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
library(elo)
library(data.table)
library(purrr)
#####################################################
# Getting Team Averages
# afl_stats <- map_dfr(2013:2025, ~fetch_player_stats_afl(season = .x))
# saveRDS(afl_stats, "afl_stats.RDS")
afl_stats <- readRDS("afl_stats.RDS")
colnames(afl_stats)
str(afl_stats)

afl_team_averages <- afl_stats %>%
  # Extract year as season
  mutate(season = year(ymd_hms(utcStartTime))) %>%
  # Group by team and season
  group_by(teamId, team.name, season) %>%
  # Count distinct matches per team
  summarise(
    games_played = n_distinct(providerId),
    # Sum all numeric stats across games
    across(where(is.numeric), ~sum(.x, na.rm = TRUE), .names = "total_{.col}"),
    .groups = "drop"
  ) %>%
  # Calculate per-game averages
  mutate(across(starts_with("total_"), ~.x / games_played, .names = "avg_{.col}"))
#####################################################
# Did a team play in the grand final?
grand_final_teams <- afl_stats %>%
  mutate(season = year(ymd_hms(utcStartTime))) %>%
  filter(round.name == "Grand Final") %>%
  select(season, teamId, team.name) %>%
  distinct() %>%
  mutate(grand_final = TRUE)

afl_team_averages <- afl_team_averages %>%
  left_join(grand_final_teams, by = c("teamId", "team.name", "season")) %>%
  mutate(grand_final = ifelse(is.na(grand_final), FALSE, grand_final))
#####################################################
afl_tables <- fetch_results_afltables(2013:2025)
colnames(afl_tables)
# Creating visualisations
afl_long <- afl_tables %>%
  select(Game, Season, Round.Number, Round, Date, Venue,
         Home.Team, Away.Team, Home.Points, Away.Points) %>%
  mutate(
    HomeTeam = Home.Team,
    AwayTeam = Away.Team
  ) %>%
  pivot_longer(
    cols = c(Home.Team, Away.Team),
    names_to = "team_type",
    values_to = "Team"
  ) %>%
  mutate(
    Points.For = ifelse(team_type == "Home.Team", Home.Points, Away.Points),
    Points.Against = ifelse(team_type == "Home.Team", Away.Points, Home.Points),
    Opponent = ifelse(team_type == "Home.Team", AwayTeam, HomeTeam)
  ) %>%
  select(Game, Date, Season, Round.Number, Round, Venue,
         Team, Opponent, Points.For, Points.Against)

afl_squiggle <- afl_long %>%
  group_by(Season, Team) %>%
  arrange(Season, Team, Round.Number) %>%
  mutate(
    Game.Number = row_number(),
    Attack.Rating = cummean(Points.For),
    Defence.Rating = cummean(Points.Against)
  ) %>%
  ungroup()

afl_squiggle_2025 <- afl_squiggle %>%
  filter(Season == 2025)

season_end_2025 <- afl_squiggle_2025 %>%
  group_by(Team) %>%
  filter(Game.Number == max(Game.Number)) %>%
  ungroup()

grand_final_teams_all <- afl_long %>%
  filter(Round == "GF", Season < 2025) %>%
  select(Season, Team) %>%
  distinct()

gf_points <- afl_squiggle %>%
  semi_join(grand_final_teams_all, by = c("Season", "Team")) %>%
  group_by(Season, Team) %>%
  filter(Game.Number == max(Game.Number)) %>%
  ungroup() %>%
  mutate(grand_final = TRUE)

ggplot(afl_squiggle_2025, aes(x = Defence.Rating, y = Attack.Rating, group = Team)) +
  geom_path(aes(color = Team), linewidth = 1.2, alpha = 0.7) +
  geom_point(data = gf_points, aes(x = Defence.Rating, y = Attack.Rating), shape = 24, size = 3, fill = "gold", color = "black") +
  geom_point(data = season_end_2025, shape = 21, size = 3, fill = "black", color = "black") +
  labs(
    title = "2025 AFL Squiggle",
    subtitle = "Live 2025 team trajectories with historical Grand Finalists (2013–2024) in gold",
    x = "Defence Rating (Lower = Better)",
    y = "Attack Rating (Higher = Better)"
  ) +
  theme_minimal()
#####################################################
season_end_2025 <- afl_squiggle %>%
  filter(Season == 2025) %>%
  group_by(Team) %>%
  filter(Game.Number == max(Game.Number)) %>%
  ungroup()

grand_final_teams_all <- afl_long %>%
  filter(Round == "GF", Season < 2025) %>%
  select(Season, Team) %>%
  distinct()

gf_points <- afl_squiggle %>%
  semi_join(grand_final_teams_all, by = c("Season", "Team")) %>%
  group_by(Season, Team) %>%
  filter(Game.Number == max(Game.Number)) %>%
  ungroup() %>%
  mutate(grand_final = TRUE)

ggplot() +
  geom_point(data = season_end_2025, aes(x = Defence.Rating, y = Attack.Rating, color = Team), size = 4) +
  geom_text(data = season_end_2025, aes(x = Defence.Rating, y = Attack.Rating, label = Team), vjust = -0.8, size = 3) +
  geom_point(data = gf_points, aes(x = Defence.Rating, y = Attack.Rating), shape = 24, size = 3, fill = "gold", color = "black") +
  labs(
    title = "2025 AFL Team Ratings (Snapshot + Historical Grand Finalists)",
    subtitle = "Current season positions with all past Grand Finalists (2013–2024) as gold triangles",
    x = "Defence Rating (Points Conceded/Game – Lower is Better)",
    y = "Attack Rating (Points Scored/Game – Higher is Better)"
  ) +
  theme_minimal()
