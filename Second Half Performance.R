# Call Libraries
library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(ggplot2)

# Import Data
path <- "data/drivers.csv"
drivers <- read.csv(path, header = TRUE, sep = ",")

path <- "data/races.csv"
races <- read.csv(path, header = TRUE, sep = ",")

path <- "data/results.csv"
results <- read.csv(path, header = TRUE, sep = ",")

path <- "data/lap_times.csv"
laps <- read.csv(path, header = TRUE, sep = ",")

path <- "data/pit_stops.csv"
pits <- read.csv(path, header = TRUE, sep = ",")

path <- "data/qualifying.csv"
quali <- read.csv(path, header = TRUE, sep = ",")

path <- "data/circuits.csv"
circ <- read.csv(path, header = TRUE, sep = ",")

path <- "data/driver_standings.csv"
wdc <- read.csv(path, header = TRUE, sep = ",")

path <- "data/constructor_standings.csv"
wcc <- read.csv(path, header = TRUE, sep = ",")

path <- "data/constructors.csv"
constructors <- read.csv(path, header = TRUE, sep = ",")

# Find the average finishing position for the first half of the season
year_x <- 2015
last_round <- max(races$round[races$year == year_x])
mid_point <- round(last_round / 2)

first_half <- races %>%
  filter(year == year_x & round <= mid_point) %>%
  merge(results, by = "raceId") %>%
  group_by(constructorId) %>%
  summarise(mean_result = mean(positionOrder)) %>%
  merge(select(constructors, constructorId, name), by = "constructorId") %>%
  ungroup()

# Find how each team performed over the back half of the season
back_half <- races %>%
  filter(year == year_x & round > mid_point) %>%
  merge(results, by = "raceId") %>%
  group_by(raceId, round, constructorId) %>%
  summarise(team_position = mean(positionOrder)) %>%
  merge(first_half, by = "constructorId") %>%
  mutate(relative_position = team_position - mean_result) %>%
  ungroup()

back_by_team <- select(back_half, round, name, relative_position) %>%
  pivot_wider(names_from = name, values_from = relative_position) %>%
  arrange(round)

back_delta <- data.frame(matrix(ncol = ncol(back_by_team), nrow = 0))
colnames(back_delta) <- colnames(back_by_team)

back_delta[1, ] <- 0
back_delta[1, 1] <- mid_point

for (i in 1:nrow(back_by_team)) {
  j = i + 1
  back_delta[j, ] <- back_delta[i, ] - back_by_team[i, ]
  back_delta[j, 1] <- back_by_team[i, 1]
}

back_delta <- back_delta %>%
  pivot_longer(!round, names_to = "team_name", values_to = "position_delta") %>%
  merge(filter(races, year == year_x), by = "round") %>%
  arrange(team_name)

teams_x = unique(back_delta$team_name)

wcc_result <- select(wcc, wcc_position = position, raceId, constructorId) %>%
  merge(select(races, year, round, raceId), by = "raceId") %>%
  merge(constructors, by = "constructorId") %>%
  filter(year == year_x & round == last_round) %>%
  select(team_name = name, wcc_position) %>%
  mutate(scale = ((max(wcc_position) + 1) - wcc_position) / max(wcc_position)) %>%
  arrange(desc(wcc_position))

back_delta <- merge(back_delta, wcc_result, by = "team_name")

# Color mapping by team
teams_2000s <- constructors %>%
  select(team_name = name, constructorId) %>%
  merge(results, by = "constructorId") %>%
  merge(races, by = "raceId") %>%
  select(team_name, year) %>%
  filter(year >= 2000) %>%
  group_by(team_name) %>%
  summarise()

teams_2000s$color[teams_2000s$team_name == "Alfa Romeo"] <- "#9e251b"
teams_2000s$color[teams_2000s$team_name == "AlphaTauri"] <- "#08253c"
teams_2000s$color[teams_2000s$team_name == "Alpine F1 Team"] <- "#0068af"
teams_2000s$color[teams_2000s$team_name == "Arrows"] <- "#f98226"
teams_2000s$color[teams_2000s$team_name == "Aston Martin"] <- "#034a2d"
teams_2000s$color[teams_2000s$team_name == "BAR"] <- "#929262"
teams_2000s$color[teams_2000s$team_name == "Benetton"] <- "#0094d4"
teams_2000s$color[teams_2000s$team_name == "BMW Sauber"] <- "#06306f"
teams_2000s$color[teams_2000s$team_name == "Brawn"] <- "#a2b10f"
teams_2000s$color[teams_2000s$team_name == "Caterham"] <- "#034a2d"
teams_2000s$color[teams_2000s$team_name == "Ferrari"] <- "#d00002"
teams_2000s$color[teams_2000s$team_name == "Force India"] <- "#ca0f73"
teams_2000s$color[teams_2000s$team_name == "Haas F1 Team"] <- "#79797b"
teams_2000s$color[teams_2000s$team_name == "Honda"] <- "#79797b"
teams_2000s$color[teams_2000s$team_name == "HRT"] <- "#978141"
teams_2000s$color[teams_2000s$team_name == "Jaguar"] <- "#1a2e24"
teams_2000s$color[teams_2000s$team_name == "Jordan"] <- "#e1c30f"
teams_2000s$color[teams_2000s$team_name == "Lotus"] <- "#f0a901"
teams_2000s$color[teams_2000s$team_name == "Lotus F1"] <- "#f0a901"
teams_2000s$color[teams_2000s$team_name == "Manor Marussia"] <- "#043d80"
teams_2000s$color[teams_2000s$team_name == "Marussia"] <- "#043d80"
teams_2000s$color[teams_2000s$team_name == "McLaren"] <- "#f27902"
teams_2000s$color[teams_2000s$team_name == "Mercedes"] <- "#019b97"
teams_2000s$color[teams_2000s$team_name == "MF1"] <- "#2f2f2f"
teams_2000s$color[teams_2000s$team_name == "Minardi"] <- "#2b2726"
teams_2000s$color[teams_2000s$team_name == "Prost"] <- "#00327b"
teams_2000s$color[teams_2000s$team_name == "Racing Point"] <- "#ca0f73"
teams_2000s$color[teams_2000s$team_name == "Red Bull"] <- "#4619a1"
teams_2000s$color[teams_2000s$team_name == "Renault"] <- "#eeb204"
teams_2000s$color[teams_2000s$team_name == "Sauber"] <- "#9e251b"
teams_2000s$color[teams_2000s$team_name == "Spyker"] <- "#bf5914"
teams_2000s$color[teams_2000s$team_name == "Spyker MF1"] <- "#bf5914"
teams_2000s$color[teams_2000s$team_name == "Super Aguri"] <- "#b1002a"
teams_2000s$color[teams_2000s$team_name == "Toro Rosso"] <- "#08253c"
teams_2000s$color[teams_2000s$team_name == "Toyota"] <- "#79797b"
teams_2000s$color[teams_2000s$team_name == "Virgin"] <- "#181818"
teams_2000s$color[teams_2000s$team_name == "Williams"] <- "#00a0de"

teams_2000s <- wcc_result %>%
  merge(teams_2000s, by = "team_name") %>%
  arrange(desc(wcc_position))

teams_2000s$line_type <- "solid"
teams_2000s$line_type[teams_2000s$scale <= .5] <- "dashed"

teams_2000s$size_type <- 0.8
teams_2000s$size_type[teams_2000s$wcc_position <= 3] <- 1.2

color_list <- teams_2000s$color[teams_2000s$team_name %in% teams_x]
line_list <- teams_2000s$line_type[teams_2000s$team_name %in% teams_x]
size_list <- teams_2000s$size_type[teams_2000s$team_name %in% teams_x]

p_back_half <- ggplot(back_delta,
      aes(x = round,
          y = position_delta,
          color = reorder(team_name, desc(wcc_position)))) +
  geom_abline(aes(intercept = 0, slope = 0),
              size = 0.8) +
  geom_line(aes(linetype = reorder(team_name, desc(wcc_position)),
                size = reorder(team_name, desc(wcc_position))),
            alpha = 0.7) +
  scale_color_manual(values = color_list) +
  scale_linetype_manual(values = line_list) +
  scale_size_manual(values = size_list) +
  guides(color = guide_legend(title = "Teams in WCC Order", reverse = TRUE),
         linetype = guide_legend(title = "Teams in WCC Order", reverse = TRUE),
         size = guide_legend(title = "Teams in WCC Order", reverse = TRUE)) +
  labs(x = "Championship Round",
       y = "Cummulative Relative Finishing Position",
       title = paste("Teams Relative Performance in the Second Half of the Season (",
                     year_x, ")", sep = ""))

p_back_half  