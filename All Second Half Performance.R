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
back_all_years <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(back_all_years) <- c("team_name", "net_round", "position_delta", 
                              "year", "wcc_position")

for (i in 1950:2021) {
  year_x <- i
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
    select(team_name = name, wcc_position)
  
  back_delta <- merge(back_delta, wcc_result, by = "team_name")
  
  back_delta <- back_delta %>%
    mutate(net_round = round - mid_point) %>%
    select(team_name, net_round, position_delta, year, wcc_position)
  
  back_all_years <- rbind(back_all_years, back_delta)
}

back_all_years <- back_all_years %>%
  mutate(index = paste(team_name, year))

black <- c()

for (i in 1:nrow(back_all_years)) {
  black <- append(black, "black")
}


p_all_lines <- ggplot(back_all_years,
    aes(x = net_round,
        y = position_delta,
        color = index)) +
  geom_line(alpha = 0.2) +
  scale_color_manual(values = black) +
  theme(legend.position = "none")

p_all_lines