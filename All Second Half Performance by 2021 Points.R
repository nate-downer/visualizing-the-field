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

# Update past results to match 2022 points scheme
adj_res <- results %>%
  mutate(adj_points = case_when(positionOrder == 1 ~ 25,
                                positionOrder == 2 ~ 18,
                                positionOrder == 3 ~ 15,
                                positionOrder == 4 ~ 12,
                                positionOrder == 5 ~ 10,
                                positionOrder == 6 ~ 8,
                                positionOrder == 7 ~ 6,
                                positionOrder == 8 ~ 4,
                                positionOrder == 9 ~ 2,
                                positionOrder == 10 ~ 1,
                                positionOrder > 10 ~ 0),
         adj_points_plus = case_when(rank == 1 & positionOrder <= 10 ~ adj_points + 1,
                                     rank == 1 & positionOrder > 10 ~ adj_points,
                                     rank != 1 ~ adj_points))
  
adj_res$adj_points_plus <- replace_na(adj_res$adj_points_plus, 0)

# Find the average points per team for the first half of the season
back_all_years <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(back_all_years) <- c("team_name", "net_round", "position_delta", 
                              "year", "wcc_position")

for (i in 1950:2021) {
  year_x <- i
  last_round <- max(races$round[races$year == year_x])
  mid_point <- round(last_round / 2)
  
  first_half <- races %>%
    filter(year == year_x & round <= mid_point) %>%
    merge(adj_res, by = "raceId") %>%
    group_by(constructorId) %>%
    summarise(mean_points = mean(adj_points_plus)) %>%
    merge(select(constructors, constructorId, name), by = "constructorId") %>%
    ungroup()
  
  # Find how each team performed over the back half of the season
  back_half <- races %>%
    filter(year == year_x & round > mid_point) %>%
    merge(adj_res, by = "raceId") %>%
    group_by(raceId, round, constructorId) %>%
    summarise(team_points = mean(adj_points_plus)) %>%
    merge(first_half, by = "constructorId") %>%
    mutate(relative_points = team_points - mean_points) %>%
    ungroup()
  
  back_by_team <- select(back_half, round, name, relative_points) %>%
    pivot_wider(names_from = name, values_from = relative_points) %>%
    arrange(round) 
  
  back_by_team[is.na(back_by_team)] <- 0
  
  back_delta <- data.frame(matrix(ncol = ncol(back_by_team), nrow = 0))
  colnames(back_delta) <- colnames(back_by_team)
  
  back_delta[1, ] <- 0
  back_delta[1, 1] <- mid_point
  
  for (i in 1:nrow(back_by_team)) {
    j = i + 1
    back_delta[j, ] <- back_delta[i, ] + back_by_team[i, ]
    back_delta[j, 1] <- back_by_team[i, 1]
  }
  
  back_delta <- back_delta %>%
    pivot_longer(!round, names_to = "team_name", values_to = "points_delta") %>%
    merge(filter(races, year == year_x), by = "round") %>%
    arrange(team_name)
  
  wcc_result <- select(wcc, wcc_position = position, raceId, constructorId) %>%
    merge(select(races, year, round, raceId), by = "raceId") %>%
    merge(constructors, by = "constructorId") %>%
    filter(year == year_x & round == last_round) %>%
    select(team_name = name, wcc_position)
  
  back_delta <- merge(back_delta, wcc_result, by = "team_name")
  
  back_delta <- back_delta %>%
    mutate(net_round = round - mid_point) %>%
    select(team_name, net_round, points_delta, year, wcc_position)
  
  back_all_years <- rbind(back_all_years, back_delta)
}

back_all_years <- back_all_years %>%
  mutate(index = paste(team_name, year))

# Highlight and theme select results
hlights <- c("Mercedes 2019",
             "Brawn 2009",
             "Williams 1979",
             "Jordan 1998",
             "Mercedes 2018",
             "Williams 1983",
             "McLaren 2021",
             "Red Bull 2021")

manual_vals <- back_all_years %>%
  group_by(index, year) %>%
  summarise(final_points_delta = 0) %>%
  ungroup()

for (i in manual_vals$index) {
  net_last_round <- max(back_all_years$net_round[back_all_years$index == i])
  x <- back_all_years$points_delta[back_all_years$index == i &
                                   back_all_years$net_round == net_last_round]
  manual_vals$final_points_delta[manual_vals$index == i] <- x
}

manual_vals <- manual_vals %>%
  mutate(abs = abs(final_points_delta),
         color = case_when(index %in% hlights ~ "blue",
                           TRUE ~ "black"),
         alpha = case_when(index %in% hlights ~ 1,
                           year == 2021 ~ .7,
                           TRUE ~ 0.09),
         line_type = case_when(index %in% hlights ~ "longdash",
                               year == 2021 ~ "dashed",
                               TRUE ~ "solid"))

back_all_years <- merge(back_all_years, manual_vals, by = "index")

# Plot the Results   
p_all_lines <- ggplot(back_all_years,
    aes(x = net_round,
        y = points_delta,
        color = final_points_delta,
        alpha = index)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_line(aes(linetype = index)) +
  scale_color_gradient2(low = "#9e251b", mid = "grey10", high = "#5d9fc8", 
                        midpoint = 0) +
  scale_alpha_manual(values = manual_vals$alpha) +
  scale_linetype_manual(values = manual_vals$line_type) +
  labs(x = "Championship Round (Where Zero is the Halfway Point)",
       y = "Points Scored (Relative to First Half of the Season)") +
  scale_x_continuous(breaks = seq(0,11,1)) +
  scale_y_continuous(breaks = seq(-80,80,10)) +
  theme(legend.position = "none",
        axis.line.y = element_line(color = "grey10"))


# Display the plot
p_all_lines