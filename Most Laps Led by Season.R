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

path <- "data/constructors.csv"
constructors <- read.csv(path, header = TRUE, sep = ",")

# Question 1:
  # Which driver led the most laps each season, what percent did they lead,
  # and were they the WDC?

# Build data frame with the number of laps each driver completed each year
l_data <- merge(laps, races, by = "raceId")

l_data <- l_data %>%
  filter(position == 1) %>%
    group_by(year, driverId) %>%
      summarise(lapsLed = n()) %>%
    ungroup()

# Add data for total laps each year
laps_per_year <- results %>%
  group_by(raceId) %>%
    summarise(maxLaps = max(laps)) %>%
  ungroup()

laps_per_year <- merge(laps_per_year, races, by = "raceId")

laps_per_year <- laps_per_year %>%
  group_by(year) %>%
    summarise(totalLaps = sum(maxLaps)) %>%
  ungroup()

l_data <- merge(l_data, laps_per_year, by = "year")

# Add each driver's WDC position at the end of the year
season_wdc <- merge(wdc, races, by = "raceId")

season_wdc <- season_wdc %>%
  arrange(desc(round)) %>%
    distinct(year, driverId, .keep_all = TRUE) %>%
      select(year, driverId, position)

l_data = merge(l_data, season_wdc, by = c("year", 'driverId'))
l_data = merge(l_data, 
               select(drivers, code, driverRef, driverId), 
               by = "driverId")

# Handel exceptions with driver code
l_data$code[l_data$driverRef == "damon_hill"] <- "HIL"
l_data$code[l_data$driverRef == "hakkinen"] <- "HAK"

l_data_by_year <- l_data %>%
  arrange(desc(lapsLed)) %>%
    distinct(year, .keep_all = TRUE)

plot3 = ggplot(l_data_by_year, 
  aes(x = year,
      y = lapsLed,
      fill = position)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "red", high = "grey") +
  geom_text(aes(label = paste(code, " (P", position, ")", sep = "")), 
            nudge_y = -55, 
            color = "black", 
            size = 2.5) +
  coord_flip() +
  labs(x = "Year",
       y = "Total Laps Led",
       title = "Most Laps Led by Year") +
  theme(legend.position = "none")
  
# Question 2:
  # how did the number of laps led by each team change from year to year?

# Get the data for laps led by each team
t_data = merge(select(results, raceId, constructorId, driverId), races, 
               by = "raceId")
t_data = merge(t_data, laps, by = c("raceId", "driverId"))

t_data = t_data %>%
  filter(position == 1) %>%
    group_by(year, driverId, constructorId) %>%
      summarise(lapsLed = n()) %>%
        group_by(year, constructorId) %>%
          summarise(lapsLed = sum(lapsLed))%>%
    ungroup()

t_data = merge(t_data, select(constructors, constructorId, name), 
               by = "constructorId")

short_list <- c("Brawn", 
                "Ferrari", 
                "McLaren", 
                "Mercedes", 
                "Red Bull",
                "Renault",
                "Williams")

plot4 <- ggplot(filter(t_data, name %in% short_list),
  aes(x = year,
      y = lapsLed,
      fill = name)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values=c("#daf102", 
                             "#d00002", 
                             "#f27902", 
                             "#019b97", 
                             "#001e49",
                             "#f2c705",
                             "#00a0de")) +
  labs(x = "Year",
       y = "Count of Laps Led",
       title = "Laps Led by Major Teams by Year")

plot5 <- ggplot(filter(t_data, name %in% short_list),
    aes(x = year,
        y = name,
        fill = name)) +
  geom_raster(aes(alpha = lapsLed), hjust = 0.5,
              vjust = 0.5, interpolate = FALSE) +
  scale_fill_manual(values=c("#daf102", 
                             "#d00002", 
                             "#f27902", 
                             "#019b97", 
                             "#001e49",
                             "#f2c705",
                             "#00a0de")) +
  labs(x = "Year",
       y = "Team",
       title = "Laps Led by Major Teams by Year") +
  theme(legend.position = "none")

plot5

