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

# Question:
  # How does the average quali lap time 
  # compare to the average race lap
  # for each track on the calendar?

data <- races %>%
  select(raceId, year, name, circuitId)

t_zero <- strptime("00:00", format = "%M:%OS")

fastest_race_laps <- results %>%
  mutate(seconds = strptime(fastestLapTime, format = "%M:%OS") - t_zero,
         ms = as.numeric(seconds * 1000)) %>%
    drop_na() %>%
      group_by(raceId) %>%
        summarise(fastestLapTime = min(ms)) %>%
      ungroup()
  
data <- merge(data, fastest_race_laps, by = "raceId")

fastest_quali_laps <- quali %>%
  mutate(q3Seconds = strptime(q3, format = "%M:%OS") - t_zero,
         ms = as.numeric(q3Seconds * 1000)) %>%
    drop_na() %>%
      group_by(raceId) %>%
        summarise(poleTime = min(ms)) %>%
      ungroup()

data <- merge(data, fastest_quali_laps, by = "raceId")

# Remove data points where the fastest race lap was faster than the
# pole lap, as these were probably rain affected races
data <- data %>%
  mutate(relativeTime = (fastestLapTime / poleTime) * 100) %>%
    filter(relativeTime > 100)

circuit_aves <- data %>%
  group_by(circuitId) %>%
    summarise(aveRelativeTime = mean(relativeTime)) %>%
  ungroup()

data <- merge(data, circuit_aves, by = "circuitId")

data <- merge(data, select(circ, circuitId, circuitRef), by = "circuitId")
      
rel_time_by_circuit <- ggplot(data, 
    aes(x = reorder(circuitRef, aveRelativeTime), 
        y = relativeTime, 
        fill = aveRelativeTime)) + 
  geom_boxplot() +
  scale_fill_gradient(low = "grey", high = "red") + 
  coord_flip(ylim=c(98,110)) +
  labs(x = "Circuit Name",
       y = "Fastest Race Lap as % of Pole Lap Time",
       title = "Relative Race Pace by Circuit") +
  theme(legend.position = "none")

rel_time_by_circuit




