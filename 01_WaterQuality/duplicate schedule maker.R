# Set seed for reproducibility (set to year)
set.seed(2026)

# Define the months and number of sampling days per month
months <- month.name
sampling_days <- 8

# Station groups for each sampling day (adjust as needed)
stations_by_day <- list(
  VR = c("C3A", "C9", "C10A"),
  MD1 = c("D12", "D19", "D28A"),
  MD2 = c("D16", "D26", "MD10A", "P8"),
  SR = c("D4", "D22", "NZ068"),
  SB = c("D8", "D10", "D6"),
  GB = c("D7", "NZS42", "NZ032"),
  SP = c("D41A", "D41"),
  DO = c("Light 6", "Light 28", "Light 41", "Turning Basin")
)

# optional san pablo stations: "NZ325", "NZ002", "NZ004"

# Randomly select a duplicate station for each day
select_duplicates <- function(station_groups) {
  sapply(station_groups, function(stations) sample(stations, 1))
}

# Create the schedule for the entire year
schedule <- do.call(rbind, lapply(months, function(month) {
  duplicates <- select_duplicates(stations_by_day)
  data.frame(
    Month = month,
    Day = names(stations_by_day),
    Duplicate_Station = duplicates
  )
}))

# Print the schedule
print(schedule)

# Save to CSV (optional)
#write.csv(schedule, "yearly_duplicate_schedule.csv", row.names = FALSE)
