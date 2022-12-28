# Multiple dates

# This script uses the generate_ics function to create a single text file
# containing iCalendar entries for multiple events. It loops through the rows of
# the data frame and generates an iCalendar entry for each event using the
# generate_ics function. The entries are then written to a file using the
# writeLines function.

# Load lubridate library
library(lubridate)

# Load data frame with event information
events <- read.csv("data/deepworkSchedule.csv")
events

# Date for next monday
next_monday <- data.frame(date = seq(from = today(), by = "day", length.out = 7)) %>%
  mutate(Day = wday(date, label = TRUE, abbr = FALSE)) %>%
  filter(Day == "Monday") %>%
  pull(date)

# Create a data frame with the day of the week and date
next_work_week <- data.frame(date = seq(from = next_monday, by = "day", length.out = 5)) %>%
  mutate(Day = wday(date, label = TRUE, abbr = FALSE))

events <- left_join(next_work_week, events) %>%
  mutate(date_time_t = parse_date_time(Start.Time, "%I:%M %p")) %>%
  mutate(year = year(date), month = month(date), day = day(date)) %>%
  mutate(date_time = update(date_time_t, year = year, month = month, day = day)) %>%
  select(date_time, Duration, Activity)

head(events)

# Open file for writing
file <- file("deepWorkSchedule.ics", "w")

# Loop through events and write each event out.
for (i in 1:nrow(events)) {
  # Generate iCalendar entry for event
  ics <- generate_ics(
    start_datetime = events$date_time[i],
    duration = events$Duration[i],
    time_zone = "Europe/Copenhagen",
    title = events$Activity[i],
    location = "Office", recurrence_rule = NULL
  )

  # Write iCalendar entry to file
  writeLines(ics, file)
}

# Close file
close(file)

# Now I will remove the unnecessary VCALENDAR components and ensure that there is
# a single "BEGIN..." and "END..." component wrapping all the individual events.

# Read in the ics file
ics_data <- readLines("deepWorkSchedule.ics")

# Remove the unnecessary VCALENDAR components
ics_data <- ics_data[!grepl("BEGIN:VCALENDAR", ics_data)]
ics_data <- ics_data[!grepl("END:VCALENDAR", ics_data)]

# Add a single VCALENDAR component at the beginning of the file
ics_data <- c("BEGIN:VCALENDAR", ics_data)

# Add a single VCALENDAR component at the end of the file
ics_data <- c(ics_data, "END:VCALENDAR")

# Write the modified ics data to a new file
writeLines(ics_data, "deepWorkSchedule.ics")
