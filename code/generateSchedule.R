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

#Date for next monday
next_monday <- data.frame(date = seq(from = today(), by = "day", length.out = 7)) %>%
  mutate(Day = wday(date, label = TRUE, abbr = FALSE)) %>%
  filter(Day == "Monday") %>%
  pull(date)

# Create a data frame with the day of the week and date
next_work_week <- data.frame(date = seq(from = next_monday, by = "day", length.out = 5)) %>%
  mutate(Day = wday(date, label = TRUE, abbr = FALSE))

events <- left_join(next_work_week,events) %>% 
  mutate(date_time_t = parse_date_time(Start.Time,'%I:%M %p')) %>% 
  mutate(year = year(date), month = month(date), day = day(date)) %>% 
  mutate(date_time = update(date_time_t,year = year, month = month, day = day)) %>% 
  select(date_time, Duration, Activity)

head(events)

library(stringr)

parse_duration_string <- function(duration_string) {
  # Extract the numeric value from the text string
  duration_minutes <- as.numeric(str_extract(duration_string, "\\d+"))
  
  # Check for the presence of specific time units in the text string
  if (str_detect(duration_string, "hour")) {
    # Multiply the numeric value by 60 to convert it to minutes
    duration_minutes <- duration_minutes * 60
  } else if (str_detect(duration_string, "minutes")) {
    # Leave the numeric value as is
  } else {
    # Set the numeric value to 0 if no time units are detected
    duration_minutes <- 0
  }
  
  # Return the numeric value in minutes
  return(duration_minutes)
}

events$duration_mins <- sapply(events$Duration, parse_duration_string)
events
  


# Open file for writing
file <- file("deepWorkSchedule.ics", "w")

# Loop through events
for (i in 1:nrow(events)) {
  # Generate iCalendar entry for event
  ics <- generate_ics(start_datetime = events$date_time[i], 
                      duration = events$duration_mins[i],  
                      time_zone = "Europe/Copenhagen", 
                      title = events$Activity[i], 
                      location = "NA", recurrence_rule = NULL)

  # Write iCalendar entry to file
  writeLines(ics, file)
}

# Close file
close(file)
