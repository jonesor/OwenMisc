# Multiple dates

# This script uses the generate_ics function to create a single text file
# containing iCalendar entries for multiple events. It loops through the rows of
# the data frame and generates an iCalendar entry for each event using the
# generate_ics function. The entries are then written to a file using the
# writeLines function.

# Load lubridate library
library(lubridate)

# Load data frame with event information
events <- read.csv("events.csv")

# Open file for writing
file <- file("calendar.ics", "w")

# Loop through events
for (i in 1:nrow(events)) {
  # Generate iCalendar entry for event
  ics <- generate_ics(events$start_time[i], events$duration[i], events$time_zone[i], events$title[i], events$location[i], events$recurrence_rule[i])

  # Write iCalendar entry to file
  writeLines(ics, file)
}

# Close file
close(file)
