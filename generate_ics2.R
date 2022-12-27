generate_ics <- function(start_date, start_time, duration, time_zone, title, location, recurrence_rule = NULL) {
  # Check that start date and time are valid
  if (!is.Date(as.Date(start_date))) {
    stop("Invalid start date")
  }
  if (!is.POSIXt(as.POSIXct(start_time, format = "%T"))) {
    stop("Invalid start time")
  }
  
  # Check that duration is a valid duration string
  if (!is.difftime(as.difftime(duration))) {
    stop("Invalid duration")
  }
  
  # Convert time zone to tz object
  tz <- tz(time_zone)
  
  # Combine date and time into single string
  start_time <- paste(start_date, start_time, sep = " ")
  
  # Convert start time to POSIXct object
  start_time <- as.POSIXct(start_time, tz = tz)
  
  # Convert duration to seconds
  duration_secs <- as.difftime(duration)
  
  # Calculate end time
  end_time <- start_time + duration_secs
  
  # Define iCalendar template
  ics_template <- "BEGIN:VCALENDAR
BEGIN:VEVENT
DTSTART:%s
DTEND:%s
TZID:%s
SUMMARY:%s
LOCATION:%s
%s
END:VEVENT
END:VCALENDAR"
  
  # Fill in template with input values
  if (is.null(recurrence_rule)) {
    ics <- sprintf(ics_template,
                   format(start_time, "%Y%m%dT%H%M%S"),
                   format(end_time, "%Y%m%dT%H%M%S"),
                   tz,
                   title,
                   location,
                   "")
  } else {
    ics <- sprintf(ics_template,
                   format(start_time, "%Y%m%dT%H%M%S"),
                   format(end_time, "%Y%m%dT%H%M%S"),
                   tz,
                   title,
                   location,
                   paste0("RRULE:", recurrence_rule))
  }
  
  return(ics)
}
