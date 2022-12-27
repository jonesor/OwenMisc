#' Generate iCalendar Entry
#'
#' Generates an iCalendar entry in plain text format for an event with the specified start time, duration, time zone, title, location, and optional recurrence rule.
#'
#' @param start_time A character string representing the start time of the event in the format "YYYY-MM-DD HH:MM:SS".
#' @param duration A character string representing the duration of the event in the format "HH:MM:SS".
#' @param time_zone A character string representing the time zone of the event.
#' @param title A character string representing the title of the event.
#' @param location A character string representing the location of the event.
#' @param recurrence_rule A character string representing the recurrence rule for the event in the format defined by the iCalendar specification.
#' @return A character string containing the iCalendar entry in plain text format.
#' @export
#' @examples
#' generate_ics("2022-12-27 15:00:00", "1 hour", "America/New_York", "Meeting", "Conference Room")
#' generate_ics("2022-12-27 15:00:00", "1 hour", "America/New_York", "Meeting", "Conference Room", "FREQ=DAILY;COUNT=5")

generate_ics <- function(start_time, duration, time_zone, title, location, recurrence_rule = NULL) {
  require(lubridate)
    # Check that start time is a valid date-time string
  if (!is.POSIXct(as.POSIXct(start_time, tz = time_zone))) {
    stop("Invalid start time")
  }
  
  # Check that duration is a valid duration string
  if (!is.difftime(as.difftime(duration))) {
    stop("Invalid duration")
  }
  
  # Convert time zone to tz object
  tz <- tz(time_zone)
  
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
