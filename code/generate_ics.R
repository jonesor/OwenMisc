#' Generate iCalendar Entry
#'
#' Generates an iCalendar entry in plain text format for an event with the specified start time, duration, time zone, title, location, and optional recurrence rule.
#'
#' @param start_datetime A character string representing the start time of the event in the format "YYYY-MM-DD HH:MM:SS".
#' @param duration A numeric value representing duration in minutes.
#' @param time_zone A character string representing the time zone of the event.
#' @param title A character string representing the title of the event.
#' @param location A character string representing the location of the event.
#' @param recurrence_rule A character string representing the recurrence rule for the event in the format defined by the iCalendar specification.
#' @param freebusy A character string of FREE or BUSY indicating if the event should be indicated as Free/Busy in Outlook.
#' @return A character string containing the iCalendar entry in plain text format.
#' 
#' @importFrom TeachingDemos char2seed
#' @importFrom lubridate tz
#' @export
#' @examples
#' generate_ics("2022-12-27 15:00:00", 60, "America/New_York", "Meeting", "Conference Room")
#' generate_ics("2022-12-27 15:00:00", 60, "America/New_York", "Meeting", "Conference Room", "FREQ=DAILY;COUNT=5")
#'
generate_ics <- function(start_datetime, duration, time_zone, title, location, recurrence_rule = NULL, freebusy = "BUSY") {
  
  ics_random_seed_text <- paste0(start_datetime, duration, time_zone, title, location, ifelse(is.null(recurrence_rule),"NULL_value",recurrence_rule), freebusy)
  ics_random_seed <- char2seed(ics_random_seed_text, set = FALSE)
  set.seed(ics_random_seed)
  
  # Validation
  # Check that duration is a positive numeric value
  if (!is.numeric(duration) || !duration > 0) {
    stop("Error: duration must be a positive numeric value")
  }
  
  # Check that time_zone is a valid time zone identifier
  if (!time_zone %in% OlsonNames()) {
    stop("Error: time_zone must be a valid time zone identifier")
  }
  
  # Convert time zone to tz object
  tz <- tz(time_zone)
  
  # Check that start_datetime is a character string in the correct format
  start_time <- strptime(start_datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if (is.na(start_time)) {
    stop("Error: start_datetime must be a character string in the format YYYY-MM-DD HH:MM:SS")
  }
  
  # Convert duration to seconds
  duration_secs <- duration * 60
  
  # Calculate end time
  end_time <- start_time + duration_secs
  
  # Define iCalendar template
  ics_template <- "BEGIN:VCALENDAR
PRODID:-//ATFutures/ical //EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:PUBLISH
BEGIN:VEVENT
UID:ical-%s
DTSTART:%s
DTEND:%s
TZID:%s
SUMMARY:%s
LOCATION:%s
%s
X-MICROSOFT-CDO-BUSYSTATUS:%s
END:VEVENT

END:VCALENDAR"
  
  # Fill in template with input values
  if (is.null(recurrence_rule)) {
    ics <- sprintf(
      ics_template,
      paste(sample(x = c(letters, 0:9, rep("-", 2)), replace = FALSE, size = 25), collapse = ""),
      format(start_time, "%Y%m%dT%H%M%S"),
      format(end_time, "%Y%m%dT%H%M%S"),
      tz,
      title,
      location,
      "",
      freebusy
    )
  } else {
    ics <- sprintf(
      ics_template,
      format(start_time, "%Y%m%dT%H%M%S"),
      format(end_time, "%Y%m%dT%H%M%S"),
      tz,
      title,
      location,
      paste0("RRULE:", recurrence_rule),
      freebusy
    )
  }
  
  return(ics)
}

