#' Generate iCalendar Entry
#'
#' Generates an iCalendar entry in plain text format for an event with the specified start time, duration, time zone, title, location, and optional recurrence rule.
#'
#' @param uid_seed a seed for the unique id generation
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
generate_ics <- function(uid_seed = NULL, start_datetime, duration, time_zone, title, location, recurrence_rule = NULL, freebusy = "BUSY") {
  
  if(is.null(uid_seed)){
    set.seed(as.numeric(Sys.time()))
    }else{set.seed(uid_seed)}
  
  # Validation
  if (is.null(uid_seed)) {
    warning("A NULL uid will produce identical uids when multiple ics are generated rapidly.")
  }
  
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
PRODID:-//jonesor/ical //EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:PUBLISH
BEGIN:VEVENT
UID:%s
DTSTART:%s
DTEND:%s
TZID:%s
SUMMARY:%s
LOCATION:%s
%s
BEGIN:VALARM
TRIGGER:-PT5M
ACTION:DISPLAY
DESCRIPTION:Reminder
END:VALARM
X-MICROSOFT-CDO-BUSYSTATUS:%s
END:VEVENT
\n\n
END:VCALENDAR"
  
  myUUID_a <- paste(sample(x = c(letters, 0:9), replace = FALSE, size = 8),collapse = "")
  myUUID_b <- paste(sample(x = c(letters, 0:9), replace = FALSE, size = 4),collapse = "")
  myUUID_c <- paste(sample(x = c(letters, 0:9), replace = FALSE, size = 4),collapse = "")
  myUUID_d <- paste(sample(x = c(letters, 0:9), replace = FALSE, size = 4),collapse = "")
  myUUID_e <- paste(sample(x = c(letters, 0:9), replace = FALSE, size = 12),collapse = "")
  myUUID <- paste(myUUID_a,myUUID_b,myUUID_c,myUUID_d,myUUID_e,sep = "-")
  
  # Fill in template with input values
  if (is.null(recurrence_rule)) {
    ics <- sprintf(
      ics_template,
      myUUID,
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
      myUUID,
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

