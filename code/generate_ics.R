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
#' @return A character string containing the iCalendar entry in plain text format.
#' @export
#' @examples
#' generate_ics("2022-12-27 15:00:00", 60, "America/New_York", "Meeting", "Conference Room")
#' generate_ics("2022-12-27 15:00:00", 60, "America/New_York", "Meeting", "Conference Room", "FREQ=DAILY;COUNT=5")
#'
generate_ics <- function(start_datetime, duration, time_zone, title, location, recurrence_rule = NULL) {

  # Validation
  # Check that duration is a positive numeric value
  if (!is.numeric(duration) || !duration>0) {
    stop("Error: duration must be a positive numeric value")
  }

  # Check that time_zone is a valid time zone identifier
  if (!time_zone %in% OlsonNames()) {
    stop("Error: time_zone must be a valid time zone identifier")
  }

  # Convert time zone to tz object
  tz <- tz(time_zone)

  # Convert start time to POSIXct object
  # start_time <- as.POSIXct(start_datetime, tz = tz)

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
    ics <- sprintf(
      ics_template,
      format(start_time, "%Y%m%dT%H%M%S"),
      format(end_time, "%Y%m%dT%H%M%S"),
      tz,
      title,
      location,
      ""
    )
  } else {
    ics <- sprintf(
      ics_template,
      format(start_time, "%Y%m%dT%H%M%S"),
      format(end_time, "%Y%m%dT%H%M%S"),
      tz,
      title,
      location,
      paste0("RRULE:", recurrence_rule)
    )
  }

  return(ics)
}
