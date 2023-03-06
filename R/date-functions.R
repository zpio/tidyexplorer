#' Date Functions
#'
#' Perform various operations on date objects.
#'
#' @param date A vector of dates.
#' @param start_date A vector of dates.
#' @param end_date A vector of dates.
#' @param date_part A string indicating the part of the date to consider: 'day', 'month', 'year', 'week', 'quarter', 'hour', 'minute', 'second'.
#' @param interval A number interval added to the specified date_part of that date.
#' @param date_format A string specifying the date format of a date string: 'ymd', 'ydm', 'mdy', etc.
#' @param date_string A character vector of dates.
#' @param week_start An integer indicating the week start day. Default is 1, Monday.
#' @param year A numeric year.
#' @param month A numeric month.
#' @param day A numeric day.
#' @param hour A numeric hour.
#' @param min A numeric minute.
#' @param sec A numeric second.
#' @param tz A string indicating the time zone. Defaults to UTC.
#'
#' @return A date or date-time object the length of x
#'
#' @name Date_Functions
#'
#' @examples
#'
#'
#' calendarAuto("2023-01-01", "2023-01-26")
#'
#' dateAdd("year", 1, as.Date("2023-01-31"))
#'
#' dateDiff("months", as.Date("2023-02-28"), as.Date("2023-01-01"))
#'
#' dateName('quarters', as.Date("2023-02-28"))
#'
#' dateParse("ymd_hms", "2023-02-28 20:30:08.0001 pm")
#'
#' datePart('month', as.Date("2023-02-28"))
#'
#' dateTrunc('month', as.Date("2023-12-24"))
#'
#' dateTrunc('quarter', as.Date("2023-12-24"))
#'
#' dateTrunc('month', lubridate::ymd_hms("2023-01-24 10:20:20 AM"))
#'
#' year(as.Date("2023-01-24"))
#'
#' month(as.Date("2023-01-24"))
#'
#' day(as.Date("2023-01-24"))
#'
#' quarter(as.Date("2023-01-24"))
#'
#' semester(as.Date("2023-01-24"))
#'
#' week(as.Date("2023-01-24"))
#'
#' dayOfWeek(as.Date("2023-01-24"))
#'
#' dayOfYear(as.Date("2023-12-31"))
#'
#' dayOfMonth(as.Date("2023-12-31"))
#'
#' isWorkingDay(as.Date("2023-01-01"))
#'
#' isDate(as.Date("2009-08-03"))
#'
#' makeDate(2023,01,01)
#'
#' makeDateTime(2023,01,01,12,50,10)
#'
#' now()
#'
#' today()
#'
#'
#'
#' @rdname date-functions
#' @export
calendarAuto <- function(start_date, end_date, week_start = 1){
  calendar <- data.frame(
    Date = seq.Date(
      from = lubridate::as_date(start_date),
      to   = lubridate::as_date(end_date),
      by   = "day"
    )
  ) %>%
    dplyr::mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date),
      Day = lubridate::day(Date),
      Quarter = lubridate::quarter(Date),
      Semester = lubridate::semester(Date),
      Week = lubridate::week(Date),
      dayOfWeek = lubridate::wday(Date, week_start = week_start),
      dayOfYear = lubridate::yday(Date),
      dayOfMonth = lubridate::mday(Date),
      Month_name = lubridate::month(Date, label = TRUE, abbr = FALSE),
      Quarter_name = paste0("Q",lubridate::quarter(Date)),
      dayOfWeek_name = lubridate::wday(Date, label = TRUE, abbr = FALSE),
      Is_Working_Day = dplyr::if_else((dayOfWeek == 6 | dayOfWeek == 7),FALSE,TRUE)
    )

  return(calendar)

}

#' @rdname date-functions
#' @export
dateAdd <- function(date_part, interval, date){

  x <- date

  if (date_part == "days" | date_part == "day"){

    date_add <- x + lubridate::days(interval)

  }else if(date_part == "months" | date_part == "month"){

    date_add <- lubridate::add_with_rollback(x, months(interval))

  }else if(date_part == "years" | date_part == "year"){

    date_add <- x + lubridate::years(interval)

  }else if(date_part == "weeks" | date_part == "week"){

    date_add <- x + lubridate::days(7*interval)

  }else if(date_part == "quarters" | date_part == "quarter"){

    date_add <- lubridate::add_with_rollback(x, months(3*interval))

  }else if(date_part == "hours" | date_part == "hour"){

    date_add <- x + lubridate::hours(interval)

  }else if(date_part == "minutes" | date_part == "minute"){

    date_add <- x + lubridate::minutes(interval)

  }else if(date_part == "seconds" | date_part == "second"){

    date_add <- x + lubridate::seconds(interval)

  }else {

    stop(call. = FALSE, "Error")

  }

  return(date_add)

}

#' @rdname date-functions
#' @export
dateDiff <- function(date_part, end_date, start_date){

  if (date_part == "months" | date_part == "month"){

    diff <- lubridate::interval(start_date, end_date) %/% months(1)

  } else if (date_part == "years" | date_part == "year"){

    diff <- lubridate::interval(start_date, end_date) %/% lubridate::years(1)

  } else if (date_part == "days" | date_part == "day"){

    diff <- lubridate::interval(start_date, end_date) %/% lubridate::days(1)

  } else {

    diff <- as.numeric(difftime(end_date, start_date, units=date_part))

  }

  return(diff)

}

#' @rdname date-functions
#' @export
dateName <- function(date_part, date){

  x <- date

  if (date_part == "days" | date_part == "day"){

    date_name <- lubridate::wday(x, label = TRUE, abbr = FALSE)

  }else if(date_part == "months" | date_part == "month"){

    date_name <- lubridate::month(x, label = TRUE, abbr = FALSE)

  }else if(date_part == "years" | date_part == "year"){

    date_name <- as.character(lubridate::year(x))

  }else if(date_part == "weeks" | date_part == "week"){

    date_name <- as.character(lubridate::week(x))

  }else if(date_part == "quarters" | date_part == "quarter"){

    date_name <- paste0("Q",lubridate::quarter(x))

  }else {

    stop(call. = FALSE, "Invalid date_part value")

  }

  return(date_name)

}

#' @rdname date-functions
#' @export
dateParse <- function(date_format, date_string) {

  x <- date_string

  switch( date_format,
          ymd = lubridate::ymd(x),
          ydm = lubridate::ydm(x),
          mdy = lubridate::mdy(x),
          myd = lubridate::myd(x),
          dmy = lubridate::dmy(x),
          dym = lubridate::dym(x),
          yq = lubridate::yq(x),
          ym = lubridate::ym(x),
          my = lubridate::my(x),
          ymd_hms = lubridate::ymd_hms(x),
          ymd_hm = lubridate::ymd_hm(x),
          ymd_h = lubridate::ymd_h(x),
          dmy_hms = lubridate::dmy_hms(x),
          dmy_hm = lubridate::dmy_hm(x),
          dmy_h = lubridate::dmy_h(x),
          mdy_hms = lubridate::mdy_hms(x),
          mdy_hm = lubridate::mdy_hm(x),
          mdy_h = lubridate::mdy_h(x),
          ydm_hms = lubridate::ydm_hms(x),
          ydm_hm = lubridate::ydm_hm(x),
          ydm_h = lubridate::ydm_h(x),
          stop("Invalid date_format value")
  )
}

#' @rdname date-functions
#' @export
datePart <- function(date_part, date){

  x <- date

  if (date_part == "days" | date_part == "day"){

    date_name <- lubridate::day(x)

  }else if(date_part == "months" | date_part == "month"){

    date_name <- lubridate::month(x)

  }else if(date_part == "years" | date_part == "year"){

    date_name <- lubridate::year(x)

  }else if(date_part == "weeks" | date_part == "week"){

    date_name <- lubridate::week(x)

  }else if(date_part == "quarters" | date_part == "quarter"){

    date_name <- lubridate::quarter(x)

  }else {

    stop(call. = FALSE, "Invalid date_part value")

  }

  return(date_name)

}

#' @rdname date-functions
#' @export
dateTrunc <- function(date_part, date, week_start=1){

  x <- date

  date_trunc <- lubridate::floor_date(
    x,
    unit = date_part,
    week_start = week_start
  )

  return(date_trunc)

}

#' @rdname date-functions
#' @export
year <- function(date){

  x <- date

  year <- lubridate::year(x)

  return(year)
}

#' @rdname date-functions
#' @export
month <- function(date){

  x <- date

  month <- lubridate::month(x)

  return(month)
}

#' @rdname date-functions
#' @export
day <- function(date){

  x <- date

  day <- lubridate::day(x)

  return(day)
}

#' @rdname date-functions
#' @export
quarter <- function(date){

  x <- date

  quarter <- lubridate::quarter(x)

  return(quarter)
}

#' @rdname date-functions
#' @export
semester <- function(date){

  x <- date

  semester <- lubridate::semester(x)

  return(semester)
}

#' @rdname date-functions
#' @export
week <- function(date){

  x <- date

  week <- lubridate::week(x)

  return(week)
}

#' @rdname date-functions
#' @export
dayOfWeek <- function(date, week_start=1){

  x <- date

  weekday <- lubridate::wday(x, week_start = week_start)

  return(weekday)
}


#' @rdname date-functions
#' @export
dayOfYear <- function(date){

  x <- date

  yday <- lubridate::yday(x)

  return(yday)
}

#' @rdname date-functions
#' @export
dayOfMonth <- function(date){

  x <- date

  mday <- lubridate::mday(x)

  return(mday)
}

#' @rdname date-functions
#' @export
isWorkingDay <- function(date, week_start = 1){

  x <- date

  dayOfWeek <- lubridate::wday(x, week_start = week_start)

  Is_Working_Day = dplyr::if_else((dayOfWeek == 6 | dayOfWeek == 7),FALSE,TRUE)

  return(Is_Working_Day)
}

#' @rdname date-functions
#' @export
isDate <- function(date){

  x <- date

  id_date <- lubridate::is.Date(date)

  return(id_date)
}

#' @rdname date-functions
#' @export
makeDate <- function(year, month, day){

  make_date <- lubridate::make_date(year = year, month = month, day = day)

  return(make_date)

}

#' @rdname date-functions
#' @export
makeDateTime <- function(year, month, day, hour, min, sec, tz = "UTC"){

  make_date_time <- lubridate::make_datetime(
    year = year, month = month, day = day, hour = hour,
    min = min, sec = sec, tz = tz
  )

  return(make_date_time)

}

#' @rdname date-functions
#' @export
now <- function(tz = ''){
  lubridate::now(tzone = tz)
}

#' @rdname date-functions
#' @export
today <- function(tz = ''){
  lubridate::today(tzone = tz)
}
























