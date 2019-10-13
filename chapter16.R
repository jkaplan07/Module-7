####Chapter 16: Dates and Times ----
#this chapter explains how to work with dates and times in R.
#dates and times are hard because they reconcile 2 physical phenomena (rotation or the Earth adn its orbit around the sun) with other geopolitical phenomena including months, time zones, and DST.

#dates and times use lubridate package.
library(tidyverse)
#install.packages("lubridate")
library(lubridate)
library(nycflights13)

#creating dates and times
 #3 types of date/time data that refer to an instant in time:
  #1) date: tibbles print this as <date>
  #2) time within a day: tibbles print this as <time>
  #3) date-time, which is a date plus a time: unique identifies an instant in type (typically to the nearest second).  tibbles pritn this as <dttm>.  Elsewhere in R these are call POSIXct.

#hms package: stores times.

#you should always use the simplest possible date type that works with your needs.  Date-tiems are substantially more complicated because of the need to handle time zones.

#to get current date or date=time, use today() or now()

today()
now()

#3 ways to create a date/time: from a string, from individual date-time components, from an existing date/time object.

####16.2.1: From strings----
#date/time data often come as strings.  Some functions in lubridate automatically work out the fomrat once you specify hte order of the component.  To use them, identify order in which year, month, day appear in the dates, then arrange in "y", "m", "day" in same order.

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

#the functions also take unquoted numbers: most concise way to create a single date/time object.
ymd(20170131)

#ymd and friends create dates.  To create date-time, add an underscore and one or more "h", "m", and "s" to the naem of the parsing function.

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

#you can also force creation of date-time from a date by supplying a timezone:
ymd(20170131, tz = "UTC")

#From individual components
#instead of a single string, sometimes you'll have individual components of date-time spread across multiple columns. Ex.:
flights %>%
  select(year, month, day, hour, minute)
#to create date/time from this sort of input, use make_date() for dates, or make_datetime() for datetimes:

flights %>%
  select(year, month, day, hour, minute) %>%
  mutate(departure = make_datetime(year, month, day, hour, minute))

#in flights, times are represented in wierd format so we need to use moduls arithmetic to pull out the hour and minute components.

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

#this data allows to visualize distribution of departure times across the year:
flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

#visualize distribution of departure times across a single day:
flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

#When you use date-time in a numeric context (i.e. histogram), 1 means 1 second, so a binwidth of 86400=1 day.  For dates, 1 means 1 day.

#from other types
#as_datetime() is used when you want to switch between a date-time and a date.

as_datetime(today())
#> [1] "2019-01-08 UTC"
as_date(now())
#> [1] "2019-01-08"

#if you get date/times as numeric offsets from "Unix Epoch", 1970-01-01.  If ofset is in seconds, use asdatetime().  if in dates use as_date()

as_datetime(60 * 60 * 10)
#> [1] "1970-01-01 10:00:00 UTC"
as_date(365 * 10 + 2)
#> [1] "1980-01-01"

####16.2.4 Exercises ----
#1. What happens if you parse a string that contains invalid dates?
ymd(c("2010-10-10", "bananas"))
ret <- ymd(c("2010-10-10", "bananas"))
#> Warning: 1 failed to parse.
print(class(ret))
#> [1] "Date"
ret
#> [1] "2010-10-10" NA

#2. What does the tzone argument to today() do? Why is it important?
 #tzone: a character vector specifying which time zone you would like to find the current date of. tzone defaults to the system time zone set on your computer.  Since different time-zones can have different dates, the value of today() can vary depending on the time-zone specified.
#3. Use the appropriate lubridate function to parse each of the following dates:
 d1 <- mdy("January 1, 2010")
 d2 <- ymd("2015-Mar-07")
 d3 <- dmy("06-Jun-2017")
 d4 <- c("August 19 (2015)", "July 1 (2015)")
  mdy(d4)
 d5 <- "12/30/14" # Dec 30, 2014
  mdy(d5)
  
####16.3: Getting components
#you can pull out parts of the date with accessor functions - year(), month(), mday() (day of the month), yday() (day of the year), wday() (day of the week), hour(), minute(), and second()
  
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

#month() and wday() you can set label = TRUE to return abbreviated name of month or day of the week.  abbr = FALSE returns full name.

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

#can use wday() to see how more flights depart during week than on the weekend.
flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  ggplot(aes(x = wday)) +
  geom_bar()

#there is a pattern if we look at avg departure delay by minute within hour.  Looks like flights leaving in minutes 20-30 and 50-60 have lower delays than the rest of the hour.
flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()

#if we look at schedule depature you don't see such a strong pattern.  Always be alert this type of pattern when you work with date that involves human judgement

sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

#Rounding###
#an alternative approach to plotting individual components is to round the date to a nearby unit of time with floor_date(), round_date(), and ceiling_date. Each takes a vector of dates to adjust and then name unit round down (floor), round up (ceilng) or round to.

#plot number of flights per week:
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line()

#Setting Components
#can use each accessor function to set the components of a date/time

(datetime <- ymd_hms("2016-07-08 12:34:56"))
#> [1] "2016-07-08 12:34:56 UTC"
year(datetime) <- 2020
datetime
#> [1] "2020-07-08 12:34:56 UTC"
month(datetime) <- 01
datetime
#> [1] "2020-01-08 12:34:56 UTC"
hour(datetime) <- hour(datetime) + 1
datetime
#> [1] "2020-01-08 13:34:56 UTC"

#rather than modifying in plac,e you can create new date-time with update().  You can do mulitple values at once.

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

#If values too bigt they will roll-over.

ymd("2015-02-01") %>%
  update(mday = 30)
#> [1] "2015-03-02"
ymd("2015-02-01") %>%
  update(hour = 400)
#> [1] "2015-02-17 16:00:00 UTC"

#you can use update() to show distribution of flights across course of the day for eveyr day of the year:

flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

#setting larger components of a date to a constant allows you to explore patterns in the smaller components.

####16.3.4 Exercises----
#1. How does the distribution of flight times within a day change over the course of the year?
#plot by month:
flights_dt %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(binwidth = 60 * 60)
#looks better when normalized within groups:
flights_dt %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(aes(y = ..density..), binwidth = 60 * 60)
# there doesn’t appear to much difference in within-day distribution over the year

#2. Compare dep_time , sched_dep_time and dep_delay . Are they consistent? Explain your findings.
 #If they are consistent, then dep_time = sched_dep_time + dep_delay
flights_dt %>%
  mutate(dep_time_ = sched_dep_time + dep_delay * 60) %>%
  filter(dep_time_ != dep_time) %>%
  select(dep_time_, dep_time, sched_dep_time, dep_delay)

#3. Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)
flights_dt %>%
  mutate(
    flight_duration = as.numeric(arr_time - dep_time),
    air_time_mins = air_time,
    diff = flight_duration - air_time_mins
  ) %>%
  select(origin, dest, flight_duration, air_time_mins, diff)

#4. How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time ? Why?
flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()

#5. On what day of the week should you leave if you want to minimise the chance of a delay?
#Saturday has the lowest average departure delay time and the lowest average arrival delay time.
flights_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)

flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  group_by(wday) %>%
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wday, y = ave_dep_delay)) +
  geom_bar(stat = "identity")

flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  group_by(wday) %>%
  summarize(ave_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wday, y = ave_arr_delay)) +
  geom_bar(stat = "identity")

#6. What makes the distribution of diamonds$carat and flights$sched_dep_time similar?
ggplot(diamonds, aes(x = carat)) +
  geom_density()
#In both carat and sched_dep_time there are abnormally large numbers of values are at nice “human” numbers. In sched_dep_time it is at 00 and 30 minutes. In carats, it is at 0, 1/3, 1/2, 2/3,

ggplot(diamonds, aes(x = carat %% 1 * 100)) +
  geom_histogram(binwidth = 1)

ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)

#7. Confirm my hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.

flights_dt %>%
  mutate(
    minute = minute(dep_time),
    early = dep_delay < 0
  ) %>%
  group_by(minute) %>%
  summarise(
    early = mean(early, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(minute, early)) +
  geom_line()

####16.4 Time Spans ----
#how to do arithmetic with dates including subtraction, addition, and division.
 #durations: represent an exact number of seconds
 #periods: represent human units like weeks and months
 #intervals: represent a starting and ending point.

#Durations
#when you subtract 2 dates, you get a difftime object:
# How old is Hadley?
h_age <- today() - ymd(19791014)
h_age
#> Time difference of 14331 days

#duration:
as.duration(h_age)

#constructors for duration:
dseconds(15)
#> [1] "15s"
dminutes(10)
#> [1] "600s (~10 minutes)"
dhours(c(12, 24))
#> [1] "43200s (~12 hours)" "86400s (~1 days)"
ddays(0:5)
#> [1] "0s" "86400s (~1 days)" "172800s (~2 days)"
#> [4] "259200s (~3 days)" "345600s (~4 days)" "432000s (~5 days)"
dweeks(3)
#> [1] "1814400s (~3 weeks)"
dyears(1)
#> [1] "31536000s (~52.14 weeks)"

#durations always record time span in seconds.  larger unites are create by converting larger units into seconds at a standard rate (ex. to seconds in a minute).  you can add and mulitply durations:
2 * dyears(1)
#> [1] "63072000s (~2 years)"
dyears(1) + dweeks(12) + dhours(15)
#> [1] "38847600s (~1.23 years)"

#add and subract durations to and from days:
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

#because durations represent an exact number of seconds, sometimes you might get an unexpected result:

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
#> [1] "2016-03-12 13:00:00 EST"
one_pm + ddays(1)
#> [1] "2016-03-13 14:00:00 EDT"

#Periods
#perods: time spans but ond't have fixed length in seconds, they work in "human" times, like days and months.  They work more intuitively
one_pm
#> [1] "2016-03-12 13:00:00 EST"
one_pm + days(1)
#> [1] "2016-03-13 13:00:00 EDT"

#periods can be created with a # of friendly constructor functions.
seconds(15)
#> [1] "15S"
minutes(10)
#> [1] "10M 0S"
hours(c(12, 24))
#> [1] "12H 0M 0S" "24H 0M 0S"
days(7)
#> [1] "7d 0H 0M 0S"
months(1:6)
#> [1] "1m 0d 0H 0M 0S" "2m 0d 0H 0M 0S" "3m 0d 0H 0M 0S" "4m 0d 0H 0M 0S"
#> [5] "5m 0d 0H 0M 0S" "6m 0d 0H 0M 0S"
weeks(3)
#> [1] "21d 0H 0M 0S"
years(1)
#> [1] "1y 0m 0d 0H 0M 0S"

#add and multiply periods:
10 * (months(6) + days(1))
#> [1] "60m 10d 0H 0M 0S"
days(50) + hours(25) + minutes(2)
#> [1] "50d 25H 2M 0S"

# A leap year
ymd("2016-01-01") + dyears(1)
#> [1] "2016-12-31"
ymd("2016-01-01") + years(1)
#> [1] "2017-01-01"
# Daylight Savings Time
one_pm + ddays(1)
#> [1] "2016-03-13 14:00:00 EDT"
one_pm + days(1)
#> [1] "2016-03-13 13:00:00 EDT"

#some planes seem to appear before they departed NYC:
flights_dt %>%
  filter(arr_time < dep_time)

flights_dt <- flights_dt %>%
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt %>%
  filter(overnight, arr_time < dep_time)

#Intervals:
#An interval is a duration with a starting point: that makes it precise so you can determine exactly how long it is:
  next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
  
#to find out how many periods fall into an interval, you need to use integer division:
  (today() %--% next_year) %/% days(1)
  #> Note: method with signature 'Timespan#Timespan' chosen for function '%/%',
  #> target signature 'Interval#Period'.
  #> "Interval#ANY", "ANY#Period" would also be valid
  #> [1] 365
  
#Use the simplest data structure that solves your problem.  If you only care about physical time, use a duration, if you need to add human times, use a period: if you need to figure out how long a span is in human units, use an interval.
  
###16.4.5 Exercises:
  
#1. Why is there months() but no months() ?
    
#There is no direct unambiguous value of months in seconds since months have differing numbers of days.
  
 #31 days: January, March, May, July, August, October 
  #30 days: April, June, September, November, December
 #28 or 29 days: February
#The month is not a duration of time defined independently of when it occurs, but a special interval between two dates.
  
#2. Explain days(overnight * 1) to someone who has just started learning R. How does it work?
    
#The variable overnight is equal to TRUE or FALSE. If it is an overnight flight, this becomes 1 day, and if not, then overnight = 0, and no days are added to the date.
  
#3. Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.
  #A vector of the first day of the month for every month in 2015:
    ymd("2015-01-01") + months(0:11)
  #>  [1] "2015-01-01" "2015-02-01" "2015-03-01" "2015-04-01" "2015-05-01"
  #>  [6] "2015-06-01" "2015-07-01" "2015-08-01" "2015-09-01" "2015-10-01"
  #> [11] "2015-11-01" "2015-12-01"
  
floor_date(today(), unit = "year") + months(0:11)
    #>  [1] "2019-01-01" "2019-02-01" "2019-03-01" "2019-04-01" "2019-05-01"
    #>  [6] "2019-06-01" "2019-07-01" "2019-08-01" "2019-09-01" "2019-10-01"
    #> [11] "2019-11-01" "2019-12-01"

#4. Write a function that given your birthday (as a date), returns how old you are in years.
age <- function(bday) {
  (bday %--% today()) %/% years(1)
}
age(ymd("1990-10-12"))
#> Note: method with signature 'Timespan#Timespan' chosen for function '%/%',
#>  target signature 'Interval#Period'.
#>  "Interval#ANY", "ANY#Period" would also be valid
#> [1] 28

#5. Why can’t (today() %--% (today() + years(1)) / months(1) work?  
#missing a parentheses:
(today() %--% (today() + years(1))) / months(1)

#### Time Zones ----
#very complicated.
#first challenge is that everyday names of time zones are ambiguous. EST in America is Eastern Standard Time, but Australia and Canada also have EST.  R uses international standard IANA time zones.

#what is current time zone?
Sys.timezone()
#> [1] "UTC"

#to see a complete list of time zones: OlsonNames()
OlsonNames()
length(OlsonNames())
#> [1] 606
head(OlsonNames())
#> [1] "Africa/Abidjan" "Africa/Accra" "Africa/Addis_Ababa"
#> [4] "Africa/Algiers" "Africa/Asmara" "Africa/Asmera"

#time zone is an attribute of the date-time that only controls printing. These three objects represent same instant in time:
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
#> [1] "2015-06-01 12:00:00 EDT"
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
#> [1] "2015-06-01 18:00:00 CEST"
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
#> [1] "2015-06-02 04:00:00 NZST"

#verify that they are the same using subtraction:
x1 - x2
#> Time difference of 0 secs
x1 - x3
#> Time difference of 0 secs

#default for lubridate: UTC.  It does not have DST, which mkaes a convenient representation for computation.

x4 <- c(x1, x2, x3)
x4
#> [1] "2015-06-01 12:00:00 EDT" "2015-06-01 12:00:00 EDT"
#> [3] "2015-06-01 12:00:00 EDT"

#How to change time zones:
#1) Keep the instant in time the same, and change how it’s displayed. Use this when the instant is correct, but you want a more natural display.
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
#> [1] "2015-06-02 02:30:00 +1030" "2015-06-02 02:30:00 +1030"
#> [3] "2015-06-02 02:30:00 +1030"
x4a - x4
#> Time differences in secs
#> [1] 0 0 0

#2) Change underlying instant in time.  Use this when you have an instant that has been labelled with incorrect time zone and you need to fix it:
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
#> [1] "2015-06-01 12:00:00 +1030" "2015-06-01 12:00:00 +1030"
#> [3] "2015-06-01 12:00:00 +1030"
x4b - x4
#> Time differences in hours
#> [1] -14.5 -14.5 -14.5
