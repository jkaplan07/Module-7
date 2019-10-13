########################################################################################
# Summary: Module 7 - Working with factors and dates
# Date: October 2, 2019
########################################################################################

# Factors ----
# Load packages
library(tidyverse)

# Example tibble
d <- tibble(wkdays = c("MON", "TUES", "WED", "THURS", "FRI"),
            values = c(1, 3, 3.5, 4, 7))
#
d
ggplot(data = d, aes(x = wkdays, y = values)) +
  geom_bar(stat = "identity") #order of bars is in alphabetical order.

#change order by creating a factor and manipulating levels of the factor.

d$wkdays <- factor(x = d$wkdays) #have to reassign the vector.

#levels argument takes a vector with all the levels you want in your factor and you write htem in the order you want them in your plot. the levels can include values that are not actually present in the data.

d$wkdays <- factor(d$wkdays, levels = c("MON", "TUES", "WED", "THURS", "FRI", "SAT", "SUN")) 

d$wkdays

#levels have to be written the exact same way they are present in data.

d #how shows that it is a factor vector.
ggplot(data = d, aes(x = wkdays, y = values)) +
  geom_bar(stat = "identity")  #days are now in order of factor.

#if you want change the specific names of the levels.
d %>%
  mutate(wkdays_recode = fct_recode(wkdays, 
                                    "Monday" = "MON",
                                    "Tuesday" = "TUES"
                                    )) -> d

d

ggplot(data = d, aes(x = wkdays_recode, y = values)) +
  geom_bar(stat = "identity")

factor(d$values) 

# Dates and times ----
# Load packages
library(tidyverse)
library(lubridate)

# Example tibble
z <- tibble(date1 = c("12/01/2012", "12/02/2012", "12/03/2012"),
            date2 = c("2012/01/12", "2012/02/12", "2012/03/12"),
            date3 = c("01 Dec 2012", "02 Dec 2012", "03 Dec 2012"))

z #dates were read in as character vectors

#need to use parse functions to convert to date, time, or date/time vectors

#first column
mdy(z$date1) 
z$date1

z$date1 <-mdy(z$date1) #resave date
z

#3rd column
z$date3 <-dmy(z$date3) 
z

#2nd column
z$date2 <- ymd(z$date2)
z

#extract date elements
z %>%
  mutate(month = month(date3), 
         year = year(date3), 
         day = day(date2)) %>% #create new columns with month, year, day
  unite("new_date",year, month, day, sep = "-", remove = F) %>%#create year, month, date vector, use - as sepeartor.  dont remove previous columns
  mutate(new_date_parsed = ymd(new_date)) %>%
  mutate(day_fake = 1) %>%
  unite("new_date_f",year, month, day_fake, sep = "-", remove = F) ->z2
#always helpful to have these month, day and year columns
#unite allows you to combine month, day and year

#for data that is collected monthly, you may not have a day.  If you only have month/year you can't create a date vector in R.  In this case, if you only have info on month/year, you can create a fake column, and then create a new date with those fake date.

