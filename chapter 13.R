#Julia Kaplan
#Work through Chapter 13

#pull in R packages
library(tidyverse)
library(nycflights13)

#3.1 Introduction/Prerequisites ----
#relational data: mulitple tablesof data
#relations are always defined by a pair of tables 
#3 families of verbs designed to work with relational data
  #mutating joins: add new variables to one data frame from matching observations in another
  #filtering joins: filter observations from one data frame based on whether or not they match an observation in the other table
  #set operations: treat observations as if they were set remains
#most commons palce to find relational data is in a relational database management system
#use dplyr

#13.2 nycflights13----
#nycflights13 contains 4 tibbles related to flights table
 #1) airlines - full carrier name from abbreviated code
str(airlines) #look up airlines
  #2 airports - information about each airport as defined by the faa airport code
str(airports)
  #3) planes - infomration about each plane defined by its tailnum
str(planes)
  #4) weather - weather at each NYC airport for each hour
str(weather)

#key to understanding diagrams like this is to remember each relation always concerns a pair of tables
#nycflights13:
 #flights connects to planes via tailnum.
 #flights connects to airlines via carrier
 #flights connects to airports via origin & dest
 #flights connects to weather via origin, year, month, day, and hour

#13.2.1 Exercises ----
#1. Imagine you wanted to draw (approximately) the route each plane flies from its origin to its destination. What variables would you need? What tables would you need to combine?
 #A:In order to draw the route each plane flies you would need the airports that the planes fly over, and or GPS coordinates of the plane's flight.  In order to do this you need to get the following variables: lat, lon, alt, faa, city (from airports), carrier, name, (from airlines).
 #2. I forgot to draw the relationship between weather and airports. What is the relationship and how should it appear in the diagram?
 #A: weather connects to airport via the origin variable for weather and the faa code via airports (same variable, different name)
 #3. weather only contains information for the origin (NYC) airports. If it contained weather records for all airports in the USA, what additional relation would it define with flights?
 #3A: it would be able to match with the airports for the flights path.
 #4. We know that some days of the year are “special”, and fewer people than usual fly on them.
  #a) How might you represent that data as a data frame? What would be the primary keys of that table?In order to look this up you would need to look up the types of planes that are flown and see if there are higher nubmers of smaller planes being flown, or if there are a lower number of overall flights (or both).
  #b)How would it connect to the existing tables? It would connect via the flights and the kind of planes.

#13.3: Keys ----
#keys: variables that are used to connect each pair of tables.  A key is a variable (or set of variables) that uniquely identifies an observation.  Sometimes you can idnetify an observation by a single variable, other times you need multiple variables.
 #2 types:
  #1) primary key: uniqely identifies an observation in its own table.
  #2) foreign key: uniquely identifies an observation in another table.
 #a variable can be both a primary key and a foreign key.
 #once primary keys have been identified, good practice to verify that they do uniquely identify each observation.  One way to do it is to look at count() and look for where n>1.
planes %>%
  count(tailnum) %>%
  filter(n > 1)
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)
 #sometimes a table doesn't have an explicit primary key: each row is an observation, but no combination of variables rliably identifies it.
flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1)
flights %>%
  count(year, month, day, tailnum) %>%
  filter(n > 1)
 #surrogate key: if a table lacks a primary key, sometimes useful to add one with mutate() and row_number().  Makes it easier to match observations if you've done some filtering adn want to check back in with original data.
 #relation: primary key and the corresponding foreign key in another table.  They are typically one-to-many
flights %>%
  count(year, month, day, flight, tailnum) %>%
  filter(n > 1)

#13.3.1: Exercises ----
 #1. Add a surrogate key to flights .
flights %>%
  arrange(year, month, day, sched_dep_time, carrier, flight) %>%
  mutate(flight_id = row_number()) %>%
  glimpse()
 #2. Identify the keys in the following datasets
   #1. Lahman::Batting
    #install.packages("Lahman")
    library(Lahman)
    str(Lahman::Batting)
    view(Lahman::Batting)
    Lahman::Batting %>%
      count(playerID, yearID, stint) %>%
      filter(n > 1)
    #ANSWER: combination of playerID, yearID, stint
   #2. babynames::babynames
    #install.packages("babynames")
    library(babynames)
    str(babynames::babynames)
    view(babynames::babynames)
    babynames::babynames %>%
      count(name, sex, year) %>%
      filter(n > 1)
    #ANSWER: combination of name, sex, year
   #3. nasaweather::atmos
    #install.packages("nasaweather")
    library(nasaweather)
    str(nasaweather::atmos)
    view(nasaweather::atmos)
    nasaweather::atmos %>%
      count(lat, long, year, month) %>%
      filter(n > 1)
    #ANSWER: combination of lat, long, year, month
   #4. fueleconomy::vehicles
    #install.packages("fueleconomy")
    library(fueleconomy)
    str(fueleconomy::vehicles)
    view(fueleconomy::vehicles)
    fueleconomy::vehicles %>%
      count(id) %>%
      filter(n > 1)
   #5. ggplot2::diamonds
    str(ggplot2::diamonds)
    view(diamonds)
    diamonds %>%
      count(carat, cut, color, clarity, depth, table, price, x, y, z) %>%
      filter(n > 1)
    #ANSWER: There is no combination of variables that gives a unique ID for each row.  A surrogate is needed.
    
  #3.
   #1.Draw a diagram illustrating the connections between the Batting , Master , and Salaries tables in the Lahman package.
    view(Lahman::Batting) #has playerid, yearid, stint
    view(Lahman::Master) #has playerid
    view(Lahman::Salaries) # has playerid, yearid
    #2. Draw another diagram that shows the relationship between Master , Managers , AwardsManagers . 
    view(Lahman::Managers) #has playerid, yearid, inseason
    view(Lahman::Master) #has playerid
    view(Lahman::AwardsManagers) # has playerid, yearid, awardid 
    #3. How would you characterise the relationship between the Batting , Pitching , and Fielding tables?
    view(Lahman::Batting) #has playerid, yearid, stint
    view(Lahman::Pitching) #has playerid, yearid, stint
    view(Lahman::Fielding) # has playerid, yearid, stint
     #these tables could in theory have all the same players - the tables show the statistics for each play for batting, pitching and fielding.
  
#### 13.4 Mutating Joins ----  
#mutating join - allows you to combine variables from 2 tables.  
 #first matches observations by their keys
 #copies across variables from one table to the other.
#Join functions add variables to the right.
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

#left join:
flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")
 #this results in an dditional variable: name.  Hence, a mutating join:
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

#understanding joins: 
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
#A join is a way of connecting each row in x to 0,1,or more rows in y.
#Inner join: matches pairs of observations whenever their keys are equal.  Output is a new data frame that contains the key, x values, adn y values.  Unmatched rows are NOT included in the result.
#equijoin - keys are matched using the equality operator.  Most joins are like this.
x %>%
  inner_join(y, by = "key")

#outer join: keeps observations that appear in at least one of the tables.  There are 3 types:
 #1) left join keeps all observations in x.
 #2) right join keeps all observations in y.
 #3) full join keeps all observations in x an y.
 #work by adding an additional “virtual” observation to each table.observation has a key that always matches (if no other key matches), and a value filled with NA .
 #most commonly used join is the left join: you use this whenever you look up additional data from another table, because it preserves the original observations even when there isn’t a match.

#duplicate keys
 #occurs when keys are not unique.
#When you join duplicated keys, you get all possible combinations, the Cartesian product
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

#Defining the key columns
#so far, pairs of tables have always been joined by a seingle variable, and that variable has the same name in both tables.  That constraint was encoded by: by = "key".  Other values for by to connect the tables in other ways:
 #1) default: by=NULL, uses all variables that appear in both tables.  called the Natural join.  Ex. flights and weather tables match on their common variables: year, month, day, hour, and origin.
flights2 %>%
  left_join(weather)
 #2) A Character vector: by = "x".  Similar to a natural join, but only uses some of the common variables.  Ex. since flights and planes both have year variables, but htey mean different things, we would want to join by tailnum.
flights2 %>%
  left_join(planes, by = "tailnum")
 #3) named character vector: by = c("a" = "b").  Will match variable a in table x to variable b in table y but variables from x will be used in output. Ex.  Want to draw a map combining flights data with the airports data which contains location of each airport.  Each flight has an origin and destination airport so we need to specifiy which one we want to join to.
flights2 %>%
  left_join(planes, by = "tailnum")
flights2 %>%
  left_join(airports, c("dest" = "faa"))
flights2 %>%
  left_join(airports, c("origin" = "faa"))
flights
###13.4 Exercises ----
#1. Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap() 

avg_dest_delays <-
  flights %>%
  group_by(dest) %>%
  # arrival delay NA's are cancelled flights
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))

avg_dest_delays %>%
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#(Don’t worry if you don’t understand what semi_join() does — you’ll learn about it next.)
#You might want to use the size or colour of the points to display the average delay for each airport.

#2. Add the location of the origin and destination (i.e. the lat and lon ) to flights .
airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa")
  )


airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest")
    # existing lat and lon variables in tibble gain the _origin suffix
    # new lat and lon variables are given _dest suffix
  )

# 3. Is there a relationship between the age of a plane and its delays?
plane_cohorts <- inner_join(flights,
                            select(planes, tailnum, plane_year = year),
                            by = "tailnum"
) %>%
  mutate(age = year - plane_year) %>%
  filter(!is.na(age)) %>%
  mutate(age = if_else(age > 25, 25L, age)) %>%
  group_by(age) %>%
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    dep_delay_sd = sd(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE),
    arr_delay_sd = sd(arr_delay, na.rm = TRUE),
    n_arr_delay = sum(!is.na(arr_delay)),
    n_dep_delay = sum(!is.na(dep_delay))
  )

ggplot(plane_cohorts, aes(x = age, y = dep_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Departure Delay (minutes)")

ggplot(plane_cohorts, aes(x = age, y = arr_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of Plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Arrival Delay (minutes)")
# 4. What weather conditions make it more likely to see a delay?
flight_weather <-
  flights %>%
  inner_join(weather, by = c(
    "origin" = "origin",
    "year" = "year",
    "month" = "month",
    "day" = "day",
    "hour" = "hour"
  ))
flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()

# 5. What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.
flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()
#> Warning: Removed 3 rows containing missing values (geom_point).

##more of joins:
#base::merge() can perform all four types of mutating join:
  #dplyr merge
#inner_join(x, y) merge(x, y)
#left_join(x, y) merge(x, y, all.x = TRUE)
#right_join(x, y) merge(x, y, all.y = TRUE) ,
#full_join(x, y) merge(x, y, all.x = TRUE, all.y = TRUE)
#dplyr’s joins are considerably faster and don’t mess with the order of the rows

####13.5 Filtering Joins ----
#Filtering joins match observations in the same way as mutating joins, but affect the observations, not the variables. There are two types:
 #1) semi_join(x, y) keeps all observations in x that have a match in y .
 #2) anti_join(x, y) drops all observations in x that have a match in y .
Semi-joins are useful for matching filtered summary tables back to the original rows.

#find top 10 destinations
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

#want to find out which flights went to those destinations. use a semi-join, which connects the two tables like a mutating join, but instead of adding new columns, only keeps the rows in x that have a match in y :
flights %>%
  semi_join(top_dest)

#Only the existence of a match is important; it doesn’t matter which observation is matched. Filtering joins never duplicate rows like mutating joins do.
#anti-join: the inverse of a semi-join.  Keeps the rows that DON'T have a match.
 #anti-joins are useful for dx join mismatches. 
#ex. if connecting flights and planes, may wawnt to know how many flights don't have a match.
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)
####13.5.1 Exercises
 #1. What does it mean for a flight to have a missing tailnum ? What do the tail numbers that don’t have a matching record in planes have in common? (Hint: one variable explains ~90% of the problems.)
 #find flights with missing tailnum
flights %>%
  filter(is.na(tailnum), !is.na(arr_time)) %>%
  nrow()
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = TRUE) %>%
  mutate(p = n / sum(n))
flights %>%
  distinct(carrier, tailnum) %>%
  left_join(planes, by = "tailnum") %>%
  group_by(carrier) %>%
  summarise(
    total_planes = n(),
    not_in_planes = sum(is.na(model))
  ) %>%
  mutate(missing_pct = not_in_planes / total_planes) %>%
  arrange(desc(missing_pct))
 #2. Filter flights to only show flights with planes that have flown at least 100 flights.
planes_gte100 <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n >= 100)
flights %>%
  semi_join(planes_gte100, by = "tailnum")
flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  mutate(n = n()) %>%
  filter(n >= 100)

 #3. Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.
fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))
fueleconomy::vehicles %>%
  distinct(model, make) %>%
  group_by(model) %>%
  filter(n() > 1) %>%
  arrange(model)
fueleconomy::common %>%
  distinct(model, make) %>%
  group_by(model) %>%
  filter(n() > 1) %>%
  arrange(model)
 #4. Find the 48 hours (over the course of the whole year) that have the worst delays. Crossreference it with the weather data. Can you see any patterns?
 #1) What is meant by “delay”? I will use departure delay. Since the weather data only contains data for the New York City airports, and departure delays will be more sensitive to New York City weather conditions than arrival delays.
 #2)What is meant by “worst”? I define worst delay as the average departure delay per flight for flights scheduled to depart in that hour. For hour, I will use the scheduled departure time rather than the actual departure time. If planes are delayed due to weather conditions, the weather conditions during the scheduled time are more important than the actual departure time, at which point, the weather could have improved.
 #3)What is meant by “48 hours over the course of the year”? This could mean two days, a span of 48 contiguous hours, or 48 hours that are not necessarily contiguous hours. I will find 48 not-necessarily contiguous hours. That definition makes better use of the methods introduced in this section and chapter.
 #4) What is the unit of analysis? Although the question mentions only hours, I will use airport hours. The weather dataset has an observation for each airport for each hour. Since all the departure airports are in the vicinity of New York City, their weather should be similar, it will not be the same.
worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)
weather_most_delayed <- semi_join(weather, worst_hours,
                                  by = c(
                                    "origin", "year",
                                    "month", "day", "hour"
                                  )
)
select(weather_most_delayed, temp, wind_speed, precip) %>%
  print(n = 48)
ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
  geom_point()
 #5. What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?
#The expression anti_join(flights, airports, by = c("dest" = "faa")) returns the flights that went to an airport that is not in the FAA list of destinations. Since the FAA list only contains domestic airports, these are likely foreign flights.

#The expression anti_join(airports, flights, by = c("faa" = "dest")) returns the US airports that were not the destination of any flight in the data. Since the data contains all flights from New York City airports, this is also the list of US airports that did not have a nonstop flight from New York City in 2013
 #6. You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. Confirm or reject this hypothesis using the tools you’ve learned above.
planes_carriers <-
flights %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum, carrier)
planes_carriers %>%
  count(tailnum) %>%
  filter(n > 1) %>%
  nrow()
carrier_transfer_tbl <- planes_carriers %>%
  # keep only planes which have flown for more than one airline
  group_by(tailnum) %>%
  filter(n() > 1) %>%
  # join with airlines to get airline names
  left_join(airlines, by = "carrier") %>%
  arrange(tailnum, carrier)

####13.6 Join Problems ----
 #steps to trouble shoot
 #1) start by identifying varaible that form the primary key in each table.  Make sure they don't just uniquely identify a record but they are good identifiers.
 #2) makes ure none of the variables in the primary key are missing.  If there is a missing value, then it can't identify an observation
 #3) Check foreign keys match primary keys in another table.  Best way to do this is with an anti-join
# If you do have missing keys, you’ll need to be thoughtful about your use of inner vs. outer joins, carefully considering whether or not you want to drop rows that don’t have a match

####13.7 Set Operations ----
#All these operations work with a complete row, comparing the values of every variable. These expect the x and y inputs to have the same variables, and treat the observations like sets
 #intersect(x, y) : return only observations in both x and y
 #union(x, y) : return unique observations in x and y .
 #setdiff(x, y) : return observations in x , but not in y .
#with these data:
df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)
#there are 4 possibilities:
intersect(df1, df2)
#> # A tibble: 1 x 2
#> x y
#> <dbl> <dbl>
#> 1 1 1
# Note that we get 3 rows, not 4
union(df1, df2)
#> # A tibble: 3 x 2
#> x y
#> <dbl> <dbl>
#> 1 1 2
#> 2 2 1
#> 3 1 1
setdiff(df1, df2)
#> # A tibble: 1 x 2
#> x y
#> <dbl> <dbl>
#> 1 2 1
setdiff(df2, df1)
#> # A tibble: 1 x 2
#> x y
#> <dbl> <dbl>
#> 1 1 2