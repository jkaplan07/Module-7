##################################################################
#Julia Kaplan
#Module 7 Homework
#10/11/2019
##################################################################

####Clear workspace and load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)
library(lubridate)

#### Read in data & save as tibbles ----
border_flow      <- read_csv("data/border_flow.csv")
lake_mead_depth  <- read_csv("data/lake_mead_depth.csv")
lake_powell_flow <- read_csv("data/lake_powell_flow.csv")
lees_ferry_flow  <- read_csv("data/lees_ferry_flow.csv")

###########################################
####Working on table 2 first#########
####Border Flow ----
 #Inspect Data
  str(border_flow) #looks at structure of the data
  head(border_flow) #check top of data 
  tail(border_flow) #check bottom of data 
  dim(border_flow) #check n's
  summary(border_flow)
  
#tibble has 2 variables: date and water depth.  Date was read in as character and in year, month, format.
  
  #parse dates
  border_flow$Date <- ymd(border_flow$Date) 
  border_flow
  
  #extract date elements
  border_flow %>%
    mutate(Date = ymd(Date)) %>%
    rename(date = Date) %>% #rename Date to date so it can be joined on date with other datasets  
    mutate(location = "Border") %>% #set location to Border
    rename(flow = border_flow) -> border_flow #rename border_flow to flow
  
####Lake Powell Flow ----
  #Inspect Data
  str(lake_powell_flow) #looks at structure of the data
  head(lake_powell_flow) #check top of data 
  tail(lake_powell_flow) #check bottom of data 
  dim(lake_powell_flow) #check n's
  summary(lake_powell_flow)

#tibble has 4 variables: month, day, year, and flow.
  
  #extract date elements and set river to lake powell.
  lake_powell_flow %>%
    unite(date, year, month, day, sep = "-", remove = F) %>% #create year, month, date vector, use - as seperator.  dont remove previous columns 
    mutate(date = ymd(date)) %>% #change date to a date vector
    mutate(location = "Lake Powell") %>%
    rename(flow = lakepowell_flow) -> lake_powell_flow #set location to Lake Powell
  lake_powell_flow
    
####Lees Ferry Flow ----
  #Inspect Data
  str(lees_ferry_flow) #looks at structure of the data
  head(lees_ferry_flow) #check top of data 
  tail(lees_ferry_flow) #check bottom of data 
  dim(lees_ferry_flow) #check n's
  summary(lees_ferry_flow)

  #tibble has 2 variables: date (in month, day, year format) and leesferry_flow.  Date was read in as character.
  
  #parse dates
  lees_ferry_flow %>%
    mutate(date=mdy(date)) %>%
    mutate(location = "Lees Ferry") %>%
    rename(flow = leesferry_flow) -> lees_ferry_flow
  lees_ferry_flow  
  
#### Create Figure 2 ----
  #create dataset
   #make sure each dataset only has one record per day
    border_flow %>%
     count(Date) %>%
     filter(n >1)
    
    lake_powell_flow %>%
      count(date) %>%
      filter(n > 1)  
    
    lees_ferry_flow %>%
      count(date) %>%
      filter(n > 1)

   #combine all into one dataset: total_flow  
    lake_powell_flow %>% 
    union_all(lees_ferry_flow) %>%
    union_all(border_flow) -> total_flow
    
   #reorder flow levels so they appear correctly in the graph
    total_flow$location<-factor(total_flow$location, levels = c("Lake Powell", "Lees Ferry", "Border"))
     
  #create plot 
   total_flow %>%   
    ggplot(aes(x = date, y = flow)) +
      geom_line() +
      geom_vline (xintercept = ymd("1964-01-01"), color="red") +
     facet_wrap (~ location, ncol = 1) +
      theme_bw() +
    labs(x= "Date", y="Flow (cfs)")
     
 ####Lake Mead Depth ----
     #Inspect Data
     str(lake_mead_depth) #looks at structure of the data
     head(lake_mead_depth) #check top of data 
     tail(lake_mead_depth) #check bottom of data 
     dim(lake_mead_depth) #check n's
     summary(lake_mead_depth)
     
     #tibble has 12 variables - 1 for each month, and 79 rows, each a year.
     
     lake_mead_depth %>%
       mutate(canyon_complete = ifelse(year < 1960, "Pre Glen Canyon Dam (1936 - 1964)", "Post Glen Canyon Dam (1964 - 2014)")) %>% #create complete/incomplete dame variable
       gather('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC', key = "month", value = "depth") ->lake_mead_depth #create month rows for each year
     
    #reorder month and canyon_complete variables so they appear in the correct order
     lake_mead_depth$month <- factor(lake_mead_depth$month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
     lake_mead_depth$canyon_complete <- factor(lake_mead_depth$canyon_complete, levels = c("Pre Glen Canyon Dam (1936 - 1964)", "Post Glen Canyon Dam (1964 - 2014)"))

     lake_mead_depth %>%
       ggplot(aes(x = month, y = depth)) +
       geom_boxplot() +
       facet_grid(. ~ canyon_complete) +
         theme_bw() +
       labs(x = "MONTH", y = "Lake Mead depth (ft)")
     