###Chapter 15: Factors

#factors are used to work with categorical variables - ones with a fixed and known set of possible values. Historically, factors were easier to work with than charaters.  As such, many base R functions automatically convert characters to factors.  In the tidyverse you don't need to as much, but they are still helpful sometimes.

library(tidyverse)
#install.packages("forcats")
library(forcats)

#forcasts package provides range of helpers to help working with categorical variables.  Also helps when working with factors.

##Creating factors ----
#you have a variable that records month:
x1 <- c("Dec", "Apr", "Jan", "Mar")
#using a string to record this variable has 2 problems:
 #1) there are only 12 possible months, but there is nthign that prevents you from typos.
x2 <- c("Dec", "Apr", "Jam", "Mar")
#2) doesn't sort in a useful way
sort(x2)
#> [1] "Apr" "Dec" "Jan" "Mar"

#both of these problems can be fixed with factors.
#to create a factor you must start by creating a list of valid levels:
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
#now you can create a factor:
y1 <- factor(x1, levels = month_levels)
y1
#> [1] Dec Apr Jan Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
sort(y1)
#> [1] Jan Mar Apr Dec
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

#any values not in the set will be converted to NA.
y2 <- factor(x2, levels = month_levels)
y2
#> [1] Dec Apr <NA> Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

#if you want a warning you can use readr::parse_factor():
y2 <- parse_factor(x2, levels = month_levels)
#> Warning: 1 parsing failure.
#> row col expected actual
#> 3 -- value in level set Jam

#if you omit the levels they will be taken from the data in alphabetical order:
factor(x1)
#> [1] Dec Apr Jan Mar
#> Levels: Apr Dec Jan Mar

#if you want the order of the levels to match the order they first appear in the data you can set the levels to unique(x), or after the fact with fct_inorder()
#ever need to access the set of valid levels directly, you can do so with levels() :
  y2 <- factor(x2, levels = month_levels)
y2
#> [1] Dec Apr <NA> Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
y2 <- parse_factor(x2, levels = month_levels)
#> Warning: 1 parsing failure.
#> row col expected actual
#> 3 -- value in level set Jam
factor(x1)
#> [1] Dec Apr Jan Mar
#> Levels: Apr Dec Jan Mar
f1 <- factor(x1, levels = unique(x1))
f1
#> [1] Dec Apr Jan Mar
#> Levels: Dec Apr Jan Mar
f2 <- x1 %>% 
  factor() %>% 
  fct_inorder() 
f2
#> [1] Dec Apr Jan Mar
#> Levels: Dec Apr Jan Mar
#if you ever need access to set of valid levels directly, use levels()

levels(f2)
#> [1] "Dec" "Apr" "Jan" "Mar"

###15.3 General Social Survey ----
gss_cat #general social survey - us survey by NORC at UChicago

#count():allows you to see factors that are stored in a tibble
gss_cat %>%
  count(race)
#bar chart also lets you see the different values.
ggplot(gss_cat, aes(race)) +
  geom_bar()

#by default, ggplot2 does not display values for levels.  You can display them by doing this:
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

####15.3.1 Exercise ----
#1. Explore the distribution of rincome (reported income). What makes the default bar chart hard to understand? How could you improve the plot?
rincome_plot <-
  gss_cat %>%
  ggplot(aes(x = rincome)) +
  geom_bar()
rincome_plot
#hard to read.  

#attempt 2
rincome_plot +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#attempt 3:
rincome_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#attempt 4:
rincome_plot +
  coord_flip()

#best version:
gss_cat %>%
  filter(!rincome %in% c("Not applicable")) %>%
  mutate(rincome = fct_recode(rincome,
                              "Less than $1000" = "Lt $1000"
  )) %>%
  mutate(rincome_na = rincome %in% c("Refused", "Don't know", "No answer")) %>%
  ggplot(aes(x = rincome, fill = rincome_na)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous("Number of Respondents", labels = scales::comma) +
  scale_x_discrete("Respondent's Income") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
  theme(legend.position = "None")

#2. What is the most common relig in this survey? What’s the most common partyid ?
gss_cat %>%
  count(relig) %>%
  arrange(desc(n)) %>%
  head(1)
#> # A tibble: 1 x 2
#>   relig          n
#>   <fct>      <int>
#> 1 Protestant 10846

gss_cat %>%
  count(partyid) %>%
  arrange(desc(n)) %>%
  head(1)
#> # A tibble: 1 x 2
#>   partyid         n
#>   <fct>       <int>
#> 1 Independent  4119

#3. 
 #a. Which relig does denom (denomination) apply to? How can you find out with a table?
levels(gss_cat$denom)
#>  [1] "No answer"            "Don't know"           "No denomination"     
#>  [4] "Other"                "Episcopal"            "Presbyterian-dk wh"  
#>  [7] "Presbyterian, merged" "Other presbyterian"   "United pres ch in us"
#> [10] "Presbyterian c in us" "Lutheran-dk which"    "Evangelical luth"    
#> [13] "Other lutheran"       "Wi evan luth synod"   "Lutheran-mo synod"   
#> [16] "Luth ch in america"   "Am lutheran"          "Methodist-dk which"  
#> [19] "Other methodist"      "United methodist"     "Afr meth ep zion"    
#> [22] "Afr meth episcopal"   "Baptist-dk which"     "Other baptists"      
#> [25] "Southern baptist"     "Nat bapt conv usa"    "Nat bapt conv of am" 
#> [28] "Am bapt ch in usa"    "Am baptist asso"      "Not applicable"


gss_cat %>%
  filter(!denom %in% c(
    "No answer", "Other", "Don't know", "Not applicable",
    "No denomination"
  )) %>%
  count(relig)

 #b. How can you find out with a visualisation?
gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

#### Modifying Factor Order ----
#sometimes you want to change the order of the factor levels in a visualistion.  Ex.  Want to explore average number of hours spent watching tv per day across relgions:

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

#difficult to interpret plot becuase there is no overall paturn.  You can improve plot by reordering levels of relig using fct_reorder().  
#fct_reorder() takes 3 arguments: f: factor whose levels you want to modify, x: number vector that you want to use to reorder levels.  fun: (optional) used if there are multiple levels of x for each value of f.  default value is median.
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

#you can do fct_reorder in a mutate() function instead of through aes().
relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

#Similar plot showign how avg age varies across reported income level
rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()

#Reserve fct_reorder() for factors whose levels are arbitrarily ordered

#fct_relevel(): takes factor f and then any number of levels you want to move to front of the line.

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

#coloring lines in a plot: fct_reorder2() reorders factor by the y values associated with the largest x values.  makes plot easier to read becuase colors line up with the legend.

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))
ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

#for bart plots: can use fct_infreq() to order levels in increasing frequency.  simiplest type of reordering because it does nto require extra variables.  can comine with fct_rev()
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

####15.4.1 Exercises ----
#1. There are some suspiciously high numbers in tvhours . Is the mean a good summary?
summary(gss_cat[["tvhours"]])
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>       0       1       2       3       4      24   10146

gss_cat %>%
  filter(!is.na(tvhours)) %>%
  ggplot(aes(x = tvhours)) +
  geom_histogram(binwidth = 1)
#2. For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
keep(gss_cat, is.factor) %>% names()
#> [1] "marital" "race"    "rincome" "partyid" "relig"   "denom"
levels(gss_cat[["marital"]])
gss_cat %>%
  ggplot(aes(x = marital)) +
  geom_bar()
levels(gss_cat$race)
#> [1] "Other"          "Black"          "White"          "Not applicable"

gss_cat %>%
  ggplot(aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

levels(gss_cat$rincome)
#>  [1] "No answer"      "Don't know"     "Refused"        "$25000 or more"
#>  [5] "$20000 - 24999" "$15000 - 19999" "$10000 - 14999" "$8000 to 9999" 
#>  [9] "$7000 to 7999"  "$6000 to 6999"  "$5000 to 5999"  "$4000 to 4999" 
#> [13] "$3000 to 3999"  "$1000 to 2999"  "Lt $1000"       "Not applicable"

levels(gss_cat$relig)
#>  [1] "No answer"               "Don't know"             
#>  [3] "Inter-nondenominational" "Native american"        
#>  [5] "Christian"               "Orthodox-christian"     
#>  [7] "Moslem/islam"            "Other eastern"          
#>  [9] "Hinduism"                "Buddhism"               
#> [11] "Other"                   "None"                   
#> [13] "Jewish"                  "Catholic"               
#> [15] "Protestant"              "Not applicable"

gss_cat %>%
  ggplot(aes(relig)) +
  geom_bar() +
  coord_flip()

levels(gss_cat$denom)

levels(gss_cat$partyid)

#3. Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?
 #Because that gives the level “Not applicable” an integer value of 1.

##### Modifying factor levels ----
#changing levels values allows you to clarify labels for publication and collapse levels for high-level displays.
#fct_record: allows you to recorde, or change hte value of each level.
gss_cat %>% count(partyid) #levles are terse and inconsistent.  
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat"
  )) %>%
  count(partyid)

#fct_recode() will leave levels that aren’t explicitly mentioned as is, and will warn you if you accidentally refer to a level that doesn’t exist.

#To combine groups, you can assign multiple old levels to the same new level

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat",
                              "Other" = "No answer",
                              "Other" = "Don't know",
                              "Other" = "Other party"
  )) %>%
  count(partyid)
#you have to use this technique with care - if you group together categories that are truly different you will end up with misleading results.

#If you want to collapse a lot of levels, fct_collapse() is a useful variant of fct_recode() .

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

#fct_lump - lump toether all the small groups to make a plot or table simpler.  n parameter specifies how many groups you want to keep.

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)


#####15.5.1 Exercises ----
#1. How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?
levels(gss_cat$partyid)
#>  [1] "No answer"          "Don't know"         "Other party"       
#>  [4] "Strong republican"  "Not str republican" "Ind,near rep"      
#>  [7] "Independent"        "Ind,near dem"       "Not str democrat"  
#> [10] "Strong democrat"
gss_cat %>%
  mutate(
    partyid =
      fct_collapse(partyid,
                   other = c("No answer", "Don't know", "Other party"),
                   rep = c("Strong republican", "Not str republican"),
                   ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                   dem = c("Not str democrat", "Strong democrat")
      )
  ) %>%
  count(year, partyid) %>%
  group_by(year) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(
    x = year, y = p,
    colour = fct_reorder2(partyid, year, p)
  )) +
  geom_point() +
  geom_line() +
  labs(colour = "Party ID.")

#2. How could you collapse rincome into a small set of categories?
levels(gss_cat$rincome)
#>  [1] "No answer"      "Don't know"     "Refused"        "$25000 or more"
#>  [5] "$20000 - 24999" "$15000 - 19999" "$10000 - 14999" "$8000 to 9999" 
#>  [9] "$7000 to 7999"  "$6000 to 6999"  "$5000 to 5999"  "$4000 to 4999" 
#> [13] "$3000 to 3999"  "$1000 to 2999"  "Lt $1000"       "Not applicable"

library("stringr")
gss_cat %>%
  mutate(
    rincome =
      fct_collapse(
        rincome,
        `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
        `Lt $5000` = c("Lt $1000", str_c(
          "$", c("1000", "3000", "4000"),
          " to ", c("2999", "3999", "4999")
        )),
        `$5000 to 10000` = str_c(
          "$", c("5000", "6000", "7000", "8000"),
          " to ", c("5999", "6999", "7999", "9999")
        )
      )
  ) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()