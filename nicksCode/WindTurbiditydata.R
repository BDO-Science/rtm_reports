library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(circular)
library(xml2)
library(rvest)
library(tidyverse)
library(openair)

#this code is meant for the daily report on real time operations
#The goal is produce a quick reference on wind and turbidity to examine high winds and the increases in turbidity
#This is not predictive but is useful after a turbidity spike to see what the driving force of the increase.
#Examinations of historical data show a general pattern of winds greater than 20mph being required and usual winds
#blowing from the southwest to the south east. However exceptions to the direction have been observed.

########################################
###############
#######

#selects todays date using lubridate function today
theDATE<- today()

###############
#This pulls the wind direction in degrees from cdec
#this set up the same way as above but I am useing "theDATE" object to set the time interval so it changes when ever I run the code.
wd1<- cdec_query("FRK", "10", "E", theDATE-14, theDATE)
View(wd1)

wd2 <- wd1 %>% rename(wind.dir.deg = parameter_value) %>% select(datetime, location_id, wind.dir.deg)
#View(wd2)
wd2$datetime <- as_datetime(wd2$datetime)

#######################
#This pulls the wind speed in mph from cdec

ws1<- cdec_query("FRK", "9", "E", theDATE-14, theDATE)
#View(ws1)
ws2 <- ws1 %>% rename(wind.spd.mph = parameter_value) %>% select(datetime, location_id, wind.spd.mph)
#View(ws2)
ws2$datetime <- as_datetime(ws2$datetime)

#####################################
#queries cdec for the turbidity at OBI event data
ot1<- cdec_query("OBI", "221", "E", theDATE-14, theDATE)
#View(ot1)

ot2 <- ot1 %>% rename(turb.NTU = parameter_value)%>% select(datetime, location_id, turb.NTU)
#View(ot2)
ot2$datetime <- as_datetime(ot2$datetime)

###########################
#queries cdec for the turbidity at franks tract event data
ft1<- cdec_query("FRK", "27", "E", theDATE-14, theDATE)
#View(ft1)

ft2 <- ft1 %>% rename(turb.NTU = parameter_value) %>% select(datetime, location_id, turb.NTU)
#View(ft2)
ft2$datetime <- as_datetime(ft2$datetime)

#################################
#######
#puts all turbidity data into a single table

tu1 <- bind_rows(ft2,ot2)
#View(tu1)
a1<-left_join(wd2, ws2, by ="datetime") 
#View(a1)
a2 <- left_join(a1, ft2, by= "datetime", "location_id") %>% select(datetime, location_id, wind.spd.mph, wind.dir.deg)
#View(a2)
a3 <- inner_join(a2, tu1, by= "datetime", suffix=c(".wind",".turb"))
#View(a3)
wd.a4 <- a3 %>%group_by(date=date(datetime)) %>% summarise(dir.deg.daily=mean(wind.dir.deg))
#View(wd.a4)
ws.a4 <- a3 %>%group_by(date=date(datetime)) %>% summarise(spd.mph..daily=mean(wind.spd.mph))
#View(ws.a4)
tu.a4 <- a3 %>%  group_by(location_id.turb, date=date(datetime)) %>% summarise(turb.NTU.daily=mean(turb.NTU))
#view(tu.a4)
tu.a5 <- left_join(tu.a4,wd.a4, by ="date")
#View(tu.a5)
tu.a6 <- left_join(tu.a5,ws.a4, by ="date")
#View(tu.a6)    

###################################
############
############################################################
#This section of code assigns a direction based on the degree value (ie. N= North=0 degrees)
wsd1 <- left_join(wd2, ws2, by= "datetime") %>%
  select(datetime, wind.spd.mph, wind.dir.deg)
#View(wsd1)

#Pulls table of the directions and thier intervals off the website and uses it to categorize the degrees as cardinal directions
url <- 'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- read_html(url)
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions_raw %>% 
  set_names(~tolower(sub(' Direction', '', .x))) %>% 
  slice(-1) %>% 
  separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)
#View(directions)


wsd2 <- wsd1 %>% 
  mutate(wd_cardinal = cut(
    wind.dir.deg, breaks = c(0, directions$degree_max, 360), labels = c(directions$cardinal, 'N')))

#View(wsd2)
##########################
############
#reformats date to go into wind rose    
wsd2 <- wsd2 %>% mutate(date = date(datetime))

##############################
#########
####
#this windrose plots the frequency counts of wind speed for each day of the date series pulled from CDEC

windRose(wsd2, 
         ws= "wind.spd.mph", 
         wd = "wind.dir.deg", type = "date",  angle = 10,
         cols = c("tan", "light blue", "dark blue","dark green", "orange"), 
         grid.line = 5,
         breaks =  c(0, 10, 20, 30,40,50),
         paddle = FALSE,
         key.header = "This is in MPH not Meters/second") 
####################################
#########
###
#this windrose does a single windrose for the whole date series. 
windRose(wsd2, 
         ws= "wind.spd.mph", 
         wd = "wind.dir.deg", 
         angle = 10,
         cols = c("tan", "light blue", "dark blue","dark green", "orange"), 
         grid.line = 5,
         breaks =  c(0, 10, 20, 30,40,50),
         paddle = FALSE,
         key.header = "This is in MPH not Meters/second") 



