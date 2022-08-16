library(tidyverse)
library(readr)
#############################################
#example code for date filtering
#set start date and end date
#st.date <- c("2022-05-15")
#end.date <- c("2022-05-20")

#wk.data <- sal.salmon %>% 
 # filter(SampleDate >= as.Date(st.date) & SampleDate <= as.Date(end.date)) %>% group_by(SPCODE)
####################################################


sal.salmon <- read_csv("Salvage/Salmon_2022_05242022.csv")
View(sal.salmon)
lossbygroup<- sal.salmon %>%  group_by(ADCLIP, `Size Race`) %>% summarise(sum(LOSS))

View(lossbygroup)
   


#####################################
library(readr)
sal.steelhd <- read_csv("Salvage/Steelhead_Salvage_Summary_2022_rawdata_06082022.csv", 
                                                            col_types = cols(Date = col_date(format = "%m/%d/%Y")))
#View(sal.steelhd)   
#############################
#separates sal.steelhead the data based on the two different counting periods used for Steelhead. 
sal.steelhd.1 <- sal.steelhd %>% 
  filter(Date >= as.Date("2021-08-01") & Date <= as.Date("2022-03-31"))

sal.steelhd.2 <- sal.steelhd %>% 
  filter(Date >= as.Date("2022-04-01") & Date <= as.Date("2022-06-15"))


SH.loss.1 <- sal.steelhd.1 %>% 
  summarise(SWP.Total = sum(Loss_SWPN,na.rm = TRUE),CVP.Total = sum(Loss_CVPN,na.rm = TRUE)) %>% 
  mutate(Total= SWP.Total+CVP.Total)
view(SH.loss.1)

SH.loss.2 <- sal.steelhd.2 %>% 
  summarise(SWP.Total = sum(Loss_SWPN,na.rm = TRUE),CVP.Total = sum(Loss_CVPN,na.rm = TRUE)) %>%
  mutate(Total= SWP.Total+CVP.Total)
view(SH.loss.2)

#Cumulative total clipped steal head loss. This is for the whole season and does not separate by seasonal timing. 
cumulative.clipped.SH <- sal.steelhd  %>% 
  summarise(SWP.Total = sum(Loss_SWPC,na.rm = TRUE),CVP.Total = sum(Loss_CVPC,na.rm = TRUE)) %>%
  mutate(Total= SWP.Total+CVP.Total)
view(cumulative.clipped.SH)

