library(CDECRetrieve)
library(readxl)
library(lubridate)

##############################################################################

#The code on this page is set up to draw on a list of cdec stations and their sensors to be draw down a whole group at once rather than querying each one indivdually. 

SensorsList <- read_excel("SensorsList.xlsx")
View(SensorsList)

#creates an object of todays date
theDATE<- today()

#################################################

#creates empty data frame to be filled by the 
temp.dat <-data.frame(agency_cd = NA, location_id = NA, datetime = as_datetime("1900-05-24 00:00:00"), parameter_cd = NA, parameter_value = NA, parameter_des = NA) 

#for loop that pulls from Sensorslist excel file  to fill in parts of the cdecquery
#within the loop, the parts of the cdec query had to be name matched to the names of the arguments listed in the help page 
for (i in 1:nrow(SensorsList)) {
  ##cdecquery set up to accept the variables created by the loop
  dat <- as.data.frame(cdec_query(station = SensorsList[i,"station.ID"], sensor_num = as.factor(SensorsList[i,"sensor.num"]), dur_code = SensorsList[i,"duration"],theDATE-5, theDATE-1))
  #fills in the column with the description for all parameters
  dat$parameter_des <-rep(SensorsList[i,"description"])
  #browser commmented out lets you cycle through each iteration and see the outcome. Look at help option for commmands
  #browser()
  #row binds each new iteration to the dataframe.
  temp.dat <- rbind(temp.dat,dat)
  
  
}
#first line will need to be removed from dataframe
View(temp.dat)

#example of standard cdec format.
cdec_query("OBI", "221", "E", "2020-05-01", "2020-05-25")

