
dataset21 <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2021_DE_Region_Mobility_Report.csv", header=TRUE)
library(dplyr)

data_frame_berlin <- dataset21[dataset21$sub_region_1=="Berlin",]


data_frame_berlin<-data_frame_berlin%>%
  select(date, workplaces_percent_change_from_baseline, transit_stations_percent_change_from_baseline)
data_frame_berlin$day <- weekdays(as.Date(data_frame_berlin$date))

data_frame_berlin=filter(data_frame_berlin, day!="Saturday" & day!="Sunday")

data_frame_berlin<-data_frame_berlin%>%
  select(workplaces_percent_change_from_baseline, transit_stations_percent_change_from_baseline)

linModel <- lm(transit_stations_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = data_frame_berlin)

attributes(linModel) 

plot(transit_stations_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = data_frame_berlin,
     xlab = "workplace change from baseline",
     ylab = "transit stations change form baseline",
     main = "Correlation between workplace change and transit stations change relative to the baseline"
)


abline(linModel, col = "red", lwd = 3)

x=data_frame_berlin$workplaces_percent_change_from_baseline
y=data_frame_berlin$transit_stations_percent_change_from_baseline

cor(x,y)

text(-80, -20, paste("r:",round(cor(x,y), digits=2)))



