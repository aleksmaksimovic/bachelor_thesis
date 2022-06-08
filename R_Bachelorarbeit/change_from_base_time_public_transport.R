# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)



dataset <- read.csv("~/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/Region_Mobility_Report_CSVs/2022_DE_Region_Mobility_Report.csv", header=TRUE)

dataset21 <- read.csv("~/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/Region_Mobility_Report_CSVs/2021_DE_Region_Mobility_Report.csv", header=TRUE)
dataset20 <- read.csv("~/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/Region_Mobility_Report_CSVs/2020_DE_Region_Mobility_Report.csv", header=TRUE)


dataset$date <- as.Date(dataset$date)
dataset21$date <- as.Date(dataset21$date)
dataset20$date <- as.Date(dataset20$date)

data_frame <- rbind(dataset20, dataset21, dataset)



data_frame <- data_frame[data_frame$sub_region_1=="Bremen",]



p <- ggplot(data_frame, aes(x=date, y=transit_stations_percent_change_from_baseline )) +
  geom_area()+labs(title="Percentual  change for visits of transit stations from baseline over the years (Bremen) ")
 


p+scale_x_date(date_labels = "%b", date_breaks="1 month", name="month")+ylim(-60, 60)


#https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html

#https://www.statmethods.net/advgraphs/layout.html




