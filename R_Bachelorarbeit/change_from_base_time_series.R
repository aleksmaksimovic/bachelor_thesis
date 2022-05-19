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

#total_data <- rbind(dataset, dataset20, dataset21)



data_frame_Berlin <- dataset20[dataset20$sub_region_1=="Hamburg",]

#total_data_Nordrhein-Westfalen <- total_data[total_data$sub_region_1=="Berlin",]



p <- ggplot(data_frame_Berlin, aes(x=date, y=data_frame_Berlin$residential_percent_change_from_baseline )) +
  geom_area()
 


p+scale_x_date(date_labels = "%b", date_breaks="1 month")+ylim(-50, 50)


#https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html

#https://www.statmethods.net/advgraphs/layout.html



