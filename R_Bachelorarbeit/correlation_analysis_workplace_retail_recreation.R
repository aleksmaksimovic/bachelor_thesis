dataset <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2022_DE_Region_Mobility_Report_22_03_06.csv", header=TRUE)

dataset21 <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2021_DE_Region_Mobility_Report.csv", header=TRUE)
dataset20 <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2020_DE_Region_Mobility_Report.csv", header=TRUE)


dataset$date <- as.Date(dataset$date)
dataset21$date <- as.Date(dataset21$date)
dataset20$date <- as.Date(dataset20$date)

total_data <- rbind(dataset, dataset20, dataset21)

library(dplyr)

data_frame_berlin <- total_data[dataset21$sub_region_1=="Berlin",]


data_frame_berlin<-data_frame_berlin%>%
  select(date, workplaces_percent_change_from_baseline, retail_and_recreation_percent_change_from_baseline)
data_frame_berlin$day <- weekdays(as.Date(data_frame_berlin$date))

data_frame_berlin=filter(data_frame_berlin, day!="Saturday" & day!="Sunday")

data_frame_berlin<-data_frame_berlin%>%
  select(workplaces_percent_change_from_baseline, retail_and_recreation_percent_change_from_baseline)

linModel <- lm(retail_and_recreation_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = data_frame_berlin)

attributes(linModel) 

plot(retail_and_recreation_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = data_frame_berlin,
     xlab = "workplace change from baseline",
     ylab = "retail and recreation change from baseline",
     main = "Correlation between workplace change and retail change relative to the baseline"
)


abline(linModel, col = "red", lwd = 3)

x=data_frame_berlin$workplaces_percent_change_from_baseline
y=data_frame_berlin$retail_and_recreation_percent_change_from_baseline

cor(x,y)

text(-80, -20, paste("r:",round(cor(x,y), digits=2)))



