dataset <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2022_DE_Region_Mobility_Report_22_03_06.csv", header=TRUE)

dataset21 <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2021_DE_Region_Mobility_Report.csv", header=TRUE)
dataset20 <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2020_DE_Region_Mobility_Report.csv", header=TRUE)


dataset$date <- as.Date(dataset$date)
dataset21$date <- as.Date(dataset21$date)
dataset20$date <- as.Date(dataset20$date)

total_data <- rbind(dataset, dataset20, dataset21)

library(dplyr)

dataset21 <- dataset21[dataset21$sub_region_1=="Saarland",]

#only consider timespans where people are usually on holidays to prevent distortion

dataset21=filter(dataset21, (date >= as.Date("2021-01-01") & date <= as.Date("2021-05-31")) | (date >= as.Date("2021-9-30") & date <= as.Date("2021-12-31")) )




dataset21<-dataset21%>%
  select(date, workplaces_percent_change_from_baseline, retail_and_recreation_percent_change_from_baseline)
dataset21$day <- weekdays(as.Date(dataset21$date))

dataset21=filter(dataset21, day!="Saturday" & day!="Sunday")

dataset21<-dataset21%>%
  select(workplaces_percent_change_from_baseline, retail_and_recreation_percent_change_from_baseline)

linModel <- lm(retail_and_recreation_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = dataset21)

attributes(linModel) 

plot(retail_and_recreation_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = dataset21,
     xlab = "workplace change from baseline",
     ylab = "retail_and_recreation_percent_change_from_baseline",
     main = "Correlation between workplace and retail_and_recreation_percent_change_from_baseline relative to the baseline"
)


abline(linModel, col = "red", lwd = 3)

x=dataset21$workplaces_percent_change_from_baseline
y=dataset21$retail_and_recreation_percent_change_from_baseline

cor(x,y)

text(-80, -30, paste("r:",round(cor(x,y), digits=2)))



