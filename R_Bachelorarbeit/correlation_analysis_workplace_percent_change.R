
dataset21 <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2021_DE_Region_Mobility_Report.csv", header=TRUE)
library(dplyr)

data_frame_berlin <- dataset21[dataset21$sub_region_1=="Berlin",]


data_frame_berlin<-data_frame_berlin%>%
  select(residential_percent_change_from_baseline, workplaces_percent_change_from_baseline)



linModel <- lm(residential_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = data_frame_berlin)

attributes(linModel) 

plot(residential_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = data_frame_berlin,
     xlab = "workplace change from baseline",
     ylab = "residential change form baseline",
     main = "Correlation between workplace change and residential change relative to the baseline"
)


abline(linModel, col = "red", lwd = 3)

x=data_frame_berlin$workplaces_percent_change_from_baseline
y=data_frame_berlin$residential_percent_change_from_baseline

cor(x,y)

text(0, 25, paste("r:",round(cor(x,y), digits=2)))



