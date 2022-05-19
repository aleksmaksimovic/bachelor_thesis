library(dplyr)
library(tidyverse)
library(tibble)
library(tibbletime)
library(reshape2) 
library(ggplot2)
library(lubridate)

# import dataset bl_infektionen
dataset_infections_p_year <- read.csv("/Users/Aleks/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Corona Datenplattform/bl_infektionen.csv", header=TRUE)
# remove unnecessary columns





dataset_infections_p_year$X_id<- NULL 
dataset_infections_p_year$ags2<- NULL 

#reduce dataset to location "Saarland" and variable "bl_inz_rate"
dataset_infections_p_year=filter(dataset_infections_p_year, bundesland == "Saarland" & variable== "bl_inz_rate")

# remove unnecessary columns
dataset_infections_p_year$bundesland<- NULL 
dataset_infections_p_year$variable<- NULL 

# Transpose dataset and rename columns
dataset_infections_p_year=as.data.frame(t(dataset_infections_p_year))
dataset_infections_p_year <- tibble::rownames_to_column(dataset_infections_p_year, "date")
names(dataset_infections_p_year)[2] <- "incidence_number"

# Parse date and convert column to date format
dataset_infections_p_year=dataset_infections_p_year %>%
  mutate(date = substr(date, 2, 9))
dataset_infections_p_year=dataset_infections_p_year %>%
  mutate(dataset_infections_p_year, date = as.POSIXct(date, format = "%Y%m%d"))
dataset_infections_p_year$date <- as.Date(dataset_infections_p_year$date)

# Convert dataset_measure_subcategory_p_year to tbl_time in order to filter by year
dataset_infections_p_year <- as_tbl_time(dataset_infections_p_year, index = date)

dataset_infections_p_year=filter_time(dataset_infections_p_year, ~'2021')


dataset_infections_p_year=filter(dataset_infections_p_year, date >= as.Date("2021-01-01"), date <= as.Date("2021-05-31"))

p <- ggplot(dataset_infections_p_year, aes(x=date, y=dataset_infections_p_year$incidence_number )) +
  geom_line() + 
  xlab("")


p+scale_x_date(date_labels = "%b", date_breaks="1 month")



#import workplaces_change data


dataset21 <- read.csv("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/2021_DE_Region_Mobility_Report.csv", header=TRUE)


dataset21$date <- as.Date(dataset21$date)
data_frame_workplaces_change_Saarland <- dataset21[dataset21$sub_region_1=="Saarland",]

#Merge dataframes dataset21 and dataset_infections_p_year
dataset_merged=merge(x=data_frame_workplaces_change_Saarland, y=dataset_infections_p_year,by="date")


#Consider colder months only to prohibit distortion (vacation)
dataset_merged=filter(dataset_merged, date >= as.Date("2021-01-01"), date <= as.Date("2021-05-31"))


#Simple linear correlation

linModel <- lm(workplaces_percent_change_from_baseline~incidence_number, data = dataset_merged)




attributes(linModel) 

plot(workplaces_percent_change_from_baseline~incidence_number, data = dataset_merged,
     ylab = "workplace change from baseline",
     xlab = "incidence number",
     main = "Correlation between workplace change and incidence number change"
)


abline(linModel, col = "red", lwd = 3)

y=dataset_merged$workplaces_percent_change_from_baseline
x=dataset_merged$incidence_number

cor(x,y)

text(0, 25, paste("r:",round(cor(x,y), digits=2)))






