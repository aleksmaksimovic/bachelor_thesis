library(dplyr)
library(tidyverse)
library(tibble)
library(tibbletime)
library(reshape2)                       

# import dataset bl_infektionen
dataset_infections_p_year <- read.csv("/Users/Aleks/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Corona Datenplattform/bl_infektionen.csv", header=TRUE)
# remove unnecessary columns
# remove unnecessary columns
dataset_infections_p_year$X_id<- NULL 
dataset_infections_p_year$ags2<- NULL 

#reduce dataset to location "Berlin" and variable "bl_inz_rate"
dataset_infections_p_year=filter(dataset_infections_p_year, bundesland == "Berlin" & variable== "bl_inz_rate")

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



p <- ggplot(dataset_infections_p_year, aes(x=date, y=dataset_infections_p_year$incidence_number )) +
  geom_line() + 
  xlab("")


p+scale_x_date(date_labels = "%b", date_breaks="1 month")











